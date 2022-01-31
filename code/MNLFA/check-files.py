import argparse
import os
import sys
import glob as g
import pprint
import re

import pandas
import pandas as pd
from functools import reduce

pp = pprint.PrettyPrinter(indent=2)

###############################################################################

parser = argparse.ArgumentParser()

parser.add_argument("dir", help="Directory to check")

args = parser.parse_args()

the_dir = args.dir
assert os.path.isdir(the_dir), f"Supplied directory '{the_dir}' does not exist"

###############################################################################


def read_new_addtl_param(file_text, var=None):
    result = []
    appending = False
    for line in file_text:

        if line.strip() == "New/Additional Parameters":
            appending = True
        elif appending and line == "\n":
            break

        if appending:
            result.append(line.strip())

    result2 = result[2:]
    result3 = [x.split() for x in result2]
    result4 = pd.DataFrame(data=result3, columns=("eta", "est", "se", "est_se", "pval"))
    result4[["est", "se", "est_se", "pval"]] = result4[["est", "se", "est_se", "pval"]].apply(pd.to_numeric)
    result4["sig"] = result4["pval"] < .05

    # Split eta into two
    result4[["item_lambda", "DIF"]] = result4.eta.str.split("_", expand=True)

    result4.insert(0, "var", var)

    return result4


def read_model(file_text):

    result = []
    appending = False
    for line in file_text:

        if re.match("[+]l[0-9]*_[0-9]*[*][A-Z_]*", line.strip()):
            appending = True
        elif appending and line.strip() == ";":
            break

        if appending:
            result.append(line.strip())

    result2 = [x.split("*") for x in result]
    result3 = pd.DataFrame(data=result2, columns=("eta", "label"))
    # Remove '+' and upper case eta label
    result3["eta"] = [x.replace("+", "").upper() for x in result3["eta"]]

    return result3


def read_int_dif(file_text, var=None):
    result = []
    appending = False
    for line in file_text:

        if re.match(f"{var}\s*ON", line.strip()):
            appending = True
        elif appending and line == "\n":
            break

        if appending:
            result.append(line.strip())

    result2 = result[1:]
    result3 = pd.DataFrame(data=[x.split() for x in result2], columns=("label", "int_est", "int_se", "int_est_se",
                                                                       "int_pval"))

    intercept_cols = ["int_est", "int_se", "int_est_se", "int_pval"]
    result3[intercept_cols] = result3[intercept_cols].apply(pd.to_numeric)
    result3["int_sig"] = result3["int_pval"] < .05

    result3.insert(0, "var", var)

    return result3


def extract_eq(text, lh):

    match = [re.sub("[ ;]", "", x.strip()) for x in text if re.match(f" *{lh}=[0-9l].*", x)]

    if len(match) == 1:
        return match[0]
    elif len(match) == 0:
        return None
    else:
        sys.exit(f"Somehow found two equations: {match}")


def extract_est_int_dif(r2c_text):

    # Split items into df that needs to be key-matched with full frame
    lines_to_chk = pandas.DataFrame([x.split(" ", 2) for x in r2c_text if re.match("^[A-Z_]* [Oo][Nn] [A-Z_ ]*;$", x)],
                                    columns=("var", "on", "items"))

    # Extract estimated intercept DIF
    lines_to_chk["est_int_dif"] = [re.sub(";\n", "", x).split() for x in lines_to_chk["items"]]

    # Remove extraneous columns
    del lines_to_chk["on"]
    del lines_to_chk["items"]

    # print(lines_to_chk)
    return lines_to_chk


def extract_est_int_dif2(r2c_text):

    # Split items into df that needs to be key-matched with full frame
    lines_to_chk = [re.sub(";\n", "", x) for x in r2c_text if re.match("^[A-Z_]* [Oo][Nn] [A-Z0-9@._ ]*;$", x)]
    var = [x.split()[0] for x in lines_to_chk]

    df = pd.DataFrame({'var': var, 'est_int_dif': lines_to_chk})
    return df


def union_of_dif(x, y):

    if pd.isna(x):
        x = []
    else:
        x = x.split(",")

    if pd.isna(y):
        y = []
    else:
        y = y.split(",")

    u = list(set().union(x, y))
    return u

###############################################################################


def main():

    ###################################################################################################################
    # Stage 1
    ###################################################################################################################

    measinvar_out = g.glob(os.path.join(the_dir, "measinvarscript_*.out"))
    measinvar_names = [re.search("measinvarscript_(.*).out", x).groups(1)[0].upper() for x in measinvar_out]
    print("Vars: %s" % ", ".join(measinvar_names))

    r2c = os.path.join(the_dir, "round2calibration.inp")
    if os.path.exists(r2c):
        print("Stage 1 ...")
    else:
        print("round2calibration.inp does not exist; run aMNLFA_simultaneous()")
        exit(1)

    mio_text = [None] * len(measinvar_out)
    for i, the_file in enumerate(measinvar_out):
        with open(the_file, "r") as f:
            mio_text[i] = f.readlines()

    # Extract bits as tables from .out script
    # New additional parameters
    mio_nap = [read_new_addtl_param(x, y) for x, y in zip(mio_text, measinvar_names)]
    # Model specification
    mio_model = [read_model(x) for x in mio_text]
    # Intercept DIF
    mio_int_dif = [read_int_dif(x, y) for x, y in zip(mio_text, measinvar_names)]

    # Merge vars with model info
    mio_vars = [x.merge(y, on="eta", how="left") for x, y in zip(mio_nap, mio_model)]
    # ... then add in intercept DIF
    mio_vars_int = [x.merge(y, on=["var", "label"], how="left") for x, y in zip(mio_vars, mio_int_dif)]

    # Reduce to one DF
    mio_vars_int1 = reduce(lambda x, y: pd.concat([x, y]), mio_vars_int)
    mio_vars_int1 = mio_vars_int1.reset_index(drop=True)

    # Extract rows

    lambda_dif = mio_vars_int1.loc[mio_vars_int1["sig"], ["var", "item_lambda", "label"]]
    lambda_dif2 = lambda_dif.groupby(["var", "item_lambda"]).agg(lambda_dif=("label", lambda z: ','.join(z))).reset_index()

    int_dif = mio_vars_int1.loc[mio_vars_int1["int_sig"], ["var", "item_lambda", "label"]]
    int_dif2 = int_dif.groupby(by=["var", "item_lambda"]).agg(intercept_dif=("label", lambda z: ','.join(z))).reset_index()

    dif_table = lambda_dif2.merge(int_dif2, on=["var", "item_lambda"], how="outer")
    dif_table.columns = ["var", "item_lambda", "lambda_dif", "intercept_dif"]
    dif_table["eq"] = [x.replace("L", "l_") for x in dif_table["item_lambda"]]

    # Now get formulae
    with open(r2c, "r") as f:
        r2c_text = f.readlines()

    # Extract the equation from round2calib inp
    dif_table["equation"] = [extract_eq(r2c_text, x) for x in dif_table["eq"]]

    # print(dif_table)

    # Extract the estimated items from r2c inp (what was actually estimated in later steps) and merge by key into the
    # big table (because they might be in a different order)
    est_int_dif_table = extract_est_int_dif(r2c_text)
    dif_table = dif_table.merge(est_int_dif_table)

    # This is what estimated int dif _should_ be
    dif_table["est_int_dif_shouldbe"] = [union_of_dif(x, y) for x, y in
                                         zip(dif_table["lambda_dif"], dif_table["intercept_dif"])]

    # Check that the lists are the same, and then make them pretty to print
    dif_table["est_int_dif_OK"] = [", ".join(x) + " OK" if sorted(x) == sorted(y) else "PROBLEM" for
                                   x, y in zip(dif_table["est_int_dif"], dif_table["est_int_dif_shouldbe"])]

    # Create empty column so shape matches - maybe do something with this later
    dif_table["sig_dif_num_lab"] = ""

    dif_table_reorder = dif_table[["var", "sig_dif_num_lab", "lambda_dif", "item_lambda", "equation", "intercept_dif",
                                   "est_int_dif_OK"]]

    # print(dif_table_reorder)
    dif_table_reorder.to_csv(f"{the_dir}-dif_table.csv", index=False)
    print(f"Stage 1 checking file {the_dir}-dif_table.csv saved")

    master_order = pd.DataFrame({"var": dif_table_reorder["var"]})

    ###################################################################################################################
    # Stage 2
    ###################################################################################################################

    lambda_dif_final_file = os.path.join(the_dir, "lambda_dif_from_aMNLFA_final.csv")
    intercept_dif_final_file = os.path.join(the_dir, "intercept_dif_from_aMNLFA_final.csv")

    if os.path.exists(intercept_dif_final_file) and os.path.exists(lambda_dif_final_file):
        print("Stage 2 ...")
    else:
        print(f"Stage 2 not ready: one or more *_dif_from_aMNLFA_final.csv do not exist")
        exit(1)

    int_dif_table = pd.read_csv(intercept_dif_final_file)
    lambda_dif_table = pd.read_csv(lambda_dif_final_file)

    # For each column other than the first, replace 1s with the column and 0s with None
    for name, i in zip(["int", "lambda"], [int_dif_table, lambda_dif_table]):

        i.columns.values[0] = "var"

        # Replace 1s with column name, and 0s with None
        for ind, column in enumerate(i.columns[1:]):
            i[column] = [column if x == 1 else None for x in i[column]]

        # Extract the surviving lambda DIF by joining cols as list then dropping the Nones
        new_col = f"surv_{name}_dif"
        # ([list(filter(None, x)) for x in i.iloc[:, 1:].values.tolist()])
        i[new_col] = [list(filter(None, x)) for x in i.iloc[:, 1:].values.tolist()]

    surv_dif_table = int_dif_table[["var", "surv_int_dif"]].merge(lambda_dif_table[["var", "surv_lambda_dif"]])

    # Open round 3 calib and
    r3c = os.path.join(the_dir, "round3calibration.inp")
    with open(r3c, "r") as f:
        r3c_text = f.readlines()

    # Extract estimated int dif
    surv_dif_table = surv_dif_table.merge(extract_est_int_dif2(r3c_text))
    surv_dif_table = surv_dif_table.merge(dif_table[["var", "eq"]])
    surv_dif_table["est_lambda_dif"] = [extract_eq(r3c_text, x) for x in surv_dif_table["eq"]]

    # Write csv
    surv_dif_table_out = master_order.merge(surv_dif_table[["var", "surv_lambda_dif", "est_lambda_dif",
                                                            "surv_int_dif", "est_int_dif"]], how="left")
    surv_dif_table_out.to_csv(f"{the_dir}-surv_table.csv", index=False)
    print(f"Stage 2 checking file {the_dir}-surv_table.csv saved")

    ###################################################################################################################
    # Stage 3
    ###################################################################################################################

    scoring = os.path.join(the_dir, "scoring.inp")

    if os.path.exists(scoring):
        print("Stage 3 ...")
    else:
        print(f"Stage 3 not ready: scoring.inp does not exist")
        exit(1)

    # Open scoring inp
    with open(scoring, "r") as f:
        scoring_text = f.readlines()

    stage3_table = dif_table[["var", "eq"]]
    stage3_table["est_lambda_dif"] = [extract_eq(scoring_text, x) for x in stage3_table["eq"]]

    pp.pprint(extract_est_int_dif2(scoring_text))

if __name__ == "__main__":
    main()
