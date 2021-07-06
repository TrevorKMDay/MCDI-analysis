
library(aMNLFA)

####################################################
# 4. Create Mplus input files for mean impact, variance impact,
#    and item-by-item measurement non-invariance (aMNLFA.initial)
#    makes initial models, populated in folder w/ MPlus input files
####################################################

# Produces a series of measinvariance.imp files for each item,
# varimpact.imp, and meanimpact.imp
# Run Models in Mplus --check .out files to see if there are any errors!
# This will run all models in Mplus in the path you set. This will take some time.

source(paste(homedir, "aMNLFA_initial.R", sep = ""))
aMNLFA.initial(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = "always")

##################################
# 5. Incorporate all "marginally significant" terms into a simultaneous Mplus
#    input file;
################################

# produces Mplus Run Models.log
# Running this code results in a single Mplus script file (round2calibration.inp)
#	All mean and variance impact terms with p<.10 are included

source(paste(homedir, "aMNLFA_simultaneous.R", sep = ""))
aMNLFA.simultaneous(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = "always")

##################################
# 6. Trim non-sig terms.
################################

# source(paste(homedir, "MNLFA/scripts/aMNLFA_final.R", sep = ""))
aMNLFA.final(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = "always")

##################################
# 7. (only for longitudinal data) Use parameter values generated from the last
# calibration model to fix
# parameter values in the scoring model using the full, longitudinal dataset
################################

#added kevin"s script
source(paste(homedir, "MNLFA/scripts/aMNLFA_scores.R", sep = ""))
aMNLFA.scores(ob)

# The resulting Mplus script uses the long (mr.dat) data file and outputs factor
# score estimates for each observation. Run the resulting scores.inp script
# manually.
# This script produces a file containing factor score estimates if data are
# cross-sectional.

##################################
# 8. Describe and visualize factor score estimates and generate empirical item
# characteristic curves
##################################

# NOTE: Step 7 saves "scores.dat" in the wrong directory (was in my root dir).
# Need to move this file to wdir before running this last line.
source(paste(homedir,"aMNLFA_scoreplots.R", sep = ""))
aMNLFA.scoreplots(ob)

