####################################################
# Load libraries
####################################################

path <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

# devtools::install_github("vtcole/aMNLFA")
# v1.0.0

library(tidyverse)
# devtools::install_github("vtcole/aMNLFA")
library(aMNLFA)
library(MplusAutomation)
library(multiplex)
library(R.utils)

source("../mcdi-setup.R")

## Load in new aMNLFA functions from non-package update
# August 12, 2021; will be replace with new CRAN package eventually
# for (f in list.files("aMNLFA-210819/", pattern = "*.R$", full.names = TRUE))
#   source(f)

select <- dplyr::select

####################################################
# Setting paths & reading in data
####################################################

# Set directory to where script will output mplus files.
wd <- homedir <- getwd()

# What are these?
calib_sample <- '1'
subscale <- 'REP'

# Read in cleaned MB-CDI data
# Isa: just replace this with readRDS()
ws <- read_data("Wordbank/WS-scored.rds")$p

ws_demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(data_id, sex, birth_order, mom_ed) %>%
  mutate(
    # Recode birth into first/not
    birth_order = if_else(birth_order == "First", "first", "later"),
    # Recode mom_ed into college graduated
    mom_ed = if_else(is.na(mom_ed), NA_character_,
                     if_else(mom_ed %in%
                        c("College", "Some Graduate", "Graduate"),
                          "mom_college", "mom_nocollege")),
  )

ws_amnlfa <- ws %>%
  select(-LEXICAL, -SYNTAX) %>%
  left_join(ws_demo) %>%
  rename(
    ID       = data_id,
    D_AGE    = age,
    L_SOUNDS = sounds,
    L_ACTION = action_words,
    L_ANIMAL = animals,
    L_BODYP  = body_parts,
    L_CLOTH  = clothing,
    L_HOUSEH = household,
    L_LOCATE = locations,
    L_OUTSDE = outside,
    L_PEOPLE = people,
    L_PLACES = places,
    L_TOYS   = toys,
    L_VEHICL = vehicles,
    L_DESCRB = descriptive_words,
    L_FOOD   = food_drink,
    L_ROOMS  = furniture_rooms,
    L_GAMES  = games_routines,
    S_HELPV  = helping_verbs,
    S_CONJ   = connecting_words,
    S_PRON   = pronouns,
    S_QUANT  = quantifiers,
    S_QWORDS = question_words,
    S_TIME   = time_words,
    # Correct irregulars
    S_WF_N   = WORD_FORMS_NOUNS,
    S_WF_V   = WORD_FORMS_VERBS,
    # Incorrect overgeneralizations
    S_WE_N   = WORD_ENDINGS_NOUNS,
    S_WE_V   = WORD_ENDINGS_VERBS,
    S_COMPLX = COMPLEXITY
  ) %>%
  mutate(
    # No character values allowed, even in IDs
    ID = as.numeric(ID),
    # Contrast-code demographic variables
    D_AGE_C  = D_AGE - 23,
    D_AGE_C2 = D_AGE_C ^ 2,
    D_MALE   = if_else(sex == "Male", 1, -1),
    D_FIRSTB = if_else(birth_order == "first", 1, -1, NA_real_),
    D_MOMCOL = if_else(mom_ed == "mom_college", 1, -1, NA_real_),
    D_AGESEX = D_AGE_C * D_MALE,
    D_AG2SEX = D_AGE_C2 * D_MALE
  ) %>%
  select(ID, starts_with("D_"), starts_with("L_"), starts_with("S_"), -D_AGE)

table(ws_demo$sex, useNA = "a")
table(ws_amnlfa$D_FIRSTB, ws_amnlfa$D_MOMCOL, useNA = "a")

ws_noNA <- ws_amnlfa %>%
  filter(
    !is.na(D_MALE),
    !is.na(D_FIRSTB),
    !is.na(D_MOMCOL)
  ) %>%
  mutate(
    age_bin = cut(D_AGE_C + 23, breaks = 7)
  )

ws_noNA_ages <- ws_noNA %>%
  group_by(age_bin) %>%
  summarize(
    mean_age = mean(D_AGE_C + 23)
  )

ggplot(ws_noNA, aes(x = age_bin, fill = as.factor(D_MALE))) +
  geom_bar() +
  theme_minimal()

####################################################
# 1.	Define aMNLFA objects (aMNLFA.object)
# Comment out subscale that is not currently being run.
####################################################

models <- tibble(
    dir = paste0("models/", c("lex_3d", "syn_3d", "lex_sex", "syn_sex")),
    df = list(NA)
  )

for (d in models$dir) { dir.create(d, showWarnings = FALSE, recursive = TRUE)}

# These frames have the complete demographics set

models$df[[1]] <- ws_amnlfa %>%
  select(ID, starts_with("D_"), starts_with("L_")) %>%
  filter(
    # Remove those with missing data
    !is.na(D_MALE),
    !is.na(D_FIRSTB),
    !is.na(D_MOMCOL)
  )

models$df[[2]] <- ws_amnlfa %>%
  select(ID, starts_with("D_"), starts_with("S_")) %>%
  filter(
    # Remove those with missing data
    !is.na(D_MALE),
    !is.na(D_FIRSTB),
    !is.na(D_MOMCOL)
  )

# These frames have the sample for confirming sex only

models$df[[3]] <- ws_amnlfa %>%
  filter(
    # Remove those with missing data
    !is.na(D_MALE),
    is.na(D_FIRSTB),
    is.na(D_MOMCOL)
  ) %>%
  select(ID, starts_with("D_"), starts_with("L_"), -D_FIRSTB, -D_MOMCOL)

models$df[[4]] <- ws_amnlfa %>%
  select(ID, starts_with("D_"), starts_with("S_")) %>%
  filter(
    # Remove those with missing data
    !is.na(D_MALE),
    is.na(D_FIRSTB),
    is.na(D_MOMCOL)
  ) %>%
  select(ID, starts_with("D_"), starts_with("S_"), -D_FIRSTB, -D_MOMCOL)

models <- models %>%
  mutate(
    nobs = map_int(df, nrow),
    ob = list(NA)
  )

sapply(models$df, function(x) sum(is.na(x)))

# Get all demo vars
vars <- list()
vars$l <- str_subset(colnames(ws_amnlfa), "^L_")
vars$s <- str_subset(colnames(ws_amnlfa), "^S_")
vars$d <- str_subset(colnames(ws_amnlfa), "^D_")

# Canonical object creation

models$ob[[1]] <- aMNLFA.object(

    # location of data
    dir        = models$dir[1],

    # read in dataframe from R
    mrdata     = models$df[[1]],

    # list a set of indicators of a single factor; make names as short as
    # possible
    indicators = vars$l,

    # age variable (can be centered)
    time       = NULL,

    # mean and var are for things you are substantively interested in
    # mean: what your moderators of interest are
    #       Contrast coding of nominal variables
    meanimpact = vars$d,

    # var: contrast coding of nominal variables; this is computationally
    #      expensive; JUST DO TIME VARIABLE
    varimpact  = c("D_AGE_C", "D_AGE_C2"),

    # this part: specific indicators impacted by mods? should included all
    #      mean/var impact items
    measinvar  = vars$d,

    # which of variables are factors
    factors    = c("D_MALE", "D_FIRSTB", "D_MOMCOL"),

    ID         = "ID",

    # Vars in df not being used for analysis
    auxiliary  = "ID",

    # indicate whether you would like to test measurement invariance of
    # thresholds for ordinal indicators. SET TO TRUE. seems to require at
    # least one categorical indicator?
    thresholds = FALSE

  )

models$ob[[2]] <- aMNLFA.object(
    dir        = models$dir[2],
    mrdata     = models$df[[2]],
    indicators = vars$s,
    time       = NULL,
    meanimpact = vars$d,
    varimpact  = c("D_AGE_C", "D_AGE_C2"),
    measinvar  = vars$d,
    factors    = c("D_MALE", "D_FIRSTB", "D_MOMCOL"),
    ID         = "ID",
    auxiliary  = "ID",
    thresholds = FALSE
  )

models$ob[[3]] <- aMNLFA.object(
    dir        = models$dir[3],
    mrdata     = models$df[[3]],
    indicators = vars$l,
    time       = NULL,
    meanimpact = c("D_AGE_C", "D_MALE"),
    varimpact  = c("D_AGE_C", "D_AGE_C2"),
    measinvar  = c("D_AGE_C", "D_MALE"),
    factors    = "D_MALE",
    ID         = "ID",
    auxiliary  = "ID",
    thresholds = FALSE
  )

models$ob[[4]] <- aMNLFA.object(
    dir        = models$dir[4],
    mrdata     = models$df[[4]],
    indicators = vars$s,
    time       = NULL,
    meanimpact = c("D_AGE_C", "D_MALE"),
    varimpact  = c("D_AGE_C", "D_AGE_C2"),
    measinvar  = c("D_AGE_C", "D_MALE"),
    factors    = "D_MALE",
    ID         = "ID",
    auxiliary  = "ID",
    thresholds = FALSE
  )

models$rds <- paste0(models$dir, "/", basename(models$dir), ".rds")
lapply(1:nrow(models), function(x) saveRDS(models$ob[[x]], models$rds[x]))

# ws_noNA_ages <- ws_noNA %>%
#   group_by(age_bin) %>%
#   nest() %>%
#   mutate(
#     n = map_int(data, nrow),
#     range_str = str_replace(age_bin, ",", "_") %>%
#                   str_remove_all("[^0-9_]"),
#     lob  = map2(data, range_str,
#               ~aMNLFA.object(
#                 dir        = paste0("models/lex-agebin", .y),
#                 mrdata     = .x,
#                 indicators = vars$l,
#                 time       = NULL,
#                 meanimpact = vars$d[-1],
#                 varimpact  = NULL,
#                 measinvar  = vars$d[-1],
#                 factors    = "D_MALE",
#                 ID         = "ID",
#                 auxiliary  = "ID",
#                 thresholds = FALSE
#               )),
#     sob  = map2(data, range_str,
#                 ~aMNLFA.object(
#                   dir        = paste0("models/syn-agebin", .y),
#                   mrdata     = .x,
#                   indicators = vars$s,
#                   time       = NULL,
#                   meanimpact = vars$d[-1],
#                   varimpact  = NULL,
#                   measinvar  = vars$d[-1],
#                   factors    = "D_MALE",
#                   ID         = "ID",
#                   auxiliary  = "ID",
#                   thresholds = FALSE
#                 ))
#   )

# robin has separate objects for each factor; cannot be simultaneously done for
# all factors
# must feel confident about factor going in; i.e., structural invariance
# make sure males and females have overall factor structure

####################################################
# 2.	Plot items over time
####################################################
# source(paste(homedir, 'aMNLFA_itemplots.R', sep=''))

# can give clues to invariance, produces item plots over time for each other
# moderators in folder

# all_models <- c(models$ob, ws_noNA_ages$lob, ws_noNA_ages$sob)
all_models <- models$ob

for (ob in all_models) {

  dir.create(ob$dir, showWarnings = FALSE)
  pngs <- list.files(ob$dir, pattern = "*.png")
  if (length(pngs) == 0)
    aMNLFA.itemplots(ob)

}

####################################################
# 3.	Draw a calibration sample.
# Sample one calibration per ID. Outputs a calibration file. Was originally
# designed for independent obs. Build initial models w/ calib sample.
# do for a couple of calibration samples to check robustness.
####################################################

set.seed(55455)

# Author modified code to allow user to input calibration sample
# source(paste(homedir, 'aMNLFA_sample.R', sep=''))
# aMNLFA.sample(ob,ru)
# produces calibration.dat, full.dat, header.txt, header2.txt, srdata.dat
for (ob in all_models) {

  if (!file.exists(paste0(ob$dir, "/calibration.dat")))
    aMNLFA.sample(ob)

}

for (ob in all_models) {

  rds <- paste0(ob$dir, "/", basename(ob$dir), ".rds")
  if (!file.exists(rds))
    saveRDS(ob, rds)

}

####################################################
# Optional code - used for adding manual additions to MPLUS script
# (e.g. constraining variance of moderators so model would converge)
####################################################

# calib.dat <- read.delim("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/calibration.dat",
#                         header=FALSE)
# vars = read.csv("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/vars.csv",
#                 header=TRUE)
# vars
