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
# v1.1.0

library(tidyverse)
library(aMNLFA)
library(MplusAutomation)
library(multiplex)
library(R.utils)

source("../mcdi-setup.R")

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
    D_MALE   = if_else(sex == "Male", 1, -1),
    D_FIRSTB = if_else(birth_order == "first", 1, -1, NA_real_),
    D_MOMCOL = if_else(mom_ed == "mom_college", 1, -1, NA_real_)
  ) %>%
  select(ID, starts_with("D_"), starts_with("L_"), starts_with("S_"))

table(ws_demo$sex, useNA = "a")
table(ws_amnlfa$D_FIRSTB, ws_amnlfa$D_MOMCOL, useNA = "a")

ws_noNA <- ws_amnlfa %>%
  filter(
    !is.na(D_MALE),
    !is.na(D_FIRSTB),
    !is.na(D_MOMCOL)
  ) %>%
  mutate(
    D_AGE_C = D_AGE - 24,
    D_AGE_C2 = D_AGE_C ^ 2,
    D_AGCMLE = D_AGE_C * D_MALE,
    D_AC2MLE = D_AGE_C2 * D_MALE,
  ) %>%
  select(-D_AGE)

ggplot(ws_noNA, aes(x = age_bin, fill = as.factor(D_MALE))) +
  geom_bar() +
  theme_minimal()

####################################################
# 1.	Define aMNLFA objects (aMNLFA.object)
# Comment out subscale that is not currently being run.
####################################################

# Get all demo vars
vars <- list()
vars$l <- str_subset(colnames(ws_noNA), "^L_")
vars$s <- str_subset(colnames(ws_noNA), "^S_")
vars$d <- str_subset(colnames(ws_noNA), "^D_")

# Canonical object creation
onefac_ob <- aMNLFA.object(

    # location of data
    dir        = "models/onefac",

    # read in dataframe from R
    mrdata     = ws_noNA,

    # list a set of indicators of a single factor; make names as short as
    # possible
    indicators = c(vars$l, vars$s),

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

####################################################
# 2.	Plot items over time
####################################################
# source(paste(homedir, 'aMNLFA_itemplots.R', sep=''))

# can give clues to invariance, produces item plots over time for each other
# moderators in folder
dir.create("models/onefac", showWarnings = FALSE, recursive = TRUE)
aMNLFA.itemplots(onefac_ob)

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
aMNLFA.sample(onefac_ob)

saveRDS(onefac_ob, "models/onefac/onefac.rds")

####################################################
# Optional code - used for adding manual additions to MPLUS script
# (e.g. constraining variance of moderators so model would converge)
####################################################

# calib.dat <- read.delim("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/calibration.dat",
#                         header=FALSE)
# vars = read.csv("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/vars.csv",
#                 header=TRUE)
# vars
