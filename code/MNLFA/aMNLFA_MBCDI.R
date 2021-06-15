####################################################
# Load libraries
####################################################

setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/MNLFA")

library(tidyverse)
library(aMNLFA)
library(MplusAutomation)
library(multiplex)

source("../mcdi-setup.R")

####################################################
# Setting paths & reading in data
####################################################

# Set directory to where script will output mplus files.
homedir <- '/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/'
wd <- homedir

# What are these?
calib_sample <- '1'
subscale <- 'REP'

# Read in cleaned MB-CDI data
ws <- read_data("Wordbank/WS-scored.rds")$p
ws_demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(data_id, age, sex)

ws_amnlfa <- ws %>%
  select(-LEXICAL, -SYNTAX) %>%
  left_join(ws_demo) %>%
  rename(
    ID       = data_id,
    AGE      = age,
    SEX      = sex,
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
    MALE = ifelse(SEX == "Male", 1, -1)
  ) %>%
  select(ID, AGE, MALE, starts_with("L_"), starts_with("S_"))

####################################################
# 1.	Define aMNLFA objects (aMNLFA.object)
# Comment out subscale that is not currently being run.
####################################################

# Repetitive motor --defining object to pass to model
# colnames(df)

df <- ws_amnlfa
wd <- paste0(data_dir, "/MNLFA")

lex_ob <- aMNLFA.object(
    # location of data
    dir        = wd,
    # read in dataframe from R
    mrdata     = df,
    # list a set of indicators of a single factor; make names as short as
    # possible
    indicators = str_subset(colnames(df), "^L_"),
    # age variable (can be centered)
    time       = "AGE",
    # mean and var are for things you are substantively interested in
    # mean: what your moderators of interest are
    #       Contrast coding of nominal variables
    meanimpact =  c("AGE", "MALE"),
    # var: contrast coding of nominal variables; this is computationally
    #      expensive; JUST DO TIME VARIABLE
    varimpact  = "AGE",
    # this part: specific indicators impacted by mods? should included all
    #      mean/var impact items
    measinvar  = c("AGE", "MALE"),
    # which of variables are factors
    factors    = c("MALE"),
    ID         = "ID",
    # All variables are used in analysis
    auxiliary  = NULL,
    # indicate whether you would like to test measurement invariance of
    # thresholds for ordinal indicators. SET TO TRUE. seems to require at
    # least one categorical indicator?
    thresholds = FALSE
  )

syn_ob <- aMNLFA.object(
  dir        = wd,
  mrdata     = df,
  indicators = str_subset(colnames(df), "^S_"),
  time       = "AGE",
  meanimpact =  c("AGE", "MALE"),
  varimpact  = "AGE",
  measinvar  = c("AGE", "MALE"),
  factors    = c("MALE"),
  ID         = "ID",
  auxiliary  = NULL,
  thresholds = FALSE
)

# robin has separate objects for each factor; cannot be simultaneously done for
# all factors
# must feel confident about factor going in; i.e., structural invariance
# make sure males and females have overall factor structure

####################################################
# 2.	Plot items over time
####################################################
# source(paste(homedir, 'aMNLFA_itemplots.R', sep=''))

#can give clues to invariance, produces item plots over time for each other moderators in folder
aMNLFA::aMNLFA.itemplots(ob)

####################################################
# 3.	Draw a calibration sample.
# Sample one calibration per ID. Outputs a calibration file. Was originally
# designed for independent obs. Build initial models w/ calib sample.
# do for a couple of calibration samples to check robustness.
####################################################

# Author modified code to allow user to input calibration sample
# source(paste(homedir, 'aMNLFA_sample.R', sep=''))
# aMNLFA.sample(ob,ru)
# produces calibration.dat, full.dat, header.tx, header2.txt, srdata.dat
aMNLFA::aMNLFA.sample(ob)

# THIS IS HOW FAR I GOT

####################################################
# Optional code - used for adding manual additions to MPLUS script
# (e.g. constraining variance of moderators so model would converge)
####################################################

#calib.dat <- read.delim("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/calibration.dat", header=FALSE)
#vars = read.csv("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/vars.csv", header=TRUE)
#vars = vars$x
#colnames(calib.dat) = vars
#var(calib.dat$TABLET)
#var(calib.dat$MALE)

####################################################
# 4.	Create Mplus input files for mean impact, variance impact,
# and item-by-item measurement non-invariance (aMNLFA.initial)
#makes initial models, populated in folder w/ MPlus input files
####################################################

#produces a series of measinvariance.imp files for each item,
# varimpact.imp, and meanimpact.imp
# Run Models in Mplus --check .out files to see if there are any errors!
source(paste(homedir, 'aMNLFA_initial.R', sep = ''))
aMNLFA.initial(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = 'always') # This will run all models in Mplus in the path you set. This will take some time.

##################################
# 5. Incorporate all ‘marginally significant’ terms into a simultaneous Mplus input file;
################################

source(paste(homedir, 'aMNLFA_simultaneous.R', sep =''))
aMNLFA.simultaneous(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = 'always') #produces Mplus Run Models.log
#Running this code results in a single Mplus script file (round2calibration.inp)
#	All mean and variance impact terms with p<.10 are included

##################################
# 6. Trim non-sig terms.
################################

# source(paste(homedir, 'MNLFA/scripts/aMNLFA_final.R', sep = ''))
aMNLFA.final(ob)
runModels("/Users/isabella/Desktop/E-Lab/Joint attention/Social motivation/MNLFA/",
          replaceOutfile = 'always')

##################################
# 7. (only for longitudinal data) Use parameter values generated from the last
# calibration model to fix
# parameter values in the scoring model using the full, longitudinal dataset
################################

#added kevin's script
source(paste(homedir, 'MNLFA/scripts/aMNLFA_scores.R', sep = ''))
aMNLFA.scores(ob)
#The resulting Mplus script uses the long (mr.dat) data file and outputs factor
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
source(paste(homedir,'aMNLFA_scoreplots.R', sep = ''))
aMNLFA.scoreplots(ob)

