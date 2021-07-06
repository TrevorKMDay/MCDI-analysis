####################################################
# Load libraries
####################################################

path <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
locs <- c("G:/My Drive", "I:", "/Volumes")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

# devtools::install_github("vtcole/aMNLFA")
# v1.0.0

library(tidyverse)
library(aMNLFA)
library(MplusAutomation)
library(multiplex)

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
  select(data_id, sex, birth_order) %>%
  mutate(
    # Recode birth into first/not
    birth_order = if_else(birth_order == "First", "First", "Later")
  )

ws_amnlfa <- ws %>%
  select(-LEXICAL, -SYNTAX) %>%
  left_join(ws_demo) %>%
  rename(
    ID       = data_id,
    SEX      = sex,
    B_ORDER  = birth_order,
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
    MALE    = ifelse(SEX == "Male", 1, -1),
    FIRST_B = ifelse(B_ORDER == "First", 1, -1)
  ) %>%
  select(ID, FIRST_B, MALE, starts_with("L_"), starts_with("S_"))

table(ws_demo$birth_order, useNA = "a")

####################################################
# 1.	Define aMNLFA objects (aMNLFA.object)
# Comment out subscale that is not currently being run.
####################################################

# Repetitive motor --defining object to pass to model
# colnames(df)

df <- ws_amnlfa %>%
  mutate(
    # No character values allowed, even in IDs
    ID = as.numeric(ID)
  )

dirs <- paste0(code_dir, "/MNLFA/", c("lex", "syn"))
for (d in dirs) { dir.create(d, showWarnings = FALSE, recursive = TRUE) }

lex_ob <- aMNLFA.object(

    # location of data
    dir        = dirs[1],

    # read in dataframe from R
    mrdata     = df,

    # list a set of indicators of a single factor; make names as short as
    # possible
    indicators = str_subset(colnames(df), "^L_"),

    # age variable (can be centered)
    time       = NULL,

    # mean and var are for things you are substantively interested in
    # mean: what your moderators of interest are
    #       Contrast coding of nominal variables
    meanimpact =  c("FIRST_B", "MALE"),

    # var: contrast coding of nominal variables; this is computationally
    #      expensive; JUST DO TIME VARIABLE
    varimpact  = NULL,

    # this part: specific indicators impacted by mods? should included all
    #      mean/var impact items
    measinvar  = c("FIRST_B", "MALE"),

    # which of variables are factors
    factors    = c("FIRST_B", "MALE"),

    ID         = "ID",

    # CHANGE THIS
    auxiliary  = str_subset(colnames(df), "^S_"),

    # indicate whether you would like to test measurement invariance of
    # thresholds for ordinal indicators. SET TO TRUE. seems to require at
    # least one categorical indicator?
    thresholds = FALSE

  )

syn_ob <- aMNLFA.object(
  dir        = dirs[2],
  mrdata     = df,
  indicators = str_subset(colnames(df), "^S_"),
  time       = NULL,
  meanimpact = c("FIRST_B", "MALE"),
  varimpact  = NULL,
  measinvar  = c("FIRST_B", "MALE"),
  factors    = c("FIRST_B", "MALE"),
  ID         = "ID",
  auxiliary  = str_subset(colnames(df), "^L_"),
  thresholds = TRUE
)

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
aMNLFA.itemplots(lex_ob)
aMNLFA.itemplots(syn_ob)

####################################################
# 3.	Draw a calibration sample.
# Sample one calibration per ID. Outputs a calibration file. Was originally
# designed for independent obs. Build initial models w/ calib sample.
# do for a couple of calibration samples to check robustness.
####################################################

# Author modified code to allow user to input calibration sample
# source(paste(homedir, 'aMNLFA_sample.R', sep=''))
# aMNLFA.sample(ob,ru)
# produces calibration.dat, full.dat, header.txt, header2.txt, srdata.dat
aMNLFA.sample(lex_ob)
aMNLFA.sample(syn_ob)

# THIS IS HOW FAR I GOT

####################################################
# Optional code - used for adding manual additions to MPLUS script
# (e.g. constraining variance of moderators so model would converge)
####################################################

# calib.dat <- read.delim("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/calibration.dat",
#                         header=FALSE)
# vars = read.csv("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/vars.csv",
#                 header=TRUE)
# vars = vars$x
# colnames(calib.dat) = vars
# var(calib.dat$TABLET)
# var(calib.dat$MALE)


saveRDS(lex_ob, "lex_ob.rds")
saveRDS(syn_ob, "syn_ob.rds")
