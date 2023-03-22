aMNpath <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
locs <- c("G:/My Drive", "I:", "/Volumes")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

# Empties working environment 
rm(list = ls())

library(tidyverse)
library(aMNLFA) #-- currently out of date
library(R.utils)
library(MplusAutomation)

# load("C:/Users/day00096/Desktop/aMNLFA_local/lex_ob.Rdata")

################################################################################

## Load in new aMNLFA functions from non-package update
# August 19, 2021; will be replaced with new CRAN package eventually
# for (f in list.files("aMNLFA-210819", pattern = "*.R$", full.names = TRUE))
#   source(f)

# Set directory to where script will output mplus files.
wd <- homedir <- getwd()

################################################################################
# Get objects

# Load in RDSes and then check to make sure they're the right kind of objects
# Could later go back and fix the naming pattern so they're identifiable on 
#  name alone.
obs <- tibble(
    f = list.files("models/", pattern = "*.rds$", recursive = TRUE,
                  full.names = TRUE),
    ob = list(NA)
  ) %>%
  mutate(
    bn = map_chr(f, ~str_remove(basename(.x), ".rds")),
  )

# Load models separately for clearer indication of when/if one fails
for (i in 1:nrow(obs)) {
  f <- obs$f[i]
  message(obs$bn[i])
  try(obs$ob[[i]] <- readRDS(f))
}

obs <- obs %>%
  mutate(
    class     = map(ob, class),
    is_AMNLFA = map_lgl(class, ~("aMNLFA.object" %in% .x))
  ) %>%
  filter(
    is_AMNLFA,
    # Skip lex_3d for now
    # bn != "lex_3d"
  )


###############################################################################
# 4. Create Mplus input files for mean impact, variance impact,
#    and item-by-item measurement non-invariance (aMNLFA.initial)
#    makes initial models, populated in folder w/ MPlus input files
###############################################################################

# Produces;
#   a series of measinvariance.imp files for each item,
#  varimpact.imp, and meanimpact.imp
# Run Models in Mplus --check .out files to see if there are any errors!
# This will run all models in Mplus in the path you set. This will take some 
#  time.

for (ob in obs$ob) {
  
  # Only run initial if there's no mean impact script
  if (!file.exists(paste0(ob$dir, "/meanimpactscript.inp"))){
    message(date(), ": Running initial for ", ob$dir)
    aMNLFA.initial(ob)
  }
  
  # using "modifiedDate" will only run this dir if meanimpactscript.out is 
  #  older than .inp
  message(date(), ": Running models for ", ob$dir)
  runModels(ob$dir, replaceOutfile = "never", quiet = FALSE)
  
}


###############################################################################
# 5. Incorporate all "marginally significant" terms into a simultaneous Mplus
#    input file
###############################################################################

# Produces:
#  Mplus Run Models.log
#  Running this code results in a single Mplus script file 
#  (round2calibration.inp)
#	All mean and variance impact terms with p<.10 are included

for (ob in obs$ob) { 
  
  # Only run initial if there's no round 2 calibration input script
  if (!file.exists(paste0(ob$dir, "/round2calibration.inp"))){
    message(date(), ": Running simultaneous for ", ob$dir)
    aMNLFA.simultaneous(ob)
  }
  
  # using "modifiedDate" will only run this dir if r2c out is older than .inp
  message(date(), ": Running models for ", ob$dir)
  runModels(ob$dir, replaceOutfile = "never", quiet = FALSE)
  
}

aMNLFA.simultaneous(obs$ob[obs$bn == "lex_3d"][[1]])
runModels("models/lex_3d/", replaceOutfile = "modifiedDate", quiet = FALSE)

###############################################################################
# 6. Trim non-significant terms.
###############################################################################

# NEW V1.0 steps

for (ob in obs$ob) {

  # Only run initial if there's no round 3 calibration input script
  if (!file.exists(paste0(ob$dir, "/round3calibration.inp"))){
    
    message(date(), ": Pruning ", ob$dir)
    pruned <- aMNLFA.prune(ob)
    
    
    message(date(), ": Creating DIFplots for ", ob$dir)
    
    png(paste0(ob$dir, "/int_DIF.png"))
    aMNLFA.DIFplot(pruned, diftype = "intercept")
    dev.off()
    
    png(paste0(ob$dir, "/load_DIF.png"))
    aMNLFA.DIFplot(pruned, diftype = "loading")
    dev.off() 
    
    message(date(), ": Running final for ", ob$dir)
    aMNLFA.final(ob)
  }
  
  # using "modifiedDate" will only run this dir if r3c out is older than .inp
  message(date(), ": Running models for ", ob$dir)
  runModels(ob$dir, replaceOutfile = "never", quiet = FALSE)

}

runModels(obs$ob[[8]]$dir)

###############################################################################
# 7. (only for longitudinal data) 
#    Use parameter values generated from the last calibration model to fix
#    parameter values in the scoring model using the full, longitudinal dataset
###############################################################################

for (ob in obs$ob) { 
  
  # Only run initial if there's no round 2 calibration input script
  if (!file.exists(paste0(ob$dir, "/scores.dat"))){
    message(date(), ": Running scores for ", ob$dir)
    aMNLFA.scores(ob)
  }
  
  # using "modifiedDate" will only run this dir if r2c out is older than .inp
  message(date(), ": Running models for ", ob$dir)
  runModels(ob$dir, replaceOutfile = "never", quiet = FALSE)
  
}

# No longer use aMNLFA.scoreplots