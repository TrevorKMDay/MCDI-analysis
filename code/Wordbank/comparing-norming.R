setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")

library(tidyverse)
library(wordbankr)

# The purpose of this code is to compare the Wordbank norming samples to the
# larger sample

source("wordbank-functions.R")

ws <- get_administration_data("English (American)", "WS")
norming <- ws$data_id[ws$norming]
not_norming <- ws$data_id[!ws$norming]

WS_scored <- readRDS("data/WS-scored.rds")[[2]]
