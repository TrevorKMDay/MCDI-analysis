
library(tidyverse)
library(MplusAutomation)
library(viridis)

# library(aMNLFA)
# library(R.utils)


path <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
locs <- c("G:/My Drive", "I:", "/Volumes")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

source("../mcdi-setup.R")

select <- dplyr::select

###############################################################################

# Demographics
demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(-instrument) %>%
  mutate(
    first_born = birth_order == "First",
    mom_col    = if_else(!is.na(mom_ed),
                         mom_ed %in% c("College", "Some Graduate", "Graduate"),
                         NA)
  )

demo_sexonly <- demo %>%
  filter(
    is.na(birth_order) |
      is.na(mom_ed),
    !is.na(sex)
  )

table(demo_sexonly$age)

composite <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(data_id, age, LEXICAL, SYNTAX)

percent <- read_data("Wordbank/WS-scored.rds")$p

###############################################################################

# Look up tables from loadings to short names

lex_item_lambda_lut <- tibble(
  item_lambda_label = paste0("L", 1:16),
  var =  c("ACTION", "ANIMAL", "BODYP",  "CLOTH",  "DESCRB", "FOOD", "ROOMS",
           "GAMES",  "HOUSEH", "LOCATE", "OUTSDE", "PEOPLE", "PLACES",
           "SOUNDS", "TOYS",   "VEHICL")
)

syn_item_lambda_lut <- tibble(
  item_lambda_label = paste0("L", 1:11),
  var = c("CONJ", "HELPV", "PRON", "QUANT", "QWORDS", "TIME", "WF_N", "WF_V",
          "WE_N", "WE_V", "COMPLX")
)

# This is from the input - just so we don't assume th mean age is exactly
#   halfway.
age_lut <- tibble(agebin = c("16_18", "18_20", "20_22", "22_24", "24_26",
                             "26_28", "28_30"),
                  mean_age = c(17.21, 19.44, 21.51, 23.75, 25.37, 27.43,
                               29.66))

###############################################################################

# x <- readModels("models/lex_3d/scoring.out")

models <- tibble(
  mdir = list.files("models/", full.names = TRUE, pattern = "onefac"),
  scoring = map(mdir, ~readModels(paste0(.x, "/scoring.out"))),
  r3c = map(mdir, ~readModels(paste0(.x, "/round3calibration.out")))
)

scoring <- models %>%
  mutate(
    params = map(scoring, ~.x$parameters$unstandardized),
    name = str_remove(mdir, "models/")
  ) %>%
  select(name, params) %>%
  unnest(params) %>%
  mutate(
    across(c(est, se, est_se, pval), ~replace(.x, .x == 999, NA)),
    group = replace(paramHeader, str_detect(paramHeader, "^[LS]_.*.ON$"),
                    "interceptDIF")
  ) %>%
  separate(name, into = c("score", "covariates"), sep = "_") %>%
  group_by(group) %>%
  nest()

intDIF <- scoring$data[[3]] %>%
  mutate(
    param = str_remove(param, "D_"),
    label = str_remove_all(scoring$data[[3]]$paramHeader, "(^[LS]_|.ON$)")
  )

intercepts <- scoring$data[[4]] %>%
  filter(
    param != "ETA"
  ) %>%
  mutate(
    label = str_remove_all(param, "^._")
  )

intercepts_wide <- intercepts %>%
  select(-est_se, -pval) %>%
  pivot_wider(names_from = covariates, values_from = c(est, se)) %>%
  mutate(
    label = str_remove(param, "^[LS]_")
  )

ggplot(intercepts, aes(x = label, y = est)) +
  geom_point() +
  geom_point(data = intDIF, aes(x = label, y = est, color = param))
