
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
# source("aMNLFA_getr3c.R")
source("aMNLFA_package.R")

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
    mdir = list.files("models/", full.names = TRUE),
    scoring = map(mdir, ~readModels(paste0(.x, "/scoring.out"))),
    r3c = map(mdir, ~readModels(paste0(.x, "/round3calibration.out")))
  )

scoring <- models %>%
  mutate(
    params = map(r3c, ~.x$parameters$unstandardized),
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
  )

intercepts_wide <- intercepts %>%
  select(-est_se, -pval) %>%
  pivot_wider(names_from = covariates, values_from = c(est, se)) %>%
  mutate(
    label = str_remove(param, "^[LS]_")
  )

################################################################################

ggplot(scoring$data[[2]], aes(x = param, y = est)) +
  geom_pointrange(aes(ymin = est - se, ymax = est + se,
                      color = score, shape = covariates), size = 0.5,
                  alpha = 0.75) +
  theme_minimal() +
  labs(x = "Parameter", y = "Estimate",
       title = "Main effects on latent factor after correcting for DIF")

ggplot(intDIF, aes(x = label, y = est)) +
  geom_pointrange(aes(color = covariates, ymin = est - se, ymax = est + se),
                  size = 0.25) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(rows = vars(param), cols = vars(score),
             scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(intercepts_wide, aes(x = est_3d, y = est_sex, color = score)) +
  geom_abline() +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = est_sex - se_sex, ymax = est_sex + se_sex)) +
  geom_errorbarh(aes(xmin = est_3d - se_3d,  xmax = est_3d + se_3d)) +
  ggrepel::geom_text_repel(aes(label = label), size = 3,
                           min.segment.length = 0, color = "black") +
  theme_minimal()

###############################################################################

# Extract savedata (includes ETA results)
savedata <- models %>%
  mutate(
    name = str_remove(mdir, "models/"),
    savedata = map(scoring, ~.x$savedata)
  ) %>%
  select(name, savedata)

all_cov <- left_join(savedata$savedata[[1]], savedata$savedata[[3]],
                     by = c("ID", "D_AGE_C", "D_AGE_C2", "D_MALE", "D_FIRSTB",
                            "D_MOMCOL", "D_AGESEX"),
                     suffix = c("_L", "_S")) %>%
  select(ID, starts_with("D_"), starts_with("ETA"), starts_with("L_"),
         starts_with("S_")) %>%
  mutate(
    age = D_AGE_C + 23,
    model = "3d"
  )

sex_only <- left_join(savedata$savedata[[2]], savedata$savedata[[4]],
                       by = c("ID", "D_AGE_C", "D_MALE"),
                       suffix = c("_L", "_S")) %>%
  select(ID, starts_with("D_"), starts_with("ETA"), starts_with("L_"),
         starts_with("S_")) %>%
  mutate(
    age = D_AGE_C + 23,
    model = "sex"
  )

all_eta <- bind_rows(all_cov, sex_only)

all_eta %>%
  select(model, ID, age, ETA_L, ETA_S) %>%
  write_csv(.data("MNLFA/eta_results.csv"))

ggplot(all_eta, aes(x = ETA_L, y = ETA_S, color = age)) +
  geom_point(alpha = 0.5) +
  viridis::scale_color_viridis() +
  facet_wrap(vars(model), scales = "free") +
  theme_minimal()


