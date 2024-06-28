setwd("~/Insync/day00096@umn.edu/Google Drive/Research/MCDI/MCDI-analysis/code/")

library(tidyverse)
library(effsize)

s_dict <- read_csv("../data/other/s_dict.csv")
categories <- unique(s_dict$category)

# Wordbank

wb <- readRDS("../data/Wordbank/WS-scored-230214.rds") %>%
  group_by(age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    ecdf = map(data, ~ecdf(.x$comprehension))
  )

wb_ecdf <- select(wb, age, ecdf)

get_percentile <- function(age, total) {

  the_ecdf <- wb_ecdf$ecdf[wb_ecdf$age == age][[1]]

  return(the_ecdf(total))

}

eirli <- read_csv("../data/EIRLI/EIRLI_clean.csv",
                  show_col_types = FALSE) %>%
  select(data_id, gender, age, exact_age, any_of(categories)) %>%
  mutate(
    TOTAL = select(., any_of(categories)) %>%
      rowSums(),
  ) %>%
  select(-any_of(categories)) %>%
  group_by(data_id) %>%
  mutate(
    lag_TOTAL = lag(TOTAL),
    delta = TOTAL - lag_TOTAL
  ) %>%
  filter(
    age >= 16,
    age <= 30
  ) %>%
  mutate(
    PERCENTILE = map2_dbl(age, TOTAL, ~get_percentile(.x, .y)),
    mean_PCTILE = mean(PERCENTILE)
  )

cor(eirli$delta, eirli$exact_age, use = "c") # 0.18

cohen.d(eirli$delta[eirli$gender == "Male"],
        eirli$delta[eirli$gender == "Female"],
        na.rm = TRUE) # 0.05

sex_delta_d <- 0.0562
sex_delta_r <- sex_delta_d / sqrt(sex_delta_d^2 + 4)

cohen.d(eirli$mean_PCTILE[eirli$gender == "Male"],
        eirli$mean_PCTILE[eirli$gender == "Female"],
        na.rm = TRUE) # -0.315

sex_mean_d <- -0.315
sex_mean_r <- sex_mean_d / sqrt(sex_mean_d^2 + 4)

# Structural

bcp_demo <- read_csv("../data/BCP/bcp-UMNUNC-demoeligb-200722.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(
    demographics.CandID, demographics.Sex
  ) %>%
  distinct() %>%
  mutate(
    sub = paste0("sub-", demographics.CandID)
  ) %>%
  rename(
    sex = demographics.Sex
  ) %>%
  select(-demographics.CandID)

ct <- readRDS("../../../laterality/laterality-bcp/data/BCP_all_thickness.rds") %>%
  select(sub, ses, ends_with("BA44"), ends_with("MeanThickness")) %>%
  group_by(sub) %>%
  mutate(
    age = as.numeric(str_remove_all(ses, "(ses-|mo)")),
    lr_MeanThickness = (lh_MeanThickness + rh_MeanThickness) / 2,
    lag_lr_MT = lag(lr_MeanThickness),
    delta = lr_MeanThickness - lag_lr_MT,

    lag_lhBA44 = lag(lh_BA44),
    lag_rhBA44 = lag(rh_BA44),
    delta_lhBA44 = lh_BA44 - lag_lhBA44,
    delta_rhBA44 = rh_BA44 - lag_rhBA44

  ) %>%
  left_join(bcp_demo)

ggplot(ct, aes(age, delta)) + geom_point() + geom_smooth(method = "lm")

cor(ct$age, ct$delta, use = "c")

ct %>%
  ungroup() %>%
  select(starts_with("delta")) %>%
  na.omit() %>%
  cor()

cohen.d(ct$delta[ct$sex == "Male"], ct$delta[ct$sex == "Female"],
        na.rm = TRUE) # 0.259

sex_ct_d <- 0.259
sex_ct_r <- 0.259 / sqrt(0.259^2 + 4)

library(simpr)
