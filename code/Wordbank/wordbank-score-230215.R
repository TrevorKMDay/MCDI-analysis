if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)

source("../mcdi-setup.R")
source("wordbank-functions.R")

# Load data ====

WS <- read_data("Wordbank/WS-230215.rds")
WG <- read_data("Wordbank/WG-230215.rds")

WS_scored <- WS %>%
  score.WS(include.totals = TRUE)

WG_scored <- WG %>%
  score.WG(produces_value = "produces")

WG_asWS <- WG %>%
  score.GasS()

WS_demo <- read_data("Wordbank/WS-demographics-230215.rds")
WG_demo <- read_data("Wordbank/WG-demographics-230215.rds")

# Combine data

WS2 <- left_join(WS_demo, WS_scored$n) %>%
  mutate(
    date_of_test = lubridate::as_date(date_of_test)
  ) %>%
  select(-production, -is_norming, -dataset_origin_name, -language, -form_type,
         -language_exposures, -health_conditions)

WG2 <- left_join(WG_demo, WG_scored$n) %>%
  mutate(
    date_of_test = lubridate::as_date(date_of_test)
  ) %>%
  select(-production, -is_norming, -dataset_origin_name, -language, -form_type,
         -language_exposures, -health_conditions)

WG_asWS2 <- left_join(WG_demo, WG_asWS$n) %>%
  mutate(
    date_of_test = lubridate::as_date(date_of_test)
  ) %>%
  select(-production, -is_norming, -dataset_origin_name, -language, -form_type,
         -language_exposures, -health_conditions)

WB <- bind_rows(WG_asWS2, WS2)

save_data(WS2, "Wordbank/WS-scored-230214.rds")
save_data(WG2, "Wordbank/WG-scored-230214.rds")
save_data(WG_asWS2, "Wordbank/WGasWS-scored-230214.rds")
save_data(WB, "Wordbank/WGasWS_WS-scored-230214.rds")
