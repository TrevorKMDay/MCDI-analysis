if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(readr)
library(dplyr)
# Get additional demographics
library(wordbankr)

source("../mcdi-setup.R")

date <- "230215"

# Convert actual data from CSV to RDS to load faster ====

ws_items <- get_item_data("English (American)", "WS")
wg_items <- get_item_data("English (American)", "WG")

ws_data <- get_instrument_data("English (American)", "WS")
wg_data <- get_instrument_data("English (American)", "WG")

WS <- left_join(ws_data, ws_items) %>%
  select(-form_type) %>%
  mutate(
    data_id = as.character(data_id)
  )


WG <- left_join(wg_data, wg_items) %>%
  select(-form_type) %>%
  mutate(
    data_id = as.character(data_id)
  )

WB <- bind_rows(WS, WG)

saveRDS(WS, .data(paste0("Wordbank/WS-", date, ".rds")))
saveRDS(WG, .data(paste0("Wordbank/WG-", date, ".rds")))
saveRDS(WB, .data(paste0("Wordbank/WG_WS-", date, ".rds")))

# Demographics ====

ws_demo <- get_administration_data(language = "English (American)",
                                    form = "WS",
                                    include_demographic_info = TRUE,
                                    include_health_conditions = TRUE,
                                    include_language_exposure = TRUE) %>%
  mutate(
    data_id = as.character(data_id)
  )

wg_demo <- get_administration_data(language = "English (American)",
                        form = "WG",
                        include_demographic_info = TRUE,
                        include_health_conditions = TRUE,
                        include_language_exposure = TRUE) %>%
  mutate(
    data_id = as.character(data_id)
  )

wb_demo <- bind_rows(wg_demo, ws_demo)

saveRDS(ws_demo, .data(paste0("Wordbank/WS-demographics-", date, ".rds")))
saveRDS(wg_demo, .data(paste0("Wordbank/WG-demographics-", date, ".rds")))
saveRDS(wb_demo, .data(paste0("Wordbank/WG_WS-demographics-", date, ".rds")))

# Dictionaries ====

s_dict <- WS %>%
  filter(
    item_kind == "word"
  ) %>%
  select(category, item_definition, item_id, item_kind) %>%
  distinct()

g_dict <- WG %>%
  filter(
    item_kind == "word"
  ) %>%
  select(category, item_definition, item_id, item_kind) %>%
  distinct()

write_csv(s_dict, .data("other/s_dict.csv"))
write_csv(g_dict, .data("other/g_dict.csv"))
