if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(readr)
library(dplyr)

source("../mcdi-setup.R")

#
# Convert actual data from CSV to RDS to load faster
#

sentences_file <- most_recent("Wordbank-WS-[0-9]*.csv", directory = data_dir)
gestures_file  <- most_recent("Wordbank-WG-[0-9]*.csv", directory = data_dir)

for (csv_file in c(gestures_file, sentences_file)) {

  rds_file <- gsub(".csv", ".rds", csv_file)

  if (!file.exists(rds_file) ||
      (file.info(csv_file)$ctime < file.info(rds_file)$ctime)) {

    data <- read_csv(csv_file,
                    col_types = cols(.default = "f", age = "i"))
    write_rds(data, path = rds_file)

  }

}

#
# Extract demographics and save to RDS
#

WS <- readRDS(gsub("csv", "rds", sentences_file))
WG <- readRDS(gsub("csv", "rds", gestures_file))

WS.demo <- WS %>%
  dplyr::select(data_id, age, sex, mom_ed) %>%
  distinct() %>%
  mutate(instrument = "S")

WG.demo <- WG %>%
  dplyr::select(data_id, age, sex, mom_ed) %>%
  distinct() %>%
  mutate(instrument = "G")

save_data(WS.demo, "Wordbank/WS-demographics.rds")
save_data(WG.demo, "Wordbank/WG-demographics.rds")
