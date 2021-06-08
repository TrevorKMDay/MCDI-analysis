if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)
library(tidyselect)
library(corrplot)
library(psych)

source("wordbank-csv2rds.R")
source("wordbank-functions.R")
max_size <- c(566, 221)

select <- dplyr::select
matches <- dplyr::matches

# wg_as_ws <- read_data("Wordbank/Wordbank-WG-191105.rds") %>%
#   filter(type == "word") %>%
#   score.GasS(sc.understands = FALSE)

# TO DO: Use WG-as-WS

wg <- readRDS(.data("Wordbank/WG-scored.RDS"))$n
ws <- readRDS(.data("Wordbank/WS-scored.RDS"))$n

all <- left_join(
   bind_rows(WG.demo, WS.demo),
   bind_rows(wg, ws)
  ) %>%
  mutate(
    TOTAL = select(., -any_of(colnames(WS.demo)),
                   -matches("^[A-Z]", ignore.case = FALSE)) %>%
              rowSums(na.rm = TRUE)
  )

ggplot(all, aes(x = age, y = TOTAL)) +
  geom_point()
