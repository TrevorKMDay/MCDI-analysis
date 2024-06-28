path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(psych)

source("../mcdi-setup.R")

# Simulate data =====

wb24mo <- read_data("Wordbank/WS-scored-230214.rds") %>%
  filter(
    age == 24
  )

wb24mo_mean <- mean(wb24mo$comprehension)
wb24mo_sd <- sd(wb24mo$comprehension)

set.seed(55455)

simdat <- tibble(
    ws0 = rnorm(10000, mean = wb24mo_mean, sd = wb24mo_sd)
  ) %>%
  mutate(
    # If value is less than 0 or greater than 680, floor/ceiling it and then
    # round
    ws0 = if_else(ws0 < 0, 0, if_else(ws0 > 680, 680, round(ws0))),
  )

for (i in 1:10) {

  error <- rnorm(10000, mean = 0, sd = 0.03 * 680) %>%
    round()

  simdat[, paste0("ws", i)] <- simdat$ws0 - error

}

simdat %>%
  select(starts_with("ws")) %>%
  ICC()

ggplot(simdat, aes(x = ws0, y = ws1)) +
  geom_point()

# WG data =====

wg <- read_data("Wordbank/WG-scored-230214.rds")

sum(wg$comprehension > 250) / nrow(wg)

wg %>%
  filter(
    comprehension > 250
  ) %>%
  pull(age) %>%
  range()
