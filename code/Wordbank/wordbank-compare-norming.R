if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}


library(tidyverse)
library(wordbankr)

# The purpose of this code is to compare the Wordbank norming samples to the
# larger sample

source("wordbank-csv2rds.R")
source("wordbank-functions.R")

ws <- get_administration_data("English (American)", "WS")
ws.norming <- ws$data_id[ws$norming]
ws.not_norming <- ws$data_id[!ws$norming]

table(ws$ethnicity, useNA = "always")
round(table(ws$ethnicity, useNA = "always") / 2715 * 100)

table(ws$birth_order, useNA = "always")
round(table(ws$birth_order, useNA = "always") / 2776 * 100)

ggplot(filter(ws, !is.na(ethnicity)),
       aes(x = ethnicity, y = production)) +
  geom_violin() +
  geom_jitter(alpha = 0.5)


lm(production ~ sex, data = ws) %>% summary()
lm(production ~ ethnicity, data = ws) %>% summary()
lm(production ~ birth_order == "First", data = ws) %>% summary()

# WS_scored <- read_data("Wordbank/WS-scored.rds")[[2]]

wg <- get_administration_data("English (American)", "WG")
wg.norming <- wg$data_id[wg$norming]
wg.not_norming <- wg$data_id[!wg$norming]

table(wg$ethnicity, useNA = "always")
round(table(wg$ethnicity, useNA = "always") / 1067 * 100)

table(wg$birth_order, useNA = "always")
round(table(wg$birth_order, useNA = "always") / 1047 * 100)


# WG_scored <- read_data("Wordbank/WG-scored.rds")[[2]]

(length(wg.norming) + length(ws.norming)) + (length(wg.not_norming) + length(ws.not_norming))


# Compare birth order
bo_lut <- tibble(birth_order = levels(ws$birth_order),
                 birth_order_n = 1:8)

ws <- left_join(ws, bo_lut)
wg <- left_join(wg, bo_lut)

t.test(ws$birth_order_n, wg$birth_order_n)

# Compare ethnicity
tibble(ws = table(ws$ethnicity),
       wg = table(wg$ethnicity)) %>%
  chisq.test()
