if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Libraries

library(tidyverse)
library(tidyselect)
library(readxl)
library(Hmisc)

# Helper functions

source("wordbank-functions.R")
source("format-BCP-funcs.R")

# Read data

mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-200131.csv")
s_dict_file <- "data/WS-example.csv"
g_dict_file <- "data/WG-example.csv"

# Clean up mcdi_all
colnames(mcdi_all) <- gsub("demographics,", "demo.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi,", "gest.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi_words_sentences,", "sent.", colnames(mcdi_all))

mcdi_all <- mcdi_all %>%
            select(demo.CandID, demo.Visit_label, demo.Gender,
                   starts_with("gest."), starts_with("sent.")) %>%
            separate(demo.Visit_label, into = c(NA, "demo.ideal_age"),
                     sep = "x") %>%
            mutate(demo.ideal_age = as.numeric(gsub("m", "",
                                                    demo.ideal_age))) %>%
            rename(data_id = demo.CandID,
                   age = demo.ideal_age,
                   sex = demo.Gender)

################################################################################
# BCP analysis
################################################################################

# Words and Sentences

# Format BCP data as Wordbank
BCP_WS <- format.sentences(mcdi_all, s_dict_file)

# Score WS based on Wordbank
BCP_WS_scored <- score.WS(BCP_WS)

# Extract ns (instead of perc) and sum to get total inventory size
BCP_WS_scored.n <- BCP_WS_scored[[1]] %>%
                    select(-complexity, -how_use_words,
                           -starts_with("word_")) %>%
                    add_column(inventory = rowSums(.[-(1:2)]))

ggplot(BCP_WS_scored.n, aes(x = age, y = inventory)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess") +
  geom_line(aes(group = data_id), alpha = 0.25) +
  labs(x = "Age (mo.)", y = "Inventory size")

# Words and Gestures
BCP_WG <- format.gestures(mcdi_all, g_dict_file, inventory.only = TRUE)

BCP_WG_scored <- score.WG(BCP_WG)

BCP_WG_asWS <- score.GasS(BCP_WG) %>%
                pivot_wider(c(data_id, age), names_from = category,
                            values_from = sum) %>%
                add_column(inventory = rowSums(.[-(1:2)]))

BCP_both <- bind_rows(BCP_WS_scored.n, BCP_WG_asWS)

#
#
#

library(growthcurver)

table(BCP_both$data_id) %>%
  table() %>%
  cumsum()

ggplot(BCP_both, aes(x = age, y = inventory)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  labs(x = "Age (mo.)", y = "W/S inventory") +
  geom_hline(yintercept = 690) +
  scale_x_continuous(limits = c(10, 40))

multiple <- table(BCP_both$data_id) >= 4
who.multiple <- names(table(BCP_both$data_id)[multiple])

ggplot(filter(BCP_both, data_id %in% who.multiple),
       aes(x = age, y = inventory, color = as.factor(data_id))) +
  geom_line(alpha = 0.5) +
  theme(legend.position = "none")

test <- filter(BCP_both, data_id %in% who.multiple) %>%
          select(data_id, age, inventory)

write_csv(test, "data/test-subjs.csv")

 foo <- approxExtrap(test$age, test$inventory, 0:40) %>%
        as.data.frame()

ggplot(NULL) +
  geom_line(data = test, aes(x = age, y = inventory), size = 2) +
  geom_line(data = foo, aes(x = x, y = y), color = "blue")+
  scale_y_continuous(limits = c(0, 680)) +
  geom_hline(yintercept = 680)



