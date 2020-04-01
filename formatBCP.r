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
# Save data
#

saveRDS(BCP_WG_scored, file = "data/BCP_WG_scored.rds")
saveRDS(BCP_WG_asWS, file = "data/BCP_WG_asWS.rds")
saveRDS(BCP_WS_scored, file = "data/BCP_WS_scored.rds")

# People who have four or more data points

multiple <- table(BCP_both$data_id) >= 4
who.multiple <- names(table(BCP_both$data_id)[multiple])

test <- filter(BCP_both, data_id %in% who.multiple) %>%
          select(data_id, age, inventory)

write_csv(test, "data/test-subjs.csv")

# Score by category

BCP_both.n <- full_join(BCP_WG_asWS, BCP_WS_scored[[1]])

lexsyn <- BCP_both.n %>%
            select(-how_use_words, -word_endings) %>%
            mutate_all(function(x) replace_na(x,0)) %>%
            mutate(age = replace(age, age == 0, NA)) %>%
            mutate(lex = action_words + animals + body_parts + clothing +
                          descriptive_words + food_drink + furniture_rooms +
                          games_routines + helping_verbs + household + 
                          outside + people + places + toys + vehicles,
                   syn = pronouns + quantifiers + question_words + sounds +
                          time_words + word_endings_nouns + word_endings_verbs +
                          word_forms_nouns + word_forms_verbs + complexity +
                          connecting_words + locations,
                   SUM = lex + pronouns + quantifiers + question_words + sounds +
                           time_words + connecting_words + locations) %>%
            select(-everything(), data_id, age, lex, syn, SUM) %>%
            mutate(lex.p = lex / 575, 
                   syn.p = syn / 175)


png("plots/BCP-trends-presentation.png", width = 5.5, height = 5, res = 300,
    units = "in")

ggplot(lexsyn, aes(x = age, y = SUM)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess") +
  geom_line(aes(group = data_id), alpha = 0.25) +
  labs(x = "Age (mo.)", y = "Inventory size") +
  scale_y_continuous(limits = c(0, 680)) +
  scale_x_continuous(limits = c(5, 40)) 

dev.off()

write_csv(lexsyn, "data/lexsyn.csv")
