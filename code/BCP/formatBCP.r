if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Libraries

library(tidyverse)
library(readxl)
library(Hmisc)

# Helper functions

source("wordbank-functions.R")
source("format-BCP-funcs.R")

# Read data

mcdi.date <- "200609"

mcdi_all1 <- read_csv(paste0("data/bcp-UMNUNC-mcdi-", mcdi.date, ".csv"))
s_dict_file <- "data/WS-example.csv"
g_dict_file <- "data/WG-example.csv"

# Clean up mcdi_all
colnames(mcdi_all1) <- gsub("demographics,", "demo.", colnames(mcdi_all1))
colnames(mcdi_all1) <- gsub("mcdi,", "gest.", colnames(mcdi_all1))
colnames(mcdi_all1) <- gsub("mcdi_words_sentences,", "sent.",
                            colnames(mcdi_all1))

# Note that "Gender" was changed to "Sex" in spring 2020, may not work with
# older datasets

if ("demo.Gender" %in% colnames(mcdi_all1)) {
  .sex_column <- "demo.Gender"
} else if ("demo.Sex" %in% colnames(mcdi_all1)) {
  .sex_column <- "demo.Sex"
} else {
  stop("No sex/gender column in data")
}

mcdi_all <- mcdi_all1 %>%
            select(demo.CandID, demo.Visit_label, all_of(.sex_column),
                   starts_with("gest."), starts_with("sent.")) %>%
            filter(grepl("^bcp[ABCDEG]", demo.Visit_label),
                   !(demo.Visit_label %in% c("bcpCFP", "bcpGUESTxV1")),
                   !grepl("Biomom", demo.Visit_label)) %>%
            separate(demo.Visit_label,
                     into = c(NA, "demo.ideal_age"),
                     sep = "x") %>%
            mutate(demo.ideal_age = as.numeric(gsub("m", "",
                                                    demo.ideal_age))) %>%
            dplyr::rename(data_id = demo.CandID,
                           age = demo.ideal_age,
                           sex = .sex_column) %>%
            mutate_at(vars(ends_with("morphemes")), as.numeric) %>%
            mutate_at(vars(ends_with("words")), as.numeric) %>%
            mutate(sent.Candidate_Age = as.numeric(sent.Candidate_Age))

################################################################################
# BCP analysis
################################################################################

# Words and Sentences

# Format BCP data as Wordbank
BCP_WS <- mcdi_all %>%
            select(-starts_with("gest"),
                   -ends_with("morphemes"), -ends_with("words")) %>%
            format.sentences(., s_dict_file)

# Score WS based on Wordbank
BCP_WS_scored <- score.WS(BCP_WS)

# Calculate MLU3
MLU3 <- mcdi_all %>%
          select(data_id, sent.Candidate_Age,
                 ends_with("morphemes"), ends_with("words")) %>%
          rename(age = sent.Candidate_Age) %>%
          na.omit() %>%
          mutate(age = as.numeric(age),
                  MLU3m = rowMeans(select(., ends_with("morphemes")),
                                  na.rm = TRUE),
                  MLU3w = rowMeans(select(., ends_with("words")), na.rm = TRUE))

ggplot(MLU3, aes(x = MLU3w, y = MLU3m, color = age)) +
  geom_point() +
  geom_line(aes(group = data_id), alpha = 0.25) +
  scale_color_viridis_c()

cor(MLU3$MLU3m, MLU3$MLU3w) # 0.99


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


saveRDS(BCP_WG_scored, file = paste0("data/BCP_WG_scored-", mcdi.date, ".rds"))
saveRDS(BCP_WG_asWS,   file = paste0("data/BCP_WG_asWS-",   mcdi.date, ".rds"))
saveRDS(BCP_WS_scored, file = paste0("data/BCP_WS_scored-", mcdi.date, ".rds"))
saveRDS(MLU3,          file = paste0("data/BCP_WS_MLU3-",   mcdi.date, ".rds"))

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
