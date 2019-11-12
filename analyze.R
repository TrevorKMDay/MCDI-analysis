if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis")
}

library(tidyverse)
library(ggplot2)
library(reshape2)
library(fastICA) ; set.seed("03181996")
library(readxl)
library(Hmisc)
library(psych)
library(corrplot)
library(ggrepel)

source("wordbank-functions.R")

mullen <- read_csv("data/bcp-UMN-mullen-191030.csv") %>%
            select_all(~gsub("^.*,", "", .)) %>%
            separate(Visit_label, into = c("x", "age"), sep = "x") %>%
            mutate(age = as.numeric(gsub("m", "", age))) %>%
            select(-x)

# mcdi_all <- read_csv("bcp-mcdi-103019.csv")
mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-191112.csv")

names <- read_excel("mcdi-sections.xlsx", sheet = 1)

lexical <- names$name[1:(22-7)]
syntactic <- names$name[(22-6):22]

#
# Clean up mullen
#

# Remove column prefixes
colnames(mullen) <- gsub("^.*,", "", colnames(mullen))

mullen <- mullen %>%
            mutate_at(vars(contains("language")), as.numeric) %>%
            select_all(~gsub("expressive", "expr", .)) %>%
            select_all(~gsub("receptive", "recv", .)) %>%
            select_all(~gsub("language", "lg", .)) %>%
            select_all(~gsub("age_equivalent", "age", .))

mullen.melt <- mullen %>%
                melt(id.vars = 1:6) %>%
                separate(variable, into = c("dir", "x", "variable"),
                         sep = "_") %>%
                select(-x) %>%
                mutate_at(c("variable", "dir"), as.factor)

#
# Clean up MCDI
#

# Remove some column prefixes
colnames(mcdi_all) <- gsub("demographics,", "demo.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi,", "gestures.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi_words_sentences,", "sentences.",
                           colnames(mcdi_all))

mcdi_all <- mcdi_all %>%
              separate(demo.Visit_label, into = c(NA, "demo.age_bin"),
                       sep = "x") %>%
              mutate(demo.age_bin = suppressWarnings(as.numeric(gsub("m", "",
                                                                     demo.age_bin))))

gestures <- select(mcdi_all, Identifiers, starts_with("demo"),
                        starts_with("gestures")) %>%
                  add_column(demo.age = suppressWarnings(as.numeric(.$gestures.Candidate_Age)),
                              .after = "demo.age_bin") %>%
                  filter(gestures.Administration == "All")

sent <- select(mcdi_all, Identifiers, starts_with("demo"),
                    starts_with("sentences")) %>%
              add_column(demo.age = suppressWarnings(as.numeric(.$sentences.Candidate_Age)),
                         .after = "demo.age_bin") %>%
              filter(sentences.Administration == "All")

################################################################################
################################################################################

# Mullen analysis

# Age-equivalent vs visit age
ggplot(filter(mullen.melt, variable == "age"),
       aes(x = age, y = value, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm")

# Rceptive vs. expressive
ggplot(mullen, aes(x = expr_lg_age, y = recv_lg_age)) +
  geom_point() +
  geom_smooth(method = "lm")

# Expressive predicts receptive, no duh, at B = 0.9, so E lags R (probably
# overreporting on the parent's end)
EvR <- lm(expr_lg_age ~ recv_lg_age, data = mullen)

################################################################################

################################################################################
# Score MCDI WS
################################################################################

# Section I.A

# Reduce to relevant variables
sent2 <- sent %>%
                select(demo.CandID, demo.age_bin, demo.age,
                       starts_with("sentences.I")) %>%
                pivot_longer(-c(demo.CandID, demo.age_bin, demo.age)) %>%
                mutate(name = gsub("sentences.", "", name))

# Now tabulate over sections/subsections
# We're only looking at section A, so filter over than and remove column
# section
sent2.I <- sent2 %>%
                  filter(grepl("^I_", name),
                         !grepl("_score", name)) %>%
                  separate(name, sep = "_",
                           into = c(NA, "section", "subsection", "question"),
                           convert = TRUE) %>%
                  filter(section == "A") %>%
                  select(-section) %>%
                  mutate(produces = score.produces(value, produces = "says"))

# Now group by ID and subsection and count the number of "says" (converted to
# T/F in previous step)
sent2.I.score <- sent2.I %>%
                        group_by(demo.CandID, demo.age_bin, subsection) %>%
                        summarise(n = n(), sum = sum(produces),
                                  perc = sum / n) %>%
                        mutate(subsection = as.factor(subsection))

# Assign labels to subsections
levels(sent2.I.score$subsection) <- c(lexical, syntactic) 

# Spread wide
sent2.I.score <- sent2.I.score %>%
                        pivot_wider(c(demo.CandID, demo.age_bin), 
                                    names_from = subsection, 
                                    values_from = perc) %>%
                        ungroup()

lm(clothing ~ demo.age_bin, data = sent2.I.score)

# Part II

sent2.II <- sent2 %>%
                    filter(grepl("^II_", name),
                           !grepl("_score", name)) %>%
                    separate(name, sep = "_",
                             into = c(NA, "section", "question"),
                             convert = TRUE) %>%
                    filter(section %in% c("B", "C", "E"))

# B/C

sent2.II.BC <- sent2.II %>%
                      filter(section %in% LETTERS[2:3]) %>%
                      mutate(produces = score.produces(value, 
                                                       produces = "says"))

sent2.II.BC.score <- sent2.II.BC %>%
                      group_by(demo.CandID, demo.age_bin) %>%
                      arrange(demo.CandID, demo.age_bin, section, question) 

sent2.II.B.score <- sent2.II.BC.score %>%
                      filter(section == "B") %>%
                      add_column(class = if_else(.$question <= 5, "noun", 
                                                 "verb"), 
                                 .after = "question") %>%
                      group_by(demo.CandID, demo.age_bin, class) %>%
                      summarise(n = n(), sum = sum(produces), 
                                perc = sum / n) %>%
                      pivot_wider(c(demo.CandID, demo.age_bin), 
                                  names_from = class, 
                                  values_from = perc) %>%
                      ungroup() %>%
                      rename(WORD_FORMS_NOUNS = noun,
                             WORD_FORMS_VERBS = verb)

sent2.II.C.score <- sent2.II.BC.score %>%
                      filter(section == "C") %>%
                      add_column(class = if_else(.$question <= 14, "noun", 
                                                 "verb"), 
                                 .after = "question") %>%
                      group_by(demo.CandID, demo.age_bin, class) %>%
                      summarise(n = n(), sum = sum(produces), 
                                perc = sum / n) %>%
                      pivot_wider(c(demo.CandID, demo.age_bin), 
                                  names_from = class, 
                                  values_from = perc) %>%
                      ungroup() %>%
                      rename(WORD_ENDINGS_NOUNS = noun,
                             WORD_ENDINGS_VERBS = verb)

# E

sent2.II.E <- sent2.II %>%
                filter(section == "E") %>%
                mutate(complex = score.complexity(value, cx = "more_complex"))

sent2.II.E.score <- sent2.II.E %>%
                      group_by(demo.CandID, demo.age_bin) %>%
                      summarise(n = n(), cx = sum(complex), 
                                COMPLEXITY = cx / n) %>%
                      select(-n, -cx) %>%
                      ungroup()

# Join together all scores
sent2.score <- left_join(sent2.I.score, sent2.II.B.score) %>%
                left_join(sent2.II.C.score) %>%
                left_join(sent2.II.E.score)

today <- format(Sys.Date(), "%y%m%d")
write_csv(sent2.score, path = paste0("BCP-scores-", today, ".csv"))

# Filter out everyone who is older than 30 months (the typical max age for
# MCDI-WS)
sent2.30mo <- sent2 %>%
                filter(demo.age_bin <= 30)

write_csv(sent2.30mo, path = paste0("BCP-30mo-scores-", today, ".csv"))
