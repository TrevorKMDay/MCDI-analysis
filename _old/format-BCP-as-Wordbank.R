if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis")
}

library(tidyverse)
library(readxl)
library(psych)
library(viridis)

source("wordbank-functions.R")
names <- read_excel("data/mcdi-sections.xlsx", sheet = 1)

# mcdi_all <- read_csv("bcp-mcdi-103019.csv")
# mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-191112.csv")
mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-200131.csv")

educ_raw <- read_csv("data/BCP-education-191121.csv")
colnames(educ_raw) <- c("Identifiers", "CandID", "p1.educ", "p1.rel", "p2.educ",
                    "p2.rel")

educ <- educ_raw %>%
          separate(Identifiers, into = c(NA, "age"), sep = "x") %>%
          filter(p1.educ != ".") %>%
          mutate(age = as.numeric(gsub("m", "", age))) %>%
          mutate(p1.rel = replace(p1.rel, p1.rel == "non_specified", NA),
                 p2.rel = replace(p2.rel, grep("^no[nt]_", p2.rel), NA)) %>%
          mutate_if(is_character, as_factor)

educ.p1 <- educ %>%
            select(CandID, age, p1.rel, p1.educ) %>%
            drop_na() %>%
            pivot_wider(c(CandID, age),
                        names_from = "p1.rel", values_from = "p1.educ")

educ.p2 <- educ %>%
              select(CandID, age, p2.rel, p2.educ) %>%
              drop_na() %>%
              pivot_wider(c(CandID, age),
                          names_from = "p2.rel", values_from = "p2.educ")

educ2 <- left_join(select(educ.p1, CandID, age, contains("mother")),
                   select(educ.p2, CandID, age, contains("mother")),
                          by = c("CandID", "age"))

# Mom is usually parent1
educ2$mom_ed <- educ2$biological_mother.x

# Replace missing with parent2
educ2$mom_ed[is.na(educ2$mom_ed)] <- educ2$biological_mother.y[is.na(educ2$mom_ed)]

# Finally, use nonbio mom (in case of single father)
educ2$mom_ed[is.na(educ2$mom_ed)] <- educ2$nonbio_mother[is.na(educ2$mom_ed)]

educ3 <- educ2 %>%
          select(CandID, age, mom_ed) %>%
          mutate(mom_ed = replace(mom_ed, mom_ed == "not_answered", NA))

# As of writing, no one reported primary
levels(educ3$mom_ed) <- c("College", "Graduate", "Some Graduate",
                           "Some College", "Not Answered", "Some Secondary",
                          "Secondary")

#
# Clean up MCDI
#

# Remove some column prefixes
colnames(mcdi_all) <- gsub("demographics,", "demo.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi,", "gestures.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi_words_sentences,", "sentences.",
                           colnames(mcdi_all))

# Separate into wide format

mcdi_all <- mcdi_all %>%
              separate(demo.Visit_label, into = c(NA, "demo.age_bin"),
                       sep = "x") %>%
              mutate(demo.age_bin = suppressWarnings(as.numeric(gsub("m", "",
                                                                     demo.age_bin))))
gest <- mcdi_all %>%
              select(demo.CandID, demo.age_bin, demo.Gender,
                     starts_with("gestures")) %>%
              add_column(demo.age = suppressWarnings(as.numeric(.$gestures.Candidate_Age)),
                         .after = "demo.age_bin") %>%
              filter(gestures.Administration == "All")

sent <- mcdi_all %>%
          select(demo.CandID, demo.age_bin, demo.Gender,
                 starts_with("sentences")) %>%
          add_column(demo.age = suppressWarnings(as.numeric(.$sentences.Candidate_Age)),
                     .after = "demo.age_bin") %>%
          filter(sentences.Administration == "All") %>%
          mutate(age_diff = demo.age_bin - demo.age)

### Format gestures like Wordbank

gest.cat.ord <- c("sounds", "animals", "vehicles", "toys", "food_drink",
                  "clothing", "body_parts", "furniture_rooms", "household",
                  "outside", "people", "games_routines", "action_words",
                  "time_words", "descriptive_words", "pronouns",
                  "question_words", "locations", "quantifiers")

gest.ord <- read_csv("g_dict.csv")

get.value <- function(ord, cat, i) {

  filter(ord, category == as.character(cat))$definition[as.numeric(i)] %>%
    return()

}

gest.WB.ID <- gest %>%
                select(starts_with("demo"), starts_with("gestures.I"),
                       -ends_with("score"), -contains("replacement")) %>%
                select_all(~gsub("gestures.", "", .)) %>%
                pivot_longer(-c(demo.CandID, demo.age_bin, demo.age,
                                demo.Gender)) %>%
                rename(data_id = demo.CandID,
                       age_ideal = demo.age_bin,
                       age = demo.age,
                       sex = demo.Gender) %>%
                filter(grepl("^I_D", name, .)) %>%
                separate(name, into = c(NA, NA, "category", "question"),
                         convert = TRUE) %>%
                arrange(data_id, category, question) %>%
                mutate(category = gest.cat.ord[category],
                       value = replace(value, value == "not_answered", NA),
                       value = replace(value,
                                       value == "says_and_understands",
                                       "produces"),
                       value = as_factor(value)) %>%
                select(data_id, age, age_ideal, sex, value, category,
                       question) %>%
                add_column(mom_ed = NA, .after = "sex") %>%
                add_column(type = "word", .before = "category") %>%
                add_column(item_id = NA, .after = "value")

gest.WB.ID$definition <- select(gest.WB.ID,
                                category, question) %>%
                          apply(1, function(x) get.value(gest.ord, cat = x[1],
                                                         i = x[2]))

# Get Part I, section D (lexical inventory)
# Organize the other parts later
gest.WB.ID <- select(gest.WB.ID, -question) %>%
                mutate_if(is_character, as_factor)

saveRDS(gest.WB.ID, "data/bcp-UMNUNC-mcdiG-191112.rds")

#
# Score UMN/UNC BCP W/G as if it were W/S
#

BCP.gest.sc <- score.GasS(gest.WB.ID)
BCP.gest.sc.w <- BCP.gest.sc %>%
                  select(-n, -sum) %>%
                  pivot_wider(id_cols = c(data_id, age),
                              values_from = "perc", names_from = "category") %>%
                  add_column(instrument = "G", .after = "age") %>%
                  mutate(connecting_words = 0,
                         WORD_FORMS_NOUNS = 0,
                         WORD_FORMS_VERBS = 0,
                         WORD_ENDINGS_NOUNS = 0,
                         WORD_ENDINGS_VERBS = 0,
                         COMPLEXITY = 0)

                  # mutate(connecting_words = rnorm(n(), 0.5, 0.25),
                  #        WORD_FORMS_NOUNS = rnorm(n(), 0.5, 0.25),
                  #        WORD_FORMS_VERBS = rnorm(n(), 0.5, 0.25),
                  #        WORD_ENDINGS_NOUNS = rnorm(n(), 0.5, 0.25),
                  #        WORD_ENDINGS_VERBS = rnorm(n(), 0.5, 0.25),
                  #        COMPLEXITY = rnorm(n(), 0.5, 0.25)) %>%

# Read factor analyses from file
factor.analyses <- readRDS("FA1000.rds")

BCP.GasS.est <- factor.scores(BCP.gest.sc.w[, -(1:3)],
                              factor.analyses[[2]],
                              method = "Bartlett",
                              impute = "mean")

BCP.GasS.sc <- cbind(BCP.gest.sc.w[, 1:2], BCP.GasS.est$scores)

ggplot(BCP.GasS.sc, aes(x = MR1, y = MR2, color = age)) +
  geom_point() +
  scale_color_viridis() +
  geom_abline() +
  geom_smooth() +
  labs(x = "Lexical", y = "Syntactic", color = "Age (mo).",
       title = "Sentences factors applied to BCP gestures data treated as sentences")

#
# Format sentences as Wordbank
#

# Part I A

sent.cat.ord <- c("sounds", "animals", "vehicles", "toys", "food_drink",
                  "clothing", "body_parts", "household", "furniture_rooms",
                  "outside", "places", "people", "games_routines",
                  "action_words", "descriptive_words", "time_words",
                  "pronouns", "question_words", "locations", "quantifiers",
                  "helping_verbs", "connecting_words")

sent.WB.IA <- sent %>%
                select(starts_with("demo"), starts_with("sentences.I"),
                       -ends_with("score"), -contains("replacement")) %>%
                select_all(~gsub("sentences.", "", .)) %>%
                pivot_longer(-c(demo.CandID, demo.age_bin, demo.age,
                                demo.Gender)) %>%
                rename(data_id = demo.CandID,
                       age_ideal = demo.age_bin,
                       age = demo.age,
                       sex = demo.Gender) %>%
                filter(grepl("^I_A", name, .)) %>%
                separate(name, into = c(NA, NA, "category", "question"),
                         convert = TRUE) %>%
                arrange(data_id, category, question) %>%
                mutate(category = sent.cat.ord[category],
                       value = replace(value, value == "says", "produces"),
                       value = as_factor(value)) %>%
                select(data_id, age, age_ideal, sex, value, category,
                       question) %>%
                add_column(mom_ed = NA, .after = "sex") %>%
                add_column(type = "word", .before = "category") %>%
                add_column(item_id = NA, .after = "value")

sent.ord <- read_csv("s_dict.csv")

sent.WB.IA$definition <- select(sent.WB.IA,
                                category, question) %>%
                            apply(1, function(x) get.value(sent.ord,
                                                           cat = x[1],
                                                           i = x[2]))

# Get Part I, section A (lexical inventory)
# Organize the other parts later
sent.WB.IA <- select(sent.WB.IA, -question) %>%
                mutate_if(is_character, as_factor)

# Part I B (how use words)

# Part II
sent.WB.IIB <- sent %>%
                select(starts_with("demo"), starts_with("sentences.I"),
                       -ends_with("score"), -contains("replacement")) %>%
                select_all(~gsub("sentences.", "", .)) %>%
                pivot_longer(-c(demo.CandID, demo.age_bin, demo.age,
                                demo.Gender)) %>%
                rename(data_id = demo.CandID,
                       age_ideal = demo.age_bin,
                       age = demo.age,
                       sex = demo.Gender) %>%
                filter(grepl("^II_B", name, .)) %>%
                separate(name, into = c(NA, "category", "question"),
                         convert = TRUE)%>%
                arrange(data_id, category, question) %>%
                select(data_id, age, age_ideal, sex, value,
                       question) %>%
                add_column(mom_ed = NA, .after = "sex") %>%
                add_column(type = NA, .before = "question") %>%
                  mutate(type = if_else(question <= 5, "word_forms_nouns",
                                        "word_forms_verbs"),
                         value = replace(value, value == "says",
                                         "produces")) %>%
                add_column(item_id = NA, .after = "value") %>%
                add_column(category = NA, .after = "type") %>%
                rename(definition = question) %>%
                mutate(definition = as.character(definition))

sent.WB.IIC <- sent %>%
                  select(starts_with("demo"), starts_with("sentences.I"),
                         -ends_with("score"), -contains("replacement")) %>%
                  select_all(~gsub("sentences.", "", .)) %>%
                  pivot_longer(-c(demo.CandID, demo.age_bin, demo.age,
                                  demo.Gender)) %>%
                  rename(data_id = demo.CandID,
                         age_ideal = demo.age_bin,
                         age = demo.age,
                         sex = demo.Gender) %>%
                  filter(grepl("^II_B", name, .)) %>%
                  separate(name, into = c(NA, "category", "question"),
                           convert = TRUE)%>%
                  arrange(data_id, category, question) %>%
                  select(data_id, age, age_ideal, sex, value,
                         question) %>%
                  add_column(mom_ed = NA, .after = "sex") %>%
                  add_column(type = NA, .before = "question") %>%
                  add_column(category = NA, .after = "type") %>%
                  mutate(type = if_else(question <= 14, "word_endings_nouns",
                                        "word_endings_verbs"),
                         value = replace(value, value == "says",
                                         "produces")) %>%
                  add_column(item_id = NA, .after = "value") %>%
                  rename(definition = question) %>%
                  mutate(definition = as.character(definition))

sent.WB.IIE <- sent %>%
                  select(starts_with("demo"), starts_with("sentences.I"),
                         -ends_with("score"), -contains("replacement")) %>%
                  select_all(~gsub("sentences.", "", .)) %>%
                  pivot_longer(-c(demo.CandID, demo.age_bin, demo.age,
                                  demo.Gender)) %>%
                  rename(data_id = demo.CandID,
                         age_ideal = demo.age_bin,
                         age = demo.age,
                         sex = demo.Gender) %>%
                  filter(grepl("^II_E", name, .)) %>%
                  separate(name, into = c(NA, NA, "question"),
                           convert = TRUE)%>%
                  arrange(data_id, question) %>%
                  select(data_id, age, age_ideal, sex, value, question) %>%
                  add_column(mom_ed = NA, .after = "sex") %>%
                  add_column(type = "complexity", .before = "question") %>%
                  add_column(category = NA, .after = "type") %>%
                  mutate(value = if_else(value == "more_complex",
                                         "complex", "simple")) %>%
                  add_column(item_id = NA, .after = "value") %>%
                  rename(definition = question) %>%
                  mutate(definition = as.character(definition))

# Merge
sent.WB.IA_IIBCE <- bind_rows(sent.WB.IA, sent.WB.IIB, sent.WB.IIC,
                              sent.WB.IIE)

stop()

BCP.sent.sc <- score.WS(sent.WB.IA_IIBCE)

BCP.sent.sc.w <- BCP.sent.sc %>%
                    pivot_wider(id_cols = c(data_id, age),
                                values_from = "perc",
                                names_from = "category") %>%
                    add_column(instrument = "S", .after = "age") %>%
                    select(data_id, age, instrument,
                           sent.cat.ord,
                           starts_with("W"),
                           COMPLEXITY)

BCP.sc.w <- bind_rows(BCP.sent.sc.w, BCP.gest.sc.w)

BCP.est <- factor.scores(BCP.sc.w[, -(1:3)],
                          factor.analyses[[2]],
                          method = "Bartlett",
                          impute = "mean")

BCP.est.sc <- cbind(BCP.sc.w[, 1:3], BCP.est$scores) %>%
                mutate(agegroup = cut(age, c(0, 18, 30, 60)))

ggplot(BCP.est.sc, aes(x = MR1, y = MR2, color = agegroup)) +
  geom_point(alpha = 0.75, aes(shape = instrument)) +
  scale_shape_manual(values = c(1, 3)) +
  geom_abline() +
  geom_smooth(se = FALSE) +
  labs(x = "Lexical", y = "Syntactic", color = "Age group",
       shape = "Instrument",
       title = "Sentences factors applied to BCP G/S data combined")

#
# Correlation between last gestures and first sentences
#

both <- intersect(BCP.gest.sc.w$data_id, BCP.sent.sc.w$data_id)

a <- filter(BCP.est.sc, data_id %in% both)
a.n <- group_by(a, data_id) %>%
        summarise(n = n())

a.long <- pivot_longer(a, starts_with("MR"))

ggplot(a.long, aes(x = age, y = value)) +
  geom_line(aes(group = data_id), alpha = 0.25) +
  facet_wrap(vars(name), scales = "free")

a.g <- filter(a, instrument == "G") %>%
          group_by(data_id) %>%
          arrange(age) %>%
          filter(age == max(age))

a.s <- filter(a, instrument == "S") %>%
          group_by(data_id) %>%
          arrange(age) %>%
          filter(age == min(age))

b <- full_join(a.g, a.s,
               by = c("data_id"),
               suffix = c(".g", ".s")) %>%
        mutate(diff = age.s - age.g) %>%
        select(-starts_with("instrument")) %>%
        ungroup()

# Mean difference
mean(b$diff) # = 12.9 mo
sd(b$diff)   # =  5.9 mo

cor(b$MR1.g, b$MR1.s)

cor(b$MR2.g, b$MR2.s)

c <- pivot_longer(b, starts_with("MR")) %>%
      separate(name, into = c("factor", "instrument")) %>%
      pivot_wider(names_from = instrument, values_from = value) %>%
      ungroup()

ggplot(c, aes(x = g, y = s, color = diff)) +
  geom_point() +
  scale_color_viridis() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(factor))

ggplot

lm.1 <- lm(s ~ g + diff, data = filter(c, factor == "MR1"))
lm.2 <- lm(s ~ g + diff, data = filter(c, factor == "MR2"))


lm.3 <- lm(s ~ g + diff + age.g, data = filter(c, factor == "MR1"))
lm.4 <- lm(s ~ g + diff + age.g, data = filter(c, factor == "MR2"))

lm.5 <- lm(s ~ g + diff + age.s, data = filter(c, factor == "MR1"))
lm.6 <- lm(s ~ g + diff + age.s, data = filter(c, factor == "MR2"))

lm.7 <- lm(s ~ g + age.g + age.s, data = filter(c, factor == "MR1"))
lm.8 <- lm(s ~ g + age.g + age.s, data = filter(c, factor == "MR2"))

