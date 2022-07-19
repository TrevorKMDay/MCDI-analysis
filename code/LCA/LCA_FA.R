path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)
library(viridis)
library(patchwork)
library(psych)

source("../mcdi-setup.R")
source("00-LCA_functions.R")

WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")

# EIRLI is missing some columns, here's the lex cols that DO exist
eirli_cols <- c("action_words", "animals", "body_parts", # "COMPLEXITY",
                "clothing", "connecting_words", "descriptive_words",
                "food_drink", "furniture_rooms", "games_routines",
                "helping_verbs", "people", "locations", "pronouns",
                "quantifiers", "question_words", "household", "sounds",
                "toys", "vehicles")

# Load data ####

BE <- read_data("LCA/BplusE.rds") %>%
  mutate(
    empty_prior = 0
  ) %>%
  ungroup()

BE_long <- BE %>%
  select(proj, status, data_id, age, exact_age, lex_total, syn_total,
         any_of(WS_1A), any_of(WS_II)) %>%
  pivot_longer(-c(proj, status, data_id, age, exact_age, lex_total, syn_total),
               names_to = "category")

# FA ####

wb_ws <- read_data("Wordbank/WS-scored.rds")$n
wb_wg <- read_data("Wordbank/WG_as_WS-scored.rds")$n

wb <- bind_rows(wb_ws, wb_wg) %>%
  select(data_id, age, all_of(eirli_cols))

wb_2fa <- fa(select(wb, -data_id, -age), nfactors = 2, rotate = "oblimin")

# Apply FA separately

BCP_FA <- BE %>%
  filter(
    proj == "BCP"
  ) %>%
  select(eirli_cols) %>%
  factor.scores(f = wb_2fa, method = "Bartlett")

BCP_scores <- BE %>%
  filter(
    proj == "BCP"
  ) %>%
  select(proj, status, data_id, age) %>%
  bind_cols(BCP_FA$scores)

cor(BCP_FA$scores)

EIRLI_FA <- BE %>%
  filter(
    proj == "EIRLI"
  ) %>%
  select(eirli_cols) %>%
  factor.scores(f = wb_2fa, method = "Bartlett")

EIRLI_scores <- BE %>%
  filter(
    proj == "EIRLI"
  ) %>%
  select(proj, status, data_id, age) %>%
  bind_cols(EIRLI_FA$scores)

cor(EIRLI_FA$scores)

scores <- bind_rows(BCP_scores, EIRLI_scores)

ggplot(scores, aes(x = MR1, y = MR2, color = status)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth() +
  theme_bw()
