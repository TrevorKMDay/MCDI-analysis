path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)
library(patchwork)

source("../mcdi-setup.R")

###############################################################################

# WS 1A inventory category labels
WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")

lex_cols <- WS_1A[1:15]
syn_cols <- c(WS_1A[16:22], WS_II)

###############################################################################
# Read in data

# In the following steps, use the vectors defined above to create accurate
# counts based on missing data (any_of). In this case, the syntax total is only
# COMPLEXITY due to missing data

# Read in cleaned EIRLI data with imputations for missing categories
eirli <- read_data("EIRLI/EIRLI_clean.rds") %>%
  mutate(

    inventory_total = select(., any_of(WS_1A)) %>%
      rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
      rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
      rowSums(),

    status = case_when(
      !follow_up      ~ "no_follow_up",
      follow_up &  dx ~ "lg_dx",
      follow_up & !dx ~ "no_lg_dx"
    ),

    proj = "EIRLI"

  ) %>%
  rename(
    sex = gender
  ) %>%
  select(proj, data_id, age, exact_age, sex, status, ends_with("_ed"),
         any_of(WS_1A), COMPLEXITY, ends_with("_total"))

bcp_ws <- read_data("BCP/BCP_WS_scored-200609.rds")$n

bcp <- bcp_ws %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  select(-outside, -places, -time_words, -starts_with("WORD_"), -LEXICAL,
         -SYNTAX, -TOTAL) %>%
  mutate(
    inventory_total = select(., any_of(WS_1A)) %>%
      rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
      rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
      rowSums(),
    status = "BCP"
  )

rm(bcp_ws)

wb <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(-outside, -places, -time_words, -starts_with("WORD_"), -LEXICAL,
         -SYNTAX) %>%
  mutate(
    inventory_total = select(., any_of(WS_1A)) %>%
      rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
      rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
      rowSums(),
    status = "WB"
  )

all_dat <- bind_rows(bcp, eirli, wb) %>%
  select(status, data_id, age, inventory_total) %>%
  na.omit()

ggplot(all_dat, aes(x = age, y = inventory_total)) +
  geom_jitter(width = .3, alpha = 0.1) +
  geom_smooth(aes(color = status), method = "loess", se = FALSE, size = 1.5) +
  theme_bw()
