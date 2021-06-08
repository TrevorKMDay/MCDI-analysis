if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/EIRLI")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/EIRLI")
}

library(tidyverse)
library(psych)
library(lavaan)

source("../mcdi-setup.R")
source("wordbank-functions.R")
max_size <- c(566, 221)

# wg_as_ws <- readRDS(.data("Wordbank/WG_as_WS-scored.rds"))$p
ws       <- readRDS(.data("Wordbank/WS-scored.rds"))$p
efa_half <- read_data("Wordbank/efa_half.csv")
cfa_half <- read_data("Wordbank/cfa_half.csv")

################################################################################

eirli_wide <- read_data("EIRLI/EIRLI_clean.rds")

################################################################################
# Score

eirli_dataonly <- eirli_wide %>%
  rename(
    complexity = COMPLEXITY
  ) %>%
  select(-(1:13))

cat_maxes <- apply(eirli_dataonly, 2, max)

# Calculate as percentage
eirli_wide_p <- eirli_dataonly %>%
  as.matrix() %>%
  apply(1, function(x) x / cat_maxes) %>%
  t() %>%
  as_tibble()

################################################################################

eirli_fa <- fa(eirli_wide_p, nfactors = 2)

summary(eirli_fa, fit.measures = TRUE)

png("eirli_2fa.png", width = 8, height = 6, units = "in", res = 300)
fa.diagram(eirli_fa)
dev.off()

eirli_lex <- c("body_parts", "vehicles", "sounds", "animals", "games_routines",
               "toys", "food_drink", "household", "clothing", "furniture_rooms",
               "action_words", "descriptive_words", "people")

eirli_syn <- colnames(eirli_dataonly)[!(colnames(eirli_dataonly) %in% eirli_lex)]

wb_lex <- c(eirli_lex, "outside", "places")
wb_syn <- c(eirli_syn, "time_words", "WORD_FORMS_NOUNS", "WORD_FORMS_VERBS",
            "WORD_ENDINGS_NOUNS", "WORD_ENDINGS_VERBS") %>%
  tolower()

################################################################################

# Remove categories missing from full data
#    4 morph categories (WORD_*_NOUNS/VERBS)
#  + 3 categories: outside, places, time words
#  ---
#    7 indicators missing
efa <- ws %>%
  select(-starts_with("WORD_"), -outside, -places, -time_words) %>%
  filter(
    data_id %in% efa_half$data_id
  ) %>%
  arrange(match(data_id, efa_half$data_id))

cfa <- ws %>%
  select(-starts_with("WORD_"), -outside, -places, -time_words) %>%
  filter(
    !(data_id %in% efa_half$data_id)
  ) %>%
  arrange(match(data_id, cfa_half$data_id))

################################################################################

FA1000 <- .data("Wordbank/small_FA1000.rds")

# Load in big FA
big_efa <- read_data("results/FA1000-WS-050.RDS")[[2]]
big_cfa <- read_data("results/wordbank-cfa-robust.rds")

if (!file.exists(FA1000)) {

  small_fa <- efa %>%
    select(-data_id, -age, -LEXICAL, -SYNTAX) %>%
    fa(nfactors = 2, n.iter = 1000, rotate = "Promax", weight = NULL)

  saveRDS(small_fa, FA1000)

} else{

  small_fa <- readRDS(FA1000)

}

# cfa() won't take the bootstrapped intervals included in fa() w/ n.iter > 1
small_fa_1iter <- efa %>%
  select(-data_id, -age, -LEXICAL, -SYNTAX) %>%
  fa(nfactors = 2, n.iter = 1, rotate = "Promax", weight = NULL)

fa.diagram(small_fa)
fa.diagram(small_fa_1iter)

# Use this function to generate lavaan model
small_model <- structure.diagram(small_fa_1iter, cut = 0.5, errors = TRUE)

small_cfa <- cfa(model = small_model$lavaan,
                 data  = select(cfa, -data_id, -age),
                 sample.nobs = list(nrow(efa), nrow(cfa)),
                 # robust indicator
                 estimator = "MLR")

summary(small_cfa, fit.measures = TRUE)
summary(big_cfa,   fit.measures = TRUE)

eirli_wide_p2 <- eirli_wide_p %>%
  rename(COMPLEXITY = complexity)

eirli_cfa <- cfa(model = small_model$lavaan,
                  data = eirli_wide_p2,
                  sample.nobs = nrow(eirli_wide_p),
                  # robust indicator
                  estimator = "MLR")


summary(eirli_cfa, fit.measures = TRUE)

scores <- tibble(
    data_id = efa$data_id,
    big_1 = big_efa$scores[, 1],
    big_2 = big_efa$scores[, 2],
    small_1 = small_fa$scores[, 1],
    small_2 = small_fa$scores[, 2]
  ) %>%
  pivot_longer(-data_id, names_to = c("fa", "fac"), names_sep = "_",
               names_transform = list(fac = as.integer)) %>%
  pivot_wider(c(data_id, fa), values_from = value, names_from = fac,
              names_prefix = "fa_")

# %>%
#   pivot_wider(c(data_id, fac), values_from = value, names_from = fa,
#               names_prefix = "fa_")

png("big_vs_small-scores.png", width = 5, height = 4, units = "in", res = 300)

ggplot(scores, aes(x = fa_1, y = fa_2, color = fa)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(x = "Lexical factor", y = "Structural factor", color = "FA") +
  theme_bw()

dev.off()

################################################################################
# correlations

cfa_cor <- readRDS(.data("Wordbank/WS-scored.rds"))$n %>%
  select(-LEXICAL, -SYNTAX) %>%
  select_all(~tolower(.)) %>%
  filter(
    data_id %in% cfa_half$data_id
  ) %>%
  arrange(match(data_id, cfa_half$data_id)) %>%
  mutate(
    eirli_lex_sum = select(., all_of(eirli_lex)) %>%
                      rowSums(),
    eirli_syn_sum = select(., all_of(eirli_syn)) %>%
                      rowSums(),
    wb_lex_sum = select(., all_of(wb_lex)) %>%
                  rowSums(),
    wb_syn_sum = select(., all_of(wb_syn)) %>%
                  rowSums(),
  ) %>%
  select(data_id, age, ends_with("sum"))

cfa_cor %>%
  select(-data_id, -age) %>%
  cor()

cfa_cor_nest <- cfa_cor %>%
  group_by(age) %>%
  nest() %>%
  mutate(
    cor = map(data, ~cor(select(.x, -data_id)), 3),
    lex = map_dbl(cor, ~.x[1, 3]),
    syn = map_dbl(cor, ~.x[2, 4])
  ) %>%
  select(-data, -cor) %>%
  pivot_longer(-age)

min(cfa_cor_nest$value[cfa_cor_nest$name == "lex"])
min(cfa_cor_nest$value[cfa_cor_nest$name == "syn"])


png("rs.png", width = 5, height = 4, units = "in", res = 300)

ggplot(cfa_cor_nest, aes(x = age, y = value, color = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(.9, 1)) +
  theme_bw()

dev.off()
