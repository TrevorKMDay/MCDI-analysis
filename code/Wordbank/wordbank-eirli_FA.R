if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)
library(tidyselect)
library(corrplot)
library(psych)

source("../mcdi-setup.R")
source("wordbank-functions.R")
max_size <- c(566, 221)

# wg_as_ws <- readRDS(.data("Wordbank/WG_as_WS-scored.rds"))$p
ws       <- readRDS(.data("Wordbank/WS-scored.rds"))$p
efa_half <- read_data("Wordbank/efa_half.csv")
cfa_half <- read_data("Wordbank/cfa_half.csv")

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

ggplot(scores, aes(x = fa_1, y = fa_2, color = fa)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))
