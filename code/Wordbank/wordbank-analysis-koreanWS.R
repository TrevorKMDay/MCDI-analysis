setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")

library(tidyverse)
library(psych)
library(lavaan)

source("../mcdi-setup.R")
source("wordbank-functions.R")

ko <- read_data("Wordbank/Wordbank-Korean-WS-210715.csv")

# Extract demographics per kid
ko_demo <- ko %>%
  select(data_id, age, sex, mom_ed) %>%
  distinct()

# Morphology section (1 section with five non yet/sometimes/often Qs)
# Sections:
#   Participle use (pro/nouns)
#   Word ending transformation
#   Connecting word ending
#   Adnomial word ending
#   Past or progressive verb affix
ko_morph <- ko %>%
  filter(
    type == "sentence_structure"
  ) %>%
  mutate(
    score = score.SONy(value) %>%
              replace_na(0)
  ) %>%
  group_by(data_id) %>%
  summarize(
    morph_n = n(),
    morph_score = sum(score)
  ) %>%
  mutate(
    morph_p = morph_score / 10
  )

# Extract complexity score (1 per kid)
ko_complexity <- ko %>%
  filter(
    type == "complexity"
  ) %>%
  mutate(
    value = as.numeric(value)
  ) %>%
  group_by(data_id) %>%
  summarize(
    n = n(),
    sum = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    complexity = sum / (n * 2)
  )

# Score word categories
ko_scored <- ko %>%
  filter(
    type == "word"
  ) %>%
  mutate(
    value_numeric = score.produces(value)
  ) %>%
  group_by(data_id, category) %>%
  summarize(
    n = n(),
    sum = sum(value_numeric)
  ) %>%
  mutate(
    perc = sum / n
  )

# Number of items endorsed per section
ko_scored_n <- pivot_wider(ko_scored,
                           id_cols = data_id, names_from = category,
                           values_from = sum) %>%
  left_join(select(ko_complexity, data_id, sum)) %>%
  left_join(select(ko_morph, data_id, morph_score)) %>%
  rename(
    complexity = sum
  )

# Percent of category endorsed per section
ko_scored_p <- pivot_wider(ko_scored,
                           id_cols = data_id, names_from = category,
                           values_from = perc) %>%
  left_join(select(ko_complexity, data_id, complexity)) %>%
  left_join(select(ko_morph, data_id, morph_p))

ko_scored <- list(n = ko_scored_n, p = ko_scored_p)
saveRDS(ko_scored, .data("Wordbank/korean-WS-scored.rds"))

################################################################################

set.seed(55455)

efa1 <- WS.merge1 <- ko_demo %>%
  group_by(age, sex, mom_ed) %>%
  sample_frac(size = .5)

efa.half <- ko_scored_p %>%
  filter(
    data_id %in% efa1$data_id
  ) %>%
  ungroup()

cfa.half <- ko_scored_p %>%
  filter(
    !(data_id %in% efa1$data_id)
  ) %>%
  ungroup()

factor.analyses <- lapply(1:5, function(x) fa(select(efa.half, -data_id),
                                              nfactors = x))

for (i in 1:5) print( fa.diagram(factor.analyses[[i]]) )

fa.parallel.plot <- efa.half %>%
  select(-data_id) %>%
  fa.parallel(fa = "fa")

fa_2fac <- fa(select(efa.half, -data_id),
              2, rotate = "Promax",  weight = NULL)
model_2fac <- structure.diagram(fa_2fac, cut = 0.6, errors = TRUE)

# nobs <- list(nrow(cfa.half), nrow(efa.half))
#
# ko_cfa1 <- cfa(model = model_2fac$lavaan,
#                 data =  select(cfa.half, -data_id),
#                 sample.nobs = nobs)
#
# summary(mcdi.cfa.1, fit.measures = TRUE)

ko_cfa1r <- cfa(model = model_2fac$lavaan,
                data = select(cfa.half, -c(data_id)),
                estimator = "MLR",
                sample.nobs = nobs)

summary(ko_cfa1r, fit.measures = TRUE)

# Modification indices


mix <- modificationIndices(ko_cfa1r, sort. = TRUE)
# Complexity and morph have an MI of 605

model_2fac_2 <- c(model_2fac$lavaan, "complexity ~~ morph_p")

ko_cfa1r_2 <- cfa(model = model_2fac_2,
                  data = select(cfa.half, -c(data_id)),
                  estimator = "MLR",
                  sample.nobs = nobs)

summary(ko_cfa1r_2, fit.measures = TRUE)
mix2 <- modificationIndices(ko_cfa1r_2, sort. = TRUE)
