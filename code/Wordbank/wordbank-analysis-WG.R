if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)
library(tidyselect)
library(psych)
library(viridis)
library(lavaan)
library(corrplot)

# psychometric loads MASS; which masks select()
select <- dplyr::select

source("wordbank-csv2rds.R")
source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

mean(WG.demo$age)  # 13.8
sd(WG.demo$age)    #  2.34

table.sex <- table(WG.demo$sex, useNA = "always")
sex.missing <- table.sex[length(table.sex)]
table.sex / (nrow(WG.demo) - sex.missing)

table.mom <- table(WG.demo$mom_ed, useNA = "always")
mom.missing <- table.mom[length(table.mom)]
table.mom / (nrow(WG.demo) - mom.missing)

################################################################################
# Score per Part I.D subcategory
# For type = word, sum 'produce' over all
################################################################################

# This function tests for the existence of this RDS in the data/ directory
# If it doesn't exist, it returns NA
datafile <- .data("Wordbank/WG-scored.rds")
if (file.exists(datafile)) {
  scored <- readRDS(datafile)
} else {
  # Score W&G using developed function
  # 1: n in category per subject
  # 2: %
  scored <- score.WG(WG)
  save_data(scored, filename = "Wordbank/WG-scored.rds")
}

scored_p <- scored$p %>%
  select(-LEXICAL, -SYNTAX)

# Corrplot
words.corr <- scored_p %>%
  select(-data_id, -age) %>%
  cor()

corrplot(words.corr, method = "color",
         is.corr = FALSE,
         order = "hclust", addrect = 2,
         col = rev(rainbow(100)),
         tl.pos = "l", tl.col = "black",
         cl.lim = 0:1)

################################################################################
# Split half
################################################################################

# Zip code for UMN
set.seed(55455)

## Mom ed to numeric
mom_ed.lut <- tibble(mom_ed = c("Some Secondary", "Secondary", "College",
                                "Some College", "Primary", "Graduate",
                                "Some Graduate"),
                     mom_ed_n = c(9, 12, 16, 14, 6, 20, 18))

birth_ord.lut <- tibble(birth_order = levels(WG.demo$birth_order),
                        birth_order_n = 1:8)

# We need to do the split-half with sex/mom ed present to balance them
efa.demo <- WG.demo %>%
  # Create explicit NA category for all demo variables
  mutate(across(sex:ethnicity, ~fct_explicit_na(.x, na_level = "Missing"))) %>%
  group_by(age, sex, mom_ed) %>%
  # Update to use new selector
  sample_frac(.5) %>%
  left_join(mom_ed.lut) %>%
  left_join(birth_ord.lut)

efa.half.ID <- efa.demo$data_id
cfa.half.ID <- WG.demo$data_id[!(WG.demo$data_id %in% efa.demo$data_id)]

# Descriptive statistics
mean(efa.demo$age, na.rm = TRUE) ; sd(efa.demo$age)

(table(efa.demo$sex, useNA = "a") / nrow(efa.demo) * 100) %>%
  round()
(table(efa.demo$ethnicity, useNA = "a") / nrow(efa.demo) * 100) %>%
  round()

mean(efa.demo$mom_ed_n, na.rm = TRUE) ; sd(efa.demo$mom_ed_n, na.rm = TRUE)
mean(efa.demo$birth_order_n, na.rm = TRUE)

(table(efa.demo$birth_order, useNA = "a") / nrow(efa.demo) * 100) %>%
  round()

cfa.demo <- WG.demo %>%
  filter(data_id %in% cfa.half.ID) %>%
  # Create explicit NA category for all demo variables
  mutate(across(sex:ethnicity, ~fct_explicit_na(.x, na_level = "Missing"))) %>%
  left_join(mom_ed.lut) %>%
  left_join(birth_ord.lut)

# Descriptive statistics
mean(cfa.demo$age, na.rm = TRUE) ; sd(cfa.demo$age)

(table(cfa.demo$sex, useNA = "a") / nrow(cfa.demo) * 100) %>%
  round()
(table(cfa.demo$ethnicity, useNA = "a") / nrow(cfa.demo) * 100) %>%
  round()

mean(cfa.demo$mom_ed_n, na.rm = TRUE) ; sd(cfa.demo$mom_ed_n, na.rm = TRUE)
mean(cfa.demo$birth_order_n, na.rm = TRUE)

(table(cfa.demo$birth_order, useNA = "a") / nrow(cfa.demo) * 100) %>%
  round()

efa.half <- filter(scored_p, data_id %in% efa.demo$data_id)
cfa.half <- filter(scored_p, data_id %in% cfa.demo$data_id)

################################################################################
# EFA
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- .data("FA1000-WG.RDS")
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(efa.half, factors = x,
                                       rotate = "oblimin"))

  # and save it
  saveRDS(factor.analyses, file = FA1000)

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("plots/WG-factors", i, ".png"), width = 500, height = 500)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
x <- fa.parallel(efa.half[, -1], fa = "fa")

#
# Plot 3-factor solution 1+3 against 2
#

wgl <- pivot_longer(efa.half, -c(data_id, age))

quantile(wgl$value, probs = seq(0, 1, by = 0.1))

wgl <- wgl %>%
        mutate(trunc_val = replace(value, value > 4/15, 4/15))

ggplot(wgl, aes(x = name, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

values <- factor.analyses[[3]]$scores %>%
            as_tibble() %>%
            mutate(lexical = (MR1 + MR3) / 2)

values.demo <- cbind.data.frame(efa.demo, values)

ggplot(values.demo, aes(x = lexical, y = MR2, color = age)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1,
              alpha = 0.5) +
  scale_color_viridis() +
  geom_smooth(method = "lm", formula = y ~ exp(x))


ggplot(words.lexsym, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1,
              alpha = 0.5) +
  scale_color_viridis() +
  geom_smooth(method = "lm", formula = y ~ exp(x)) +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(0, 0.5))

## Export to CSV for easy import to draft

factor.analyses[[2]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = .data("WG-FA2-loadings.csv"))

factor.analyses[[3]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = .data("WG-FA3-loadings.csv"))

#
# CFA
#

# fa_2fac <- fa(select(efa.half, -data_id), 2, rotate = "Promax",
#               weight = NULL)
# model_2fac <- structure.diagram(fa_2fac, cut = 0.4, errors = TRUE)

nobs <- list(nrow(cfa.half), nrow(efa.half))

model_2fac <- noquote(c("MR1 =~ + sounds + animals + vehicles + toys + food_drink + clothing + body_parts + household + outside + people + games_routines",
                        "MR2 =~ + furniture_rooms + household + outside + action_words + time_words + descriptive_words + pronouns + question_words + locations + quantifiers",
                        "MR1 ~~ MR2"))

mcdi.cfa2fac.r <- cfa(model = model_2fac,
                      data = select(cfa.half, -data_id),
                      sample.nobs = nobs,
                      estimator = "MLR")

summary(mcdi.cfa2fac.r, fit.measures = TRUE)

#

model_3fac <- noquote(c("MR1 =~ + animals + vehicles + food_drink + clothing + body_parts + furniture_rooms + household + outside + action_words + time_words + descriptive_words + locations + quantifiers",
                        "MR2 =~ + sounds + animals + toys + people + games_routines + pronouns",
                        "MR3 =~ + time_words + question_words",
                        "MR3 ~~ MR2"))


mcdi.cfa3fac.r <- cfa(model = model_3fac,
                      data = select(cfa.half, -data_id),
                      sample.nobs = nobs,
                      estimator = "MLR")

summary(mcdi.cfa3fac.r, fit.measures = TRUE)

################################################################################

scored_p_long <- scored_p %>%
  pivot_longer(-c(data_id, age))

threshold <- function(long, thresh) {

  long %>%
    mutate(
      thresh = thresh,
      above = value > thresh
    ) %>%
    filter(above) %>%
    group_by(above, thresh) %>%
    summarize(
      n = n()
    ) %>%
    return()

}

at_thresholds <- lapply(seq(.1, .9, by = .1),
                        function(x) threshold(scored_p_long, x)) %>%
  data.table::rbindlist() %>%
  ungroup() %>%
  mutate(
    p = n / prod(dim(scored_n))
  )

scored_p_long_old <- scored_p_long %>%
  filter(age > 17)

old <- lapply(seq(.1, .9, by = .1),
              function(x) threshold(scored_p_long_old, x)) %>%
  data.table::rbindlist() %>%
  ungroup() %>%
  mutate(
    p = n / (length(unique(scored_p_long_old$data_id)) * 21)
  )
