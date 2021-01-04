if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(broom)
library(tidyverse)
library(tidyselect)
library(corrr)

# Plotting functions
# Cite if used in final
library(viridis)
library(ggExtra)
library(corrplot)
library(gridExtra)
library(ggrepel)
library(ggridges)

# Substantive packages (to cite)
library(psych)
library(lavaan)
library(psychometric)

# psychometric loads MASS; which masks select()
select <- dplyr::select

# Kyle says Promax is orthogonal, followed by an oblique correction
# oblimin is orthogonal from the get-go, but is more computationally intensive
# (but fine on modern machines)
rotation <- "oblimin"

source("wordbank-csv2rds.R")
source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

WS.demo <- read_data("Wordbank/WS-demographics.rds")

mean(WS.demo$age)
sd(WS.demo$age)

table(WS.demo$sex, useNA = "always")

table(WS.demo$mom_ed, useNA = "always")

ws.mom_ed <- table(WS.demo$mom_ed, useNA = "always")

length(unique(WS.demo$data_id)) == nrow(WS.demo)

################################################################################
# Score per Part I subcategory
# For type = word, sum 'produce' over all
################################################################################

# This function tests for the existence of this RDS in the data/ directory
# If it doesn't exist, it returns NA
datafile <- .data("Wordbank/WS-scored.rds")
if (file.exists(datafile)) {
  scored <- readRDS(datafile)
} else {
  # Score W&S using developed function
  # 1: n in category per subject
  # 2: %
  scored <- score.WS(WS)
  save_data(scored, filename = "Wordbank/WS-scored.rds")
}

################################################################################
# Correlation plot
################################################################################

WS.corrmat <- scored$p %>%
  select(-data_id, -age, -starts_with("W"), -COMPLEXITY) %>%
  cor()

dir.create("plots/", showWarnings = FALSE)
png("plots/corrplot.png", width = 6, height = 5, units = "in", res = 300)

corrplot(WS.corrmat,
         method = "color",                # color instead of circles
         order = "hclust", addrect = 2,   # group in two
         col = rev(rainbow(100)),         # rainbow 0-1, no negative
         tl.pos = "l", tl.col = "black",  # text position
         cl.lim = 0:1)                    # data range 0-1

dev.off()

################################################################################
# Split half
################################################################################

# Zip code for UMN
set.seed(55455)

WS.merge1 <- WS.demo %>%
  # Create explicit NA category for all demo variables
  mutate(across(sex:ethnicity, ~fct_explicit_na(.x, na_level = "Missing"))) %>%
  # Although we later identified birth order and ethnicity, leave original
  # balancing call.
  group_by(age, sex, mom_ed) %>%
  sample_frac(size = .5)

apply(WS.demo, 2, function(x) sum(is.na(x)))


# Everyone in WS.merge1; everyone not
efa.half.ID <- as.character(WS.merge1$data_id)
cfa.half.ID <- scored[[1]]$data_id[!(scored[[1]]$data_id %in% efa.half.ID)]

efa.half <- scored$p %>%
  filter(data_id %in% efa.half.ID)
cfa.half <- scored$p %>%
  filter(data_id %in% cfa.half.ID)

## Mom ed to numeric
mom_ed.lut <- tibble(mom_ed = c("Some Secondary", "Secondary", "College",
                                "Some College", "Primary", "Graduate",
                                "Some Graduate"),
                     mom_ed_n = c(9, 12, 16, 14, 6, 20, 18))

birth_ord.lut <- tibble(birth_order = levels(WS.demo$birth_order),
                        birth_order_n = 1:8)

# Test for differences
efa.demo <- WS.demo %>%
  filter(data_id %in% efa.half.ID) %>%
  left_join(mom_ed.lut) %>%
  left_join(birth_ord.lut)

# Stats
mean(efa.demo$age) ; sd(efa.demo$age)
mean(efa.demo$mom_ed_n, na.rm = TRUE) ; sd(efa.demo$mom_ed_n, na.rm = TRUE)

table(efa.demo$sex, useNA = "a") / nrow(efa.demo) * 100
(table(efa.demo$ethnicity, useNA = "a") / nrow(efa.demo) * 100) %>%
  round()
(table(efa.demo$birth_order == "First", useNA = "a") / nrow(efa.demo) * 100) %>%
  round()

cfa.demo <- WS.demo %>%
  filter(data_id %in% cfa.half.ID) %>%
  left_join(mom_ed.lut) %>%
  left_join(birth_ord.lut)

# Stats
mean(cfa.demo$age) ; sd(cfa.demo$age)
mean(cfa.demo$mom_ed_n, na.rm = TRUE) ; sd(cfa.demo$mom_ed_n, na.rm = TRUE)

table(cfa.demo$sex, useNA = "a") / nrow(cfa.demo) * 100
(table(cfa.demo$ethnicity, useNA = "a") / nrow(cfa.demo) * 100) %>%
  round()
(table(cfa.demo$birth_order == "First", useNA = "a") / nrow(cfa.demo) * 100) %>%
  round()


################################################################################
# Exploratory factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- .data("FA1000-WS-050.RDS")
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(select(efa.half, -data_id, -age),
                                       factors = x))

  # and save it
  saveRDS(factor.analyses, file = FA1000)

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("plots/WS-factors-", i, ".png"), width = 5, height = 5,
      units = "in", res = 300)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel.plot <- efa.half %>%
                      select(-data_id, -age) %>%
                      fa.parallel(fa = "fa")

png("plots/fa_parallel.png", width = 10, height = 6, res = 300, units = "in")
efa.half %>%
  select(-data_id, -age) %>%
  fa.parallel(fa = "pc")
dev.off()

rmsea <- sapply(factor.analyses, function(x) x$RMSEA) %>%
          t() %>%
          as_tibble() %>%
          add_column(factor = 1:5, .before = 1) %>%
          select(-confidence)

ggplot(rmsea, aes(x = factor, y = RMSEA)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  scale_y_continuous(limits = c(0, NA))

#
# At this point, we have the model
#

# Model assumptions

efa.half.long <- efa.half %>%
                  pivot_longer(-c(data_id))

ggplot(efa.half.long, aes(x = value, y = name, fill = name)) +
  geom_density_ridges2(alpha = 0.5) +
  theme_ridges() +
  theme(legend.position = "none")

efa.half.l.list <- split(efa.half.long, f = efa.half.long$name)
efa.half.l.list.test <- sapply(efa.half.l.list, function(x)
                                shapiro.test(x$value)$p.value)

## All are non-normal, according to Shapiro-Wilk test

################################################################################
# Confirmatory factor analysis
################################################################################

# CFA doesn't seem to like the bootstrapped intervals, so redo it
# mod2 <- structure.diagram(factor.analyses[[2]], cut = .9, errors = TRUE)

fa_2fac <- fa(select(efa.half, -data_id, -age), 2, rotate = "Promax",
           weight = NULL)
model_2fac <- structure.diagram(fa_2fac, cut = 0.6, errors = TRUE)

nobs <- list(nrow(cfa.half), nrow(efa.half))

mcdi.cfa.1 <- cfa(model = model_2fac$lavaan,
                  data = cfa.half %>%
                          select(-data_id, -age),
                  sample.nobs = nobs)

summary(mcdi.cfa.1, fit.measures = TRUE)

# Robust indicator, which should be done, regardless of if it improves fit
mcdi.cfa.1r <- cfa(model = model_2fac$lavaan,
                    data = select(cfa.half, -c(data_id, age)),
                    estimator = "MLR",
                    sample.nobs = nobs)

summary(mcdi.cfa.1r, fit.measures = TRUE)

# Now look at modification indices

mix <- modificationIndices(mcdi.cfa.1r, sort. = TRUE)
# WFV/complexity is high: ~1500

lexical <- paste("MR1 =~ + ", paste(colnames(efa.half)[3:17],
                                    collapse = " + "))
syntax  <- paste("MR2 =~ + ", paste(colnames(efa.half[18:29]),
                                collapse = " + "))
mrcorr <- "MR2 ~~ MR1"

wfv.c <- "WORD_FORMS_VERBS ~~ COMPLEXITY"

# Fix 1st high MI value
model.intercorr <- noquote(c(lexical, syntax, mrcorr, wfv.c))
mcdi.cfa.2 <- cfa(model = model.intercorr,
                  data = select(cfa.half, -data_id, -age),
                  estimator = "MLR",
                  sample.nobs = nobs)

summary(mcdi.cfa.2, fit.measures = TRUE)

mix2 <- modificationIndices(mcdi.cfa.2, sort. = TRUE)

# Fix next round of high MI
model.ic.2 <- noquote(c(lexical, syntax, mrcorr, wfv.c,
                        "WORD_ENDINGS_NOUNS ~~ WORD_ENDINGS_VERBS",
                        "action_words ~ descriptive_words"))

mcdi.cfa.3 <- cfa(model = model.ic.2,
                  data = select(cfa.half, -data_id, -age),
                  estimator = "MLR",
                  sample.nobs = nobs)

summary(mcdi.cfa.3, fit.measures = TRUE)

# Fixing MI doesn't do much

# Demean?
score.avg <- cfa.half %>%
              select(-data_id, -age) %>%
              rowMeans()

cfa.half.demean <- cfa.half %>%
                    mutate_at(vars(-data_id, -age),
                              function(x) x - score.avg)

mcdi.cfa.4 <- cfa(model = model_2fac$lavaan,
                  data = select(cfa.half.demean, -data_id, -age),
                  estimator = "MLR",
                  sample.nobs = nobs)


summary(mcdi.cfa.4, fit.measures = TRUE)

# Three factor fit stats

fa_3fac <- fa(select(efa.half, -data_id, -age), 3, rotate = "Promax",
              weight = NULL)
model_3fac <- structure.diagram(fa_3fac, cut = 0.6, errors = TRUE)

mcdi.cfa3.1r <- cfa(model = model_3fac$lavaan,
                    data = select(cfa.half, -data_id, -age),
                    sample.nobs = nobs,
                    estimator = "MLR")

summary(mcdi.cfa3.1r, fit.measures = TRUE)

################################################################################
# Based on the CFA, we decide the model is fine
# Return factor analysis to age
################################################################################

#
# Make this plot with everyone
#

# Data must be in the same order as factor.analyses. Instead of redoing, do
# this li'l munge
FA_col_order <- colnames(factor.analyses[[2]]$residual)

WS.scores <- factor.scores(dplyr::select(scored$n,
                                         all_of(FA_col_order)),
                            factor.analyses[[2]],
                            method = "Thurstone")

plot.scores <- cbind.data.frame(WS.demo, WS.scores$scores) %>%
  as_tibble() %>%
  left_join(mom_ed.lut)

# Effect of sex on scores
lm(MR1 ~ sex, plot.scores) %>% summary()
lm(MR2 ~ sex, plot.scores) %>% summary()

# Effect of mom's education (continuous)
lm(MR1 ~ mom_ed_y, plot.scores) %>% summary()
lm(MR2 ~ mom_ed_y, plot.scores) %>% summary()

lm(mom_ed_y ~ sex, plot.scores) %>% summary()

smear <- ggplot(plot.scores, aes(x = MR1, y = MR2, color = age)) +
          scale_color_viridis() +
          geom_point(alpha = 0.25, size = 1) +
          labs(x = "Lexical", y = "Syntactic", color = "Age") +
          geom_abline(linetype = "longdash", color = "red", size = 1) +
          theme(legend.position = "none") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2),
                      color = "black", size = 1)

ggplot(filter(plot.scores, !is.na(sex)),
       aes(x = MR1, y = MR1 - MR2, color = sex)) +
  geom_point(alpha = 0.25, size = 1)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)

ggplot(filter(plot.scores, !is.na(mom_ed)),
       aes(x = MR1, y = MR2, color = mom_ed)) +
  geom_point(alpha = 0.25, size = 1)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = FALSE)

# Group by age
plot.scores.summary <- plot.scores %>%
  dplyr::select(-mom_ed_y) %>%
  rename(Lexical = MR1, Structural = MR2) %>%
  pivot_longer(-c(data_id, age, sex, mom_ed, instrument),
               names_to = "factor") %>%
  group_by(age, factor) %>%
  summarise(n = n(),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            iqr = IQR(value, na.rm = TRUE)) %>%
  mutate_at("factor", as_factor)

# Plot increase/decrease in lexical/syntactic scores over age.
# Ribbons represent
#   (1) IQR (50%)
#   (2) 3 IQR (99.3%)
ribbon <- ggplot(plot.scores.summary,
                 aes(x = age, y = mean, color = factor, fill = factor)) +
            geom_line(aes(y = mean), size = 2) +
            geom_ribbon(aes(ymin = mean - 0.5 * iqr, ymax = mean + 0.5 * iqr),
                        alpha = 0.4) +
            geom_ribbon(aes(ymin = mean - 1.5 * iqr, ymax = mean + 1.5 * iqr),
                        alpha = 0.3) +
            facet_grid(rows = vars(factor)) +
            theme(legend.position = "none") +
            labs(x = "Age (mo.)", y = "Mean score [1 IQR, 3 IQR]")

scores.poly.model <- lm(MR2 ~ 1 + MR1 + I(MR1^2), data = plot.scores)
scores.exp.model <- lm(MR2 ~ exp(MR1), data = plot.scores)

# Lower is better
AIC(scores.poly.model)
AIC(scores.exp.model)

ribbon_smear <- arrangeGrob(ribbon + labs(title = "(a)"),
                            smear + labs(title = "(b)"), nrow = 1)

ggsave(plot = ribbon_smear, filename = "plots/lags_5520.png",
       width = 6, height = 3, units = "in")

################################################################################
# Other metrics

total.words <- scored$n %>%
  ungroup() %>%
  mutate(total = dplyr::select(., -data_id, -age) %>% rowSums(),
         gt200 = total >= 200) %>%
  dplyr::select(data_id, total, gt200)

plot.FA2.wide2 <- left_join(plot.scores, total.words)

ggplot(plot.FA2.wide2, aes(x = MR1, y = MR2, color = total)) +
  scale_color_viridis() +
  geom_point(alpha = 0.5) +
  labs(x = "Lexical", y = "Syntactic", color = "Total words",
       title = "Lexical v. syntactic ability, colored by total words") +
  geom_abline() +
  theme(legend.position = "bottom")

ggplot(plot.FA2.wide2, aes(x = MR1, y = MR2, color = gt200)) +
  geom_point(alpha = 0.5) +
  labs(x = "Lexical", y = "Syntactic", color = "Knows >= 200 words",
       title = "Lexical v. syntactic ability, colored by words >= 200") +
  geom_abline() +
  theme(legend.position = "bottom")

################################################################################
# One-weight

count <- scored$n

lexical <- colnames(count)[3:17]
syntactic <- colnames(count)[18:29]
one.weight <- count %>%
  mutate(lex = dplyr::select(., one_of(lexical)) %>% rowSums(),
         syn = dplyr::select(., one_of(syntactic)) %>% rowSums()) %>%
  dplyr::select(data_id, age, lex, syn)

wt1.r <- cor(one.weight$lex, one.weight$syn)
CIr(r = wt1.r, n = nrow(one.weight))

# Max number of words in lex, syn
max_size <- c(566, 221)

onew.ribbon <- one.weight %>%
  mutate_at("lex", function(x) x / max_size[1]) %>%
  mutate_at("syn", function(x) x / max_size[2]) %>%
  pivot_longer(-c(data_id, age)) %>%
  group_by(age, name) %>%
  summarize(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    iqr = IQR(value)
  )

png("plots/1w-lag-presentation.png", width = 7.5, height = 5, units = "in",
    res = 96)

ggplot(onew.ribbon, aes(x = age, y = mean, color = name, fill = name)) +
  geom_line(aes(y = mean), size = 2) +
  geom_ribbon(aes(ymin = mean - 0.5 * iqr, ymax = mean + 0.5 * iqr),
              alpha = 0.4) +
  geom_ribbon(aes(ymin = mean - 1.5 * iqr, ymax = mean + 1.5 * iqr),
              alpha = 0.3) +
  facet_grid(rows = vars(name)) +
  theme(legend.position = "none") +
  labs(x = "Age (mo.)", y = "Mean score")

dev.off()

png("plots/1w-smear-presentation1.png", width = 7.5, height = 5, units = "in",
    res = 96)

ggplot(one.weight, aes(x = lex, y = syn, color = age)) +
  scale_color_viridis() +
  geom_abline(slope = max_size[2] / max_size[1], color = "red",
              linetype = "longdash", size = 1) +
  geom_point(alpha = 0.1) +
  labs(x = "Lexical", y = "Syntactic", color = "Age (mo.)") +
  theme(legend.position = "bottom")

dev.off()

png("plots/1w-smear-presentation2.png", width = 7.5, height = 5, units = "in",
    res = 96)

ggplot(one.weight, aes(x = lex, y = syn, color = age)) +
  scale_color_viridis() +
  geom_abline(slope = max_size[2] / max_size[1], color = "red",
              linetype = "longdash", size = 1) +
  geom_point(alpha = 0.3) +
  labs(x = "Lexical", y = "Syntactic", color = "Age (mo.)") +
  theme(legend.position = "bottom")

dev.off()

png("plots/1w-smear-presentation3.png", width = 7.5, height = 5, units = "in",
    res = 96)

ggplot(one.weight, aes(x = lex, y = syn, color = age)) +
  scale_color_viridis() +
  geom_abline(slope = max_size[2] / max_size[1], color = "red",
              linetype = "longdash", size = 1) +
  geom_point(alpha = 0.3) +
  labs(x = "Lexical", y = "Syntactic", color = "Age (mo.)") +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black")

dev.off()

## Save weights to CSV for easy import to paper draft

factor.analyses[[2]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = .data("results/WS-FA2-loadings.csv"))

factor.analyses[[3]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = .data("results/WS-FA3-loadings.csv"))
