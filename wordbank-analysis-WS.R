if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(broom)
library(tidyverse)
library(tidyselect)

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

# This is large, so it takes a while!
sentences.file <- "data/Wordbank-WS-191105.RDS"

if (!file.exists(sentences.file)){
  all <- read_csv("data/Wordbank-WS-191105.csv",
                  col_types = cols(.default = "f", age = "i"))
  write_rds(all, path = sentences.file)
} else {
  all <- read_rds(sentences.file)
}

# Kyle says Promax is orthogonal, followed by an oblique correction
# oblimin is orthogonal from the get-go, but is more computationally intensive
# (but fine on modern machines)
rotation <- "oblimin"

source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

all.demo <- all %>%
              select(data_id, age, sex, mom_ed) %>%
              distinct() %>%
              mutate(instrument = "S")

mean(all.demo$age)
sd(all.demo$age)

table(all.demo$sex, useNA = "always")

table(all.demo$mom_ed,
      useNA = "always")

ws.mom_ed <- table(all.demo$mom_ed, useNA = "always")

length(unique(all.demo$data_id)) == nrow(all.demo)

################################################################################
# Score per Part I subcategory
# For type = word, sum 'produce' over all
################################################################################

# Convert produces/NA to T/F (takes a while on 3.5M elements)
words <- all %>%
          filter(type == "word") %>%
          mutate(produces = score.produces(value))

words.n <- words %>%
            group_by(data_id, category) %>%
            dplyr::summarise(sum = sum(produces)) %>%
            pivot_wider(c(data_id), names_from = category,
                        values_from = sum)

# For each ID, count the number of responses in each category, then convert
# that to a proportion and spread to wide, keeping only ID and 22 columns
words.grouped <- words %>%
                  group_by(data_id, category) %>%
                  dplyr::summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id),
                              names_from = category, values_from = score) %>%
                  ungroup()

# Save grouping as RDS
write_csv(as.data.frame(colnames(words.grouped)[-1]), "word_labels.csv")
saveRDS(words.grouped, "words-grouped.RDS")

lex.corr <- cor(select(words.grouped, -data_id))

png("plots/corrplot.png", width = 6, height = 5, units = "in", res = 300)

corrplot(lex.corr, method = "color",
          is.corr = FALSE,
          order = "hclust", addrect = 2,
          col = rev(rainbow(100)),
          tl.pos = "l", tl.col = "black",
          cl.lim = 0:1)

dev.off()

################################################################################
# For the structural categories, score separately (different metrics)
################################################################################

# Morphological categories

morphology <- all %>%
                filter(type %in% c("word_forms_nouns", "word_forms_verbs",
                                   "word_endings_nouns",
                                   "word_endings_verbs")) %>%
                mutate(produces = score.produces(value))

morph.n <- morphology %>%
            group_by(data_id, age, type) %>%
            dplyr::summarise(sum = sum(produces)) %>%
            pivot_wider(c(data_id, age), names_from = type,
                        values_from = sum)

morph.grouped <- morphology %>%
                  group_by(data_id, age, type) %>%
                  dplyr::summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id, age), names_from = type,
                              values_from = score)

colnames(morph.grouped)[-(1:2)] <- toupper(colnames(morph.grouped)[-(1:2)])

ggplot(pivot_longer(data = morph.grouped,
                    cols = c(WORD_FORMS_NOUNS, WORD_FORMS_VERBS,
                             WORD_ENDINGS_NOUNS, WORD_ENDINGS_VERBS)),
       aes(x = age, y = value, color = name)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess")

# Get a list of everyone whose answer to "does your kid combine verbs" was
# "often" or "sometimes"
# syntax.combine <- all %>%
#                     filter(type == "combine",
#                            value != "not yet")
#
# who.combines <- syntax.combine$data_id

syntax <- all %>%
            filter(type == "complexity") %>%
            mutate(complexity = score.complexity(value))

syntax.n <- syntax %>%
              group_by(data_id) %>%
              summarise(COMPLEXITY = sum(complexity))

# n is constant, but just calculate instead of hardcode
syntax.grouped <- syntax %>%
                    group_by(data_id) %>%
                    dplyr::summarise(n = n(), sum = sum(complexity)) %>%
                    mutate(COMPLEXITY = sum / n) %>%
                    select(-n, -sum)

## Merge ##

# By default, fa() had been imputing missing values in COMPLEXITY as the median
# rather than 0, which would be what we expect. Do that replacement here.
all.merge <- merge(words.grouped, morph.grouped) %>%
              merge(syntax.grouped) %>%
              mutate(COMPLEXITY = replace(COMPLEXITY, is.na(COMPLEXITY), 0))

out <- list(count, all.merge)

saveRDS(out, "data/WS-scored.rds")

# Remove everyone with COMPLEXITY as NA (that means the whole section was
# skipped and so the values are inaccurate.)
# all.merge <- merge(words.grouped, morph.grouped) %>%
#                 merge(syntax.grouped) %>%
#                 filter(!is.na(.$COMPLEXITY))
#
# keep <- !is.na(syntax.grouped$COMPLEXITY)

################################################################################
# Split half
################################################################################

# Zip code for UMN
set.seed(55455)

# We need to do the split-half with sex/mom ed present to balance them
all.merge1 <- all.demo %>%
                mutate(sex = fct_explicit_na(sex, na_level = "Missing"),
                        mom_ed = fct_explicit_na(mom_ed,
                                                 na_level = "Missing")) %>%
                group_by(age, sex, mom_ed) %>%
                sample_frac(.5)

# Everyone in all.merge1; everyone not
efa.half.ID <- all.merge1$data_id
cfa.half.ID <- all.merge$data_id[!(all.merge$data_id %in% efa.half.ID)]

efa.half <- filter(all.merge, data_id %in% efa.half.ID)
cfa.half <- filter(all.merge, data_id %in% cfa.half.ID)

# Test for differences
efa.demo <- all.demo %>%
              filter(data_id %in% efa.half.ID)

cfa.demo <- all.demo %>%
              filter(data_id %in% cfa.half.ID)

# Age
t.test(efa.demo$age, cfa.demo$age)

# Sex

efa.sex <- efa.demo %>%
            mutate(sex = fct_explicit_na(sex, na_level = "Missing")) %>%
            group_by(sex) %>%
            dplyr::summarize(n = n())

cfa.sex <- cfa.demo %>%
            mutate(sex = fct_explicit_na(sex, na_level = "Missing")) %>%
            group_by(sex) %>%
            dplyr::summarize(n = n())

chisq.test(efa.sex$n, cfa.sex$n)

# Mother's education

efa.mom <- efa.demo %>%
            mutate(mom_ed = fct_explicit_na(mom_ed, na_level = "Missing")) %>%
            group_by(mom_ed) %>%
            dplyr::summarize(n = n())

cfa.mom <- cfa.demo %>%
              mutate(mom_ed = fct_explicit_na(mom_ed, na_level = "Missing")) %>%
              group_by(mom_ed) %>%
              dplyr::summarize(n = n())

chisq.test(efa.mom$n, cfa.mom$n)

################################################################################
# Exploratory factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- "FA1000-WS-050.RDS"
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(select(efa.half, -age),
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
  fa.parallel(fa = "fa")
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
                  pivot_longer(-c(data_id, age))

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

lexical <- paste("MR1 =~ + ", paste(colnames(efa.half)[2:16],
                                    collapse = " + "))
syntax  <- paste("MR2 =~ + ", paste(colnames(efa.half[c(17:23, 25:29)]),
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

all.scores <- factor.scores(select(all.merge, -data_id, -age),
                            factor.analyses[[2]],
                            method = "tenBerge")

plot.scores <- cbind.data.frame(all.demo, all.scores$scores) %>%
                as_tibble()

lm(MR1 ~ sex, plot.scores) %>% summary()
lm(MR2 ~ sex, plot.scores) %>% summary()

## Mom ed
mom_ed.lut <- data.frame(mom_ed = c("Some Secondary", "Secondary", "College", 
                                    "Some College", "Primary", "Graduate", 
                                    "Some Graduate"),
                          mom_ed_y = c(9, 12, 16, 14, 6, 20, 18))

lm(MR1 ~ mom_ed_y, plot.scores) %>% summary()
lm(MR2 ~ mom_ed_y, plot.scores) %>% summary()

lm(mom_ed_y ~ sex, plot.scores) %>% summary()

plot.scores <- left_join(plot.scores, mom_ed.lut)

smear <- ggplot(plot.scores, aes(x = MR1, y = MR2, color = age)) +
          scale_color_viridis() +
          geom_point(alpha = 0.25, size = 1) +
          labs(x = "Lexical", y = "Syntactic", color = "Age") +
          geom_abline(linetype = "longdash", color = "red", size = 1) +
          theme(legend.position = "none") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black",
                      size = 1)

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
                        select(-mom_ed_y) %>%
                        rename(Lexical = MR1, Structural = MR2) %>%
                        pivot_longer(-c(data_id, age, sex, mom_ed, instrument),
                                     names_to = "factor") %>%
                        group_by(age, factor) %>%
                        summarise(n = n(), mean = mean(value, na.rm = TRUE),
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
            labs(x = "Age (mo.)", y = "Mean score")



scores.poly.model <- lm(MR2 ~ 1 + MR1 + I(MR1^2), data = plot.FA2.wide)
scores.exp.model <- lm(MR2 ~ exp(MR1), data = plot.FA2.wide)

# Lower is better
AIC(scores.poly.model)
AIC(scores.exp.model)

ribbon_smear <- arrangeGrob(ribbon + labs(title = "(a)"),
                            smear + labs(title = "(b)"), nrow = 1)

ggsave(plot = ribbon_smear, filename = "plots/lags_5520.png", 
       width = 6, height = 3, units = "in")

################################################################################
# Other metrics

total.words <- words.n %>%
                  ungroup() %>%
                  mutate(total = select(., -data_id) %>% rowSums(),
                         gt200 = total >= 200) %>%
                  select(data_id, total, gt200)

plot.FA2.wide2 <- merge(plot.FA2.wide, total.words)

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

count <- merge(words.n, morph.n) %>%
          merge(syntax.n) %>%
          select(data_id, age, everything()) %>%
          rename(WORD_FORMS_NOUNS = word_forms_nouns,
                 WORD_FORMS_VERBS = word_forms_verbs,
                 WORD_ENDINGS_NOUNS = word_endings_nouns,
                 WORD_ENDINGS_VERBS = word_endings_verbs) %>%
          mutate(COMPLEXITY = replace(COMPLEXITY, is.na(COMPLEXITY), 0))

lexical <- colnames(count)[3:17]
syntactic <- colnames(count)[18:29]
one.weight <- count %>%
                mutate(lex = select(., one_of(lexical)) %>% rowSums(),
                       syn = select(., one_of(syntactic)) %>% rowSums()) %>%
                select(data_id, age, lex, syn)

max_size <- c(566, 221)

onew.ribbon <- one.weight %>%
                mutate_at("lex", function(x) x / max_size[1]) %>%
                mutate_at("syn", function(x) x / max_size[2]) %>%
                pivot_longer(-c(data_id, age)) %>%
                group_by(age, name) %>%
                summarize(n = n(), 
                          mean = mean(value),
                          sd = sd(value),
                          iqr = IQR(value))

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
  write_csv(path = "data/WS-FA2-loadings.csv")

factor.analyses[[3]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = "data/WS-FA3-loadings.csv")

