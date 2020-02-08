if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(broom)
library(tidyverse)
library(tidyselect)
library(psych)
library(viridis)
library(ggExtra)
library(corrplot)
library(gridExtra)
library(ggrepel)

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

ws.mom_ed <- table(all.demo$mom_ed)

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
            summarise(sum = sum(produces)) %>%
            pivot_wider(c(data_id), names_from = category,
                        values_from = sum)

# For each ID, count the number of responses in each category, then convert
# that to a proportion and spread to wide, keeping only ID and 22 columns
words.grouped <- words %>%
                  group_by(data_id, category) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id),
                              names_from = category, values_from = score) %>%
                  ungroup()

# Save grouping as RDS
write_csv(as.data.frame(colnames(words.grouped)[-1]), "word_labels.csv")
saveRDS(words.grouped, "words-grouped.RDS")

lex.corr <- cor(select(words.grouped, -data_id))

png("corrplot.png", width = 6, height = 5, units = "in", res = 300)

corrplot(lex.corr, method = "color",
          is.corr = FALSE,
          order = "hclust", addrect = 2,
          col = rev(rainbow(100)),
          tl.pos = "l", tl.col = "black",
          cl.lim = 0:1)

dev.off()

################################################################################
# For the syntactic categories, score separately (different metrics)
################################################################################

# Morphological categories

morphology <- all %>%
                filter(type %in% c("word_forms_nouns", "word_forms_verbs",
                                   "word_endings_nouns",
                                   "word_endings_verbs")) %>%
                mutate(produces = score.produces(value))

morph.n <- morphology %>%
            group_by(data_id, age, type) %>%
            summarise(sum = sum(produces))  %>%
            pivot_wider(c(data_id, age), names_from = type,
                        values_from = sum)

morph.grouped <- morphology %>%
                  group_by(data_id, age, type) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
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
                    summarise(n = n(), sum = sum(complexity)) %>%
                    mutate(COMPLEXITY = sum / n) %>%
                    select(-n, -sum)

## Merge ##

# By default, fa() had been imputing missing values in COMPLEXITY as the median
# rather than 0, which would be what we expect. Do that replacement here.
all.merge <- merge(words.grouped, morph.grouped) %>%
              merge(syntax.grouped) %>%
              mutate(COMPLEXITY = replace(COMPLEXITY, is.na(COMPLEXITY), 0))

saveRDS(all.merge, "WG-scored.rds")

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

set.seed(55455)

all.merge1 <- all.demo %>%
                mutate(sex = fct_explicit_na(sex, na_level = "Missing"),
                        mom_ed = fct_explicit_na(mom_ed, na_level = "Missing")) %>%
                group_by(age, sex, mom_ed) %>%
                sample_frac(.5)

efa.half <- all.merge1$data_id
cfa.half <- all.merge$data_id[!(all.merge$data_id %in% efa.half)]

################################################################################
# Factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- "FA1000.RDS"
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(select(all.merge, -age),
                                       factors = x))

  # and save it
  saveRDS(factor.analyses, file = "FA1000.RDS")

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("factors", i, ".png"), width = 5, height = 5, units = "in",
      res = 300)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel.plot <- all.merge %>%
                      select(-data_id, -age) %>%
                      fa.parallel(fa = "fa")

png("plots/fa_parallel.png", width = 10, height = 6, res = 300, units = "in")
all.merge %>%
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

################################################################################
# Return factor analysis to age
################################################################################

scores.FA2 <- factor.analyses[[2]]$scores

plot.FA2.wide <- cbind.data.frame(all.demo, scores.FA2) %>%
                  as_tibble()

plot.FA2 <- plot.FA2.wide %>%
              pivot_longer(starts_with("MR"),
                           names_to = "factor", values_to = "value")

ggplot(plot.FA2, aes(x = age, y = value, color = factor)) +
  geom_point(position = position_jitter(width = 0.5, height = 0), alpha = 0.1) +
  geom_boxplot(aes(group = interaction(age, factor)), outlier.shape = NA,
               alpha = 0.5) +
  geom_smooth()

plot.FA2.summary <- plot.FA2 %>%
                      group_by(age, factor) %>%
                      summarise(n = n(), mean = mean(value, na.rm = TRUE),
                                sd = sd(value, na.rm = TRUE),
                                iqr = IQR(value, na.rm = TRUE)) %>%
                      mutate_at("factor", as_factor)

levels(plot.FA2.summary$factor) <- c("Lexical", "Syntactic")

# Plot increase/decrease in lexical/syntactic scores over age.
# Ribbons represent
#   (1) IQR (50%)
#   (2) 3 IQR (99.3%)
ribbon <- ggplot(plot.FA2.summary, aes(x = age, y = mean, color = factor,
                             fill = factor)) +
            geom_line(aes(y = mean), size = 2) +
            geom_ribbon(aes(ymin = mean - 0.5 * iqr, ymax = mean + 0.5 * iqr),
                        alpha = 0.4) +
            geom_ribbon(aes(ymin = mean - 1.5 * iqr, ymax = mean + 1.5 * iqr),
                        alpha = 0.3) +
            facet_grid(rows = vars(factor)) +
            theme(legend.position = "none") +
            labs(x = "Age (mo.)", y = "Mean score")

smear <- ggplot(plot.FA2.wide, aes(x = MR1, y = MR2, color = age)) +
          scale_color_viridis() +
          geom_point(alpha = 0.5) +
          labs(x = "Lexical", y = "Syntactic", color = "Age (mo.)") +
          geom_abline(linetype = "longdash", color = "red", size = 1) +
          theme(legend.position = "bottom") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black",
                      size = 1)

scores.poly.model <- lm(MR2 ~ 1 + MR1 + I(MR1^2), data = plot.FA2.wide)
scores.exp.model <- lm(MR2 ~ exp(MR1), data = plot.FA2.wide)

# Lower is better
AIC(scores.poly.model)
AIC(scores.exp.model)

png("plots/lags.png", width = 12, height = 5, units = "in", res = 300)
grid.arrange(ribbon + labs(title = "(a)"),
             smear + labs(title = "(b)"), nrow = 1)
dev.off()

# Lowest (0%) and highest (100%)
smear +
  geom_label_repel(aes(label = ifelse(plot.FA2.wide$data_id %in% c(130423,
                                                                  134553),
                                     plot.FA2.wide$data_id, "")),
                   color = "black")

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
                 WORD_ENDINGS_NOUNS = word_endings_nouns) %>%
          mutate(COMPLEXITY = replace(COMPLEXITY, is.na(COMPLEXITY), 0))

lexical <- colnames(count)[2:17]
syntactic <- colnames(count)[18:29]
one.weight <- count %>%
                mutate(lex = select(., one_of(lexical)) %>% rowSums(),
                       syn = select(., one_of(syntactic)) %>% rowSums()) %>%
                select(data_id, age, lex, syn)

png("plots/1w-smear.png", width = 6, height = 5, units = "in", res = 300)

ggplot(one.weight, aes(x = lex, y = syn, color = age)) +
  scale_color_viridis() +
  geom_point(alpha = 0.3) +
  labs(x = "Lexical", y = "Syntactic", color = "Age (mo.)") +
  geom_abline(slope = 221 / 596, color = "red", linetype = "longdash",
              size = 1) +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black")

dev.off()

w1.poly.model <- lm(syn ~ 1 + lex + I(lex ^2), data = one.weight)
w1.exp.model <- lm(syn ~ a*exp(b+lex), data = one.weight2)

# Lower is better; poly is a clear better fit
AIC(w1.poly.model)
AIC(w1.exp.model)

aic <- matrix(NA, 2, 2)
rownames(aic) <- c("scores", "ones")
colnames(aic) <- c("poly2", "exp")

aic[1, 1] <- AIC(scores.poly.model)
aic[1, 2] <- AIC(scores.exp.model)
aic[2, 1] <- AIC(w1.poly.model)
aic[2, 2] <- AIC(w1.exp.model)

coxtest(scores.poly.model, scores.exp.model)
coxtest(w1.poly.model, w1.exp.model)

coxtest(scores.poly.model, w1.poly.model)
coxtest(scores.exp.model, w1.exp.model)

################################################################################
# How long until 50% acquired?

all.merge <- select(all.merge, data_id, age, everything())

l15 <- all.merge[, 1:17] %>%
        mutate(avg = rowMeans(select(., -data_id, -age)))
l7 <- all.merge[, c(1:2, 18:25)] %>%
  mutate(avg = rowMeans(select(., -data_id, -age)))

lm.l15 <- lm(avg ~ age, data = l15)
lm.l7  <- lm(avg ~ age, data = l7)

l15_age50 <- (.5 - lm.l15$coefficients[1]) / lm.l15$coefficients[2]
l7_age50 <- (.5 - lm.l7$coefficients[1]) / lm.l7$coefficients[2]
