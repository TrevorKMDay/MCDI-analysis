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

# This is large, so it takes a while!
all <- read_csv("Wordbank-WS-191105.csv",
                col_types = cols(.default = "f", age = "i"))

# Kyle says Promax is orthogonal, followed by an oblique correction
# oblimin is orthogonal from the get-go, but is more computationally intensive
# (but fine on modern machines)
rotation <- "promax"

source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

all.demo <- all %>%
              select(data_id, age, sex, mom_ed) %>%
              distinct()

length(unique(all.demo$data_id)) == nrow(all.demo)

################################################################################
# Score per Part I subcategory
# For type = word, sum 'produce' over all
################################################################################

# Convert produces/NA to T/F (takes a while on 3.5M elements)
words <- all %>%
          filter(type == "word") %>%
          mutate(produces = score.produces(value))

# For each ID, count the number of responses in each category, then convert
# that to a proportion and spread to wide, keeping only ID and 22 columns
words.grouped <- words %>%
                  group_by(data_id, category) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id),
                              names_from = category, values_from = score)

# Save grouping as RDS
write_csv(as.data.frame(colnames(words.grouped)[-1]), "word_labels.csv")
saveRDS(words.grouped, "words-grouped.RDS")

################################################################################
# For the syntactic categories, score separately (different metrics)
# Only score complexity if combine != 'not yet'
################################################################################

# Morphological categories

morphology <- all %>%
                filter(type %in% c("word_forms_nouns", "word_forms_verbs",
                                   "word_endings_nouns",
                                   "word_endings_verbs")) %>%
                mutate(produces = score.produces(value))

morph.grouped <- morphology %>%
                  group_by(data_id, type) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(data_id, names_from = type,
                              values_from = score)
colnames(morph.grouped)[-1] <- toupper(colnames(morph.grouped)[-1])

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

# n is constant, but just calculate instead of hardcode
syntax.grouped <- syntax %>%
                    group_by(data_id) %>%
                    summarise(n = n(), sum = sum(complexity)) %>%
                    mutate(COMPLEXITY = sum / n) %>%
                    select(-n, -sum)

## Merge ##

# By default, fa() had been imputing missing values in COMPLEXITY as the median
# rather than 0, which would be what we expect. Do that replacement here.
# all.merge <- merge(words.grouped, morph.grouped) %>%
#                merge(syntax.grouped) %>%
#                 mutate(COMPLEXITY = replace(COMPLEXITY, is.na(COMPLEXITY), 0))

# Remove everyone with COMPLEXITY as NA (that means the whole section was
# skipped and so the values are inaccurate.)
all.merge <- merge(words.grouped, morph.grouped) %>%
                merge(syntax.grouped) %>%
                filter(!is.na(.$COMPLEXITY))

keep <- !is.na(syntax.grouped$COMPLEXITY)

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
                              apply.FA(all.merge, factors = x))

  # and save it
  saveRDS(factor.analyses, file = "FA1000.RDS")

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("factors", i, ".png"), width = 500, height = 500)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel(all.merge[, -1], fa = "fa")

################################################################################
# Return factor analysis to age
################################################################################

scores.FA2 <- factor.analyses[[2]]$scores

plot.FA2.wide <- cbind.data.frame(all.demo[keep, ], scores.FA2)
plot.FA2 <- plot.FA2.wide%>%
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
                                iqr = IQR(value, na.rm = TRUE))

# Plot increase/decrease in lexical/syntactic scores over age.
# Ribbons represent
#   (1) IQR (50%)
#   (2) 3 IQR (99.3%)
ggplot(plot.FA2.summary, aes(x = age, y = mean, color = factor,
                             fill = factor)) +
  geom_line(aes(y = mean), size = 2) +
  geom_ribbon(aes(ymin = mean - 0.5 * iqr, ymax = mean + 0.5 * iqr),
              alpha = 0.4) +
  geom_ribbon(aes(ymin = mean - 1.5 * iqr, ymax = mean + 1.5 * iqr),
              alpha = 0.3) +
  facet_grid(rows = vars(factor))

# Try stuff

BCP.data <- read_csv("BCP-scores-191107.csv")

BCP.estimate <- factor.scores(BCP.data[, -(1:2)],
                              factor.analyses[[2]],
                              method = "Bartlett",
                              impute = "mean")

BCP.scores <- cbind(BCP.data[, 1:2], BCP.estimate$scores) %>%
                pivot_longer(starts_with("MR"), names_to = "factor",
                             values_to = "value")

ggplot(plot.FA2.summary, aes(x = age, y = mean, color = factor,
                             fill = factor)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.4) +
  geom_ribbon(aes(ymin = mean - 3 * sd, ymax = mean + 3 * sd),
              alpha = 0.3) +
  geom_point(data = BCP.scores, aes(x = demo.age_bin, y = value),
             color = "black") +
  geom_line(data = BCP.scores,
            aes(x = demo.age_bin, y = value, group = demo.CandID),
            color = "black") +
  facet_grid(rows = vars(factor))

ggplot(NULL) +
  geom_jitter(data = filter(plot.FA2, factor == "MR1"),
              aes(x = age, y = value),
              color = "blue", width = 0.3, height = 0,
              alpha = 0.1) +
  geom_point(data = BCP.scores,
             aes(x = demo.age_bin, y = value),
             color = "red")


#
# Does syntax lag lexicon per subject
#

BCP.scores.w <- cbind(BCP.data[, 1:2], BCP.estimate$scores)

ggplot(plot.FA2.wide, aes(x = MR1, y = MR2)) +
  geom_point(alpha = 0.5, aes(color = age)) +
  scale_color_viridis() +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 2,
              alpha = 0.5) +
  labs(x = "Lexical score", y = "Syntax score")

ggplot(plot.FA2.wide, aes(x = MR1, y = MR2)) +
  geom_point(alpha = 0.5, aes(color = age)) +
  scale_color_viridis() +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 2,
              alpha = 0.5) +
  labs(x = "Lexical score", y = "Syntax score")+
  geom_point(data = BCP.scores.w, aes(x = MR1, y = MR2), shape = 17, size = 2,
             color = "red") +
  geom_line(data = BCP.scores.w, aes(x = MR1, y = MR2, group = demo.CandID),
            color = "red")


# Extra plots

RMSEA <- sapply(factor.analyses, function(x) x$RMSEA) %>%
          t() %>%
          as_tibble() %>%
          select(-confidence) %>%
          add_column(n = 1:5, .before = 1)

ggplot(RMSEA, aes(x = n, y = RMSEA)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower,  ymax = upper), width = 0.1)

# Lexical proportion

all.lex <- all.merge %>%
            select(data_id, matches("^[a-z]", ignore.case = FALSE)) %>%
            mutate(mean = select(., -matches("data_id")) %>%
                     rowMeans(.)) %>%
            add_column(age = all.demo[keep, ]$age, .after = "data_id")

bcp.lex <- BCP.data %>%
            select(-matches("^[A-Z]", ignore.case = FALSE)) %>%
            mutate(mean = select(., -matches("demo.")) %>%
                     rowMeans(.))

ggplot(NULL) +
  geom_point(data = all.lex,
              aes(x = age, y = mean),
              position = position_jitter(width = 0.4, height = 0),
              alpha = 0.1)+
  geom_point(data = bcp.lex,
             aes(x = demo.age_bin, y = mean),
             position = position_jitter(width = 0.4, height = 0),
             color = "red")

################################################################################
# Can we estimate SYNTAX categories from lexicosyntactic categories
################################################################################

# Get the relevant columns
# Remove anyone who has NA values in COMPLEXITY because they didn't fill out the
#  last section
est.syntax <- all.merge %>%
                select(data_id, time_words, pronouns,
                       question_words, locations, quantifiers, helping_verbs,
                       connecting_words, matches("^[WC]", ignore.case = FALSE)) %>%
                add_column(age = all.demo[keep, ]$age, .after = "data_id")

# syntax$COMPLEXITY[is.na(syntax$COMPLEXITY)] <- 0

SYNTAX <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS",
            "WORD_ENDINGS_NOUNS", "WORD_ENDINGS_VERBS",
            "COMPLEXITY")
formula <- paste0("~ age + time_words + pronouns + question_words + locations",
                  "+ quantifiers + helping_verbs + connecting_words")

# Provide estimations for each of the 5 syntax categories in a list
estimations <- lapply(SYNTAX,
                      function(x) lm(data = est.syntax,
                                      formula = paste(x, formula)) %>%
                                    augment() )

for (i in 1:length(estimations)) {

  x <- estimations[[i]]

  plot <- ggplot(x, aes(x = age, y = .resid)) +
            geom_point(position = position_jitter(width = 0.4, height = 0),
                       alpha = 0.2) +
            scale_y_continuous(limits = c(-1, 1)) +
            geom_smooth(method = "lm") +
            labs(x = "Age (mo)", y = "Residual", title = SYNTAX[i])

  margins <- ggMarginal(plot, margins = "both", type = "density")

  dir.create("recapitulateSYNTAX", showWarnings = FALSE)

  png(paste0("recapitulateSYNTAX/", SYNTAX[i], ".png"),
      width = 10, height = 7, units = "in", res = 300)
  print(margins)
  dev.off()

}

estimations.value <- sapply(estimations,
                            function(x) x$.fitted,
                            simplify = TRUE)
colnames(estimations.value) <- paste0(SYNTAX)

# Recapitulate factor scores
syntax.fit <- all.merge %>%
                select(-matches("^[WC]", ignore.case = FALSE)) %>%
                cbind.data.frame(., estimations.value) %>%
                mutate_all(function(x) replace(x, x<0, 0))


# Compare the reapplication of the model
syntax.fa <- factor.scores(syntax.fit[, -1],
                            factor.analyses[[2]],
                            method = "Bartlett",
                            impute = "mean")

syntax.fa.scores <- as_tibble(syntax.fa$scores)
colnames(syntax.fa.scores) <- c("MR1.refit", "MR2.refit")
syntax.fa.scores <- add_column(.data = syntax.fa.scores,
                               data_id = est.syntax$data_id,
                               .before = 1)

model.compare <- left_join(plot.FA2.wide, syntax.fa.scores) %>%
                  mutate(MR1.diff = MR1 - MR1.refit,
                         MR2.diff = MR2 - MR2.refit,
                         MR1.err = MR1.diff / MR1,
                         MR2.err = MR2.diff / MR2) %>%
                  select(data_id, age, MR1.err, MR2.err) %>%
                  pivot_longer(c(MR1.err, MR2.err), names_to = "err",
                               values_to = "value") %>%
                  right_join(all.merge)

ggplot(model.compare, aes(x = age, y = abs(value), color = COMPLEXITY)) +
  geom_point(position = position_jitter(width = 0.4), alpha = 0.75) +
  facet_grid(rows = vars(err), scales = "free") +
  scale_color_viridis() +
  scale_y_log10() +
  geom_hline(aes(yintercept = 1)) +
  labs(x = "Age (mo)", y = "log(Error)")

model.compare %>%
    filter(err == "MR1.err", abs(value) > 50)

# Compare a de novo model
syntax.fa.2 <- fa(syntax.fit[, -1], nfactors = 2, rotate = "oblimin")

fa.diagram(syntax.fa.2)

