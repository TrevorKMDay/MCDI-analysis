if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI")
} else {
  setwd("G:/My Drive/Research/MCDI")
}

library(tidyverse)
library(tidyselect)
library(psych)

# This is large, so it takes a while!
all <- read_csv("Wordbank-WS-191105.csv",
                col_types = cols(.default = "f", age = "i"))

# Functions

score.produces <- function(v) {

  # Apply identical() over character array to compare against "produces" and
  # not choke on `NA`
  TF <- sapply(v, function(x) identical(as.character(x), "produces"))

  return(TF)

}

score.complexity <- function(v) {

  # Dummy code "complex" as 1 and "simple" as 0
  score <- ifelse(v == "complex", 1, 0)
  return(score)

}

score.SONy <- function(v) {

  # Score {often, 2}, {sometimes, 1}, {not yet, 0}
  score <- ifelse(v == "often", 2,
                  ifelse(v == "sometimes"), 1, 0)

  return(score)

}

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

# NOTE: check this still works
all.merge <- merge(words.grouped, morph.grouped) %>%
               merge(syntax.grouped)

################################################################################
# Factor analysis
################################################################################

# A function to apply the factor analysis over a given # of factors, so that
# they can be saved as a list
apply.FA <- function(data, factors, rotate = "Promax", n.iter = 1000) {

  x <- data %>%
        select(-data_id) %>%
        fa(nfactors = factors, rotate = rotate, n.iter = n.iter)

  return(x)

}

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

  print( fa.diagram(factor.analyses[[i]]) )

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel(all.merge[, -1], fa = "fa")

################################################################################
# Return factor analysis to age
################################################################################

scores.FA2 <- factor.analyses[[2]]$scores

plot.FA2 <- cbind.data.frame(all.demo, scores.FA2) %>%
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
