if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(tidyselect)
library(psych)
library(viridis)

# This is large, so it takes a while!
gestures <- read_csv("data/Wordbank-WG-191105.csv",
                      col_types = cols(.default = "f", age = "i"))

source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

g.demo <- gestures %>%
            select(data_id, age, sex, mom_ed) %>%
            distinct() %>%
            mutate(instrument = "G")

mean(g.demo$age)
sd(g.demo$age)

table(g.demo$sex,
      useNA = "always")

table(g.demo$mom_ed,
      useNA = "always")

wg.mom_ed <- table(g.demo$mom_ed)

# mom_ed <- cbind(wg.mom_ed, ws.mom_ed) %>%
#           as_tibble() %>%
#           rename(wg = wg.mom_ed, ws = ws.mom_ed) %>%
#           add_column(level = c(7, 4, 5, 2, 6, 3, 1), .before = 1)
# 
# mom_ed.melt <- mom_ed %>%
#                 mutate(wg = wg / 1068,
#                        ws = ws / 2776) %>%
#                 pivot_longer(-level)
# 
# mom_ed %>%
#   mutate(wg = wg / 1068,
#          ws = ws / 2776) %>%
#   chisq.test()
# 
# ggplot(mom_ed.melt, aes(x = level, y = value, color = name)) +
#   geom_line()
# 
# length(unique(g.demo$data_id)) == nrow(g.demo)

################################################################################
# Score per Part I.D subcategory
# For type = word, sum 'produce' over all
################################################################################

# Convert produces/NA to T/F (takes a while on 3.5M elements)
words <- gestures %>%
          filter(type == "word") %>%
          mutate(produces = score.produces(value))

# For each ID, count the number of responses in each category, then convert
# that to a proportion and spread to wide, keeping only ID and 22 columns
words.grouped <- words %>%
                  group_by(data_id, category) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id),
                              names_from = category, values_from = score) %>%
                  ungroup()

words.corr <- select(words.grouped, -data_id) %>%
                cor()

corrplot(words.corr, method = "color",
         is.corr = FALSE,
         order = "hclust", addrect = 2,
         col = rev(rainbow(100)),
         tl.pos = "l", tl.col = "black",
         cl.lim = 0:1)


words.lexsym <- words %>%
                  mutate(syntactic = category %in% c("time_words",
                                                     "descriptive_words",
                                                     "pronouns",
                                                     "question_words",
                                                     "locations",
                                                     "quantifiers")) %>%
                  group_by(data_id, age, syntactic) %>%
                  summarise(n = n(),
                            score = sum(produces),
                            perc = score / n) %>%
                  pivot_wider(id_cols = c(data_id, age), values_from = perc,
                              names_from = syntactic) %>%
                  rename(SYNTAX = "TRUE", LEXICAL = "FALSE") %>%
                  ungroup()

################################################################################
# Factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- "FA1000-WG.RDS"
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(words.grouped, factors = x,
                                       rotate = "oblimin"))

  # and save it
  saveRDS(factor.analyses, file = FA1000)

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("WG-factors", i, ".png"), width = 500, height = 500)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel(words.grouped[, -1], fa = "fa")

# Extra plots

RMSEA <- sapply(factor.analyses, function(x) x$RMSEA) %>%
  t() %>%
  as_tibble() %>%
  select(-confidence) %>%
  add_column(n = 1:5, .before = 1)

ggplot(RMSEA, aes(x = n, y = RMSEA)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower,  ymax = upper), width = 0.1)

#
# Plot 3-factor solution 1+3 against 2
#

wgl <- pivot_longer(words.grouped, -data_id)

quantile(wgl$value, probs = seq(0, 1, by = 0.1))

wgl <- wgl %>%
        mutate(trunc_val = replace(value, value > 4/15, 4/15))

ggplot(wgl, aes(x = name, y = value)) +
  geom_boxplot()


values <- factor.analyses[[3]]$scores %>%
            as_tibble() %>%
            mutate(lexical = (MR1 + MR3) / 2)

values.demo <- cbind.data.frame(g.demo, values)

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
