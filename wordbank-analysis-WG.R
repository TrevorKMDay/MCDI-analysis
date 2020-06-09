if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(tidyselect)
library(psych)
library(viridis)
library(lavaan)
library(corrplot)

# This is large, so it takes a while!
gestures <- read_csv("data/Wordbank-WG-191105.csv",
                      col_types = cols(.default = "f", age = "i"))

# filter(gestures, data_id == gestures$data_id[1]) %>%
#   write_csv("data/g_dict.csv")

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

table.sex <- table(g.demo$sex, useNA = "always")
sex.missing <- table.sex[length(table.sex)]
table.sex / (nrow(g.demo) - sex.missing)

table.mom <- table(g.demo$mom_ed, useNA = "always")
mom.missing <- table.mom[length(table.mom)]
table.mom / (nrow(g.demo) - mom.missing)

wg.mom_ed <- table(g.demo$mom_ed, useNA = "always")

# ws.mom_ed is mother's education from words and sentences, and has to be run
# from that script

if (exists("ws.mom_ed")) {
  
  ws.mom_ed.nm <- ws.mom_ed[-length(ws.mom_ed)]
  wg.mom_ed.nm <- ws.mom_ed[-length(wg.mom_ed)]

  mom_ed.test <- chisq.test(wg.mom_ed.nm, ws.mom_ed.nm, 
                            simulate.p.value = TRUE)
  
  mom_ed <- cbind(wg.mom_ed, ws.mom_ed) %>%
            as_tibble() %>%
            dplyr::rename(wg = wg.mom_ed, ws = ws.mom_ed) %>%
            add_column(level = c(7, 4, 5, 2, 6, 3, 1, 8), .before = 1) %>%
            arrange(level) %>%
            pivot_longer(-level)
  
  mom_ed.plot <- ggplot(mom_ed, aes(x = level, y = value)) +
    geom_histogram(stat = "identity", aes(fill = name), position = "dodge") +
    scale_x_continuous(breaks = 1:8,
                       labels = c("Primary", "Some Secondary", "Secondary",
                                  "Some College", "College", "Some Graduate",
                                  "Graduate", "Missing")) +
    labs(x = NULL, y = "N", fill = "Form") +
    coord_flip() +
    scale_fill_discrete(labels = c("WG", "WS")) +
    theme(legend.position = "bottom", text = element_text(size = 20))
  
  png("plots/mom_ed_presentation.png", width = 5.5, height = 5, units = "in",
      res = 96) ; print(mom_ed.plot) ; dev.off()

}
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
# Split half
################################################################################

# Zip code for UMN
set.seed(55455)

# We need to do the split-half with sex/mom ed present to balance them
efa.demo <- g.demo %>%
              mutate(sex = fct_explicit_na(sex, na_level = "Missing"),
                     mom_ed = fct_explicit_na(mom_ed,
                                              na_level = "Missing")) %>%
              group_by(age, sex, mom_ed) %>%
              sample_frac(.5)

efa.half.ID <- efa.demo$data_id
cfa.half.ID <- g.demo$data_id[!(g.demo$data_id %in% efa.half.ID)]

cfa.demo <- g.demo %>%
              mutate(sex = fct_explicit_na(sex, na_level = "Missing"),
                     mom_ed = fct_explicit_na(mom_ed,
                                              na_level = "Missing")) %>%
              filter(data_id %in% cfa.half.ID)

# Age test
t.test(efa.demo$age, cfa.demo$age)

chisq.test(table(efa.demo$sex), table(cfa.demo$sex))
chisq.test(table(efa.demo$mom_ed), table(cfa.demo$mom_ed))

efa.half <- filter(words.grouped, data_id %in% efa.demo$data_id)
cfa.half <- filter(words.grouped, data_id %in% cfa.demo$data_id)

################################################################################
# EFA
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

wgl <- pivot_longer(efa.half, -data_id)

quantile(wgl$value, probs = seq(0, 1, by = 0.1))

wgl <- wgl %>%
        mutate(trunc_val = replace(value, value > 4/15, 4/15))

ggplot(wgl, aes(x = name, y = value)) +
  geom_boxplot()


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
  write_csv(path = "data/WG-FA2-loadings.csv")

factor.analyses[[3]]$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  write_csv(path = "data/WG-FA3-loadings.csv")

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
