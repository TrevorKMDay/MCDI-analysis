if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis")
}

library(tidyverse)
library(ggplot2)
library(reshape2)
library(fastICA) ; set.seed("03181996")
library(readxl)
library(Hmisc)
library(psych)
library(corrplot)
library(ggrepel)

source("wordbank-functions.R")

mullen <- read_csv("data/bcp-UMN-mullen-191030.csv") %>%
            select_all(~gsub("^.*,", "", .)) %>%
            separate(Visit_label, into = c("x", "age"), sep = "x") %>%
            mutate(age = as.numeric(gsub("m", "", age))) %>%
            select(-x)

# mcdi_all <- read_csv("bcp-mcdi-103019.csv")
mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-200131.csv")

names <- read_excel("data/mcdi-sections.xlsx", sheet = 1)

lexical <- names$name[1:(22-7)]
syntactic <- names$name[(22-6):22]

#
# Clean up mullen
#

# Remove column prefixes
colnames(mullen) <- gsub("^.*,", "", colnames(mullen))

mullen <- mullen %>%
            mutate_at(vars(contains("language")), as.numeric) %>%
            select_all(~gsub("expressive", "expr", .)) %>%
            select_all(~gsub("receptive", "recv", .)) %>%
            select_all(~gsub("language", "lg", .)) %>%
            select_all(~gsub("age_equivalent", "age", .))

mullen.melt <- mullen %>%
                melt(id.vars = 1:6) %>%
                separate(variable, into = c("dir", "x", "variable"),
                         sep = "_") %>%
                select(-x) %>%
                mutate_at(c("variable", "dir"), as.factor)

#
# Clean up MCDI
#

# Remove some column prefixes
colnames(mcdi_all) <- gsub("demographics,", "demo.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi,", "gestures.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi_words_sentences,", "sentences.",
                           colnames(mcdi_all))

mcdi_all <- mcdi_all %>%
              separate(demo.Visit_label, into = c(NA, "demo.age_bin"),
                       sep = "x") %>%
              mutate(demo.age_bin = suppressWarnings(as.numeric(gsub("m", "",
                                                                     demo.age_bin))))

gestures <- select(mcdi_all, Identifiers, starts_with("demo"),
                        starts_with("gestures")) %>%
                  add_column(demo.age = suppressWarnings(as.numeric(.$gestures.Candidate_Age)),
                              .after = "demo.age_bin") %>%
                  filter(gestures.Administration == "All")

write_csv(gestures, "gestures.csv")

sent <- select(mcdi_all, Identifiers, starts_with("demo"),
                    starts_with("sentences")) %>%
              add_column(demo.age = suppressWarnings(as.numeric(.$sentences.Candidate_Age)),
                         .after = "demo.age_bin") %>%
              filter(sentences.Administration == "All") %>%
              mutate(age_diff = demo.age_bin - demo.age)

ggplot(sent, aes(x= sent$age_diff)) +
  geom_density()

MLU <- sent %>%
        filter(sentences.combining != "not_yet") %>%
        select(demo.CandID, demo.age_bin, sentences.II_D_1) %>%
        rename(raw = sentences.II_D_1) %>%
        mutate(raw = gsub("[[]PII reviewed[]]", "", raw)) %>%
        separate(raw, into = c("u1", "u2", "u3"), sep = "//", 
                 extra = "merge") %>%
        filter(is.na(u1) + is.na(u2) + is.na(u3) < 3)

write_csv(MLU, path = "data/MLU-draft.csv")

################################################################################
################################################################################

# Mullen analysis

# Age-equivalent vs visit age
ggplot(filter(mullen.melt, variable == "age"),
       aes(x = age, y = value, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm")

# Rceptive vs. expressive
ggplot(mullen, aes(x = expr_lg_age, y = recv_lg_age)) +
  geom_point() +
  geom_smooth(method = "lm")

# Expressive predicts receptive, no duh, at B = 0.9, so E lags R (probably
# overreporting on the parent's end)
EvR <- lm(expr_lg_age ~ recv_lg_age, data = mullen)

################################################################################

################################################################################
# Score MCDI WS
################################################################################

# Section I.A

# Reduce to relevant variables
sent2 <- sent %>%
                select(demo.CandID, demo.age_bin, demo.age,
                       starts_with("sentences.I")) %>%
                pivot_longer(-c(demo.CandID, demo.age_bin, demo.age)) %>%
                mutate(name = gsub("sentences.", "", name))

# Now tabulate over sections/subsections
# We're only looking at section A, so filter over than and remove column
# section
sent2.I <- sent2 %>%
                  filter(grepl("^I_", name),
                         !grepl("_score", name)) %>%
                  separate(name, sep = "_",
                           into = c(NA, "section", "subsection", "question"),
                           convert = TRUE) %>%
                  filter(section == "A") %>%
                  select(-section) %>%
                  mutate(produces = score.produces(value, produces = "says"))

# Now group by ID and subsection and count the number of "says" (converted to
# T/F in previous step)
sent2.I.score <- sent2.I %>%
                        group_by(demo.CandID, demo.age_bin, subsection) %>%
                        summarise(n = n(), sum = sum(produces),
                                  perc = sum / n) %>%
                        mutate(subsection = as.factor(subsection))

# Assign labels to subsections
levels(sent2.I.score$subsection) <- c(lexical, syntactic)

# Spread wide
sent2.I.score <- sent2.I.score %>%
                        pivot_wider(c(demo.CandID, demo.age_bin),
                                    names_from = subsection,
                                    values_from = perc) %>%
                        ungroup()

lm(clothing ~ demo.age_bin, data = sent2.I.score)

# Part II

sent2.II <- sent2 %>%
                    filter(grepl("^II_", name),
                           !grepl("_score", name)) %>%
                    separate(name, sep = "_",
                             into = c(NA, "section", "question"),
                             convert = TRUE) %>%
                    filter(section %in% c("B", "C", "E"))

# B/C

sent2.II.BC <- sent2.II %>%
                      filter(section %in% LETTERS[2:3]) %>%
                      mutate(produces = score.produces(value,
                                                       produces = "says"))

sent2.II.BC.score <- sent2.II.BC %>%
                      group_by(demo.CandID, demo.age_bin) %>%
                      arrange(demo.CandID, demo.age_bin, section, question)

sent2.II.B.score <- sent2.II.BC.score %>%
                      filter(section == "B") %>%
                      add_column(class = if_else(.$question <= 5, "noun",
                                                 "verb"),
                                 .after = "question") %>%
                      group_by(demo.CandID, demo.age_bin, class) %>%
                      summarise(n = n(), sum = sum(produces),
                                perc = sum / n) %>%
                      pivot_wider(c(demo.CandID, demo.age_bin),
                                  names_from = class,
                                  values_from = perc) %>%
                      ungroup() %>%
                      rename(WORD_FORMS_NOUNS = noun,
                             WORD_FORMS_VERBS = verb)

sent2.II.C.score <- sent2.II.BC.score %>%
                      filter(section == "C") %>%
                      add_column(class = if_else(.$question <= 14, "noun",
                                                 "verb"),
                                 .after = "question") %>%
                      group_by(demo.CandID, demo.age_bin, class) %>%
                      summarise(n = n(), sum = sum(produces),
                                perc = sum / n) %>%
                      pivot_wider(c(demo.CandID, demo.age_bin),
                                  names_from = class,
                                  values_from = perc) %>%
                      ungroup() %>%
                      rename(WORD_ENDINGS_NOUNS = noun,
                             WORD_ENDINGS_VERBS = verb)

# E

sent2.II.E <- sent2.II %>%
                filter(section == "E") %>%
                mutate(complex = score.complexity(value, cx = "more_complex"))

sent2.II.E.score <- sent2.II.E %>%
                      group_by(demo.CandID, demo.age_bin) %>%
                      summarise(n = n(), cx = sum(complex),
                                COMPLEXITY = cx / n) %>%
                      select(-n, -cx) %>%
                      ungroup()

# Join together all scores
sent2.score <- left_join(sent2.I.score, sent2.II.B.score) %>%
                left_join(sent2.II.C.score) %>%
                left_join(sent2.II.E.score)

today <- format(Sys.Date(), "%y%m%d")
write_csv(sent2.score, path = paste0("BCP-scores-", today, ".csv"))

# Filter out everyone who is older than 30 months (the typical max age for
# MCDI-WS)
sent2.30mo <- sent2 %>%
                filter(demo.age_bin <= 30)

write_csv(sent2.30mo, path = paste0("BCP-30mo-scores-", today, ".csv"))

## extract MLU

sent2.MLU <- sent2 %>%
              filter(grepl("^II_D_1$", name))

write.csv(sent2.MLU, file = "mlu.csv")

################################################################################

# Basic plots

sent2.score.2 <- sent2.score %>%
                  mutate(lexical = rowMeans(.[, 3:17]),
                         syntactic = rowMeans(.[, 18:25]))

sent2.plot <- sent2.score.2 %>%
                select(demo.CandID, demo.age_bin,
                       starts_with("WORD"), COMPLEXITY,
                       lexical, syntactic) %>%
                pivot_longer(-starts_with("demo")) %>%
                mutate_at("name", as.factor)
levels(sent2.plot$name) <- c("Sentence complexity", "Lexical inventory",
                             "Syntactic inventory", "Correct irreg. (nouns)",
                             "Correct irreg. (verbs)",
                             "Overgeneralizations (nouns)",
                             "Overgeneralizations (verbs)")

ggplot(sent2.plot, aes(x = demo.age_bin, y = value, color = name)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = demo.CandID), alpha = 0.2) +
  facet_wrap(vars(name)) +
  geom_smooth() +
  theme(legend.position = "none")

################################################################################

mlu2 <- read_csv("data/MLU-fixed-200131.csv") %>%
          mutate(u1.length = sapply(strsplit(u1, " "), length),
                 u2.length = sapply(strsplit(u2, " "), length),
                 u3.length = sapply(strsplit(u3, " "), length)) %>%
          rowwise() %>%
          mutate(mlu = mean(c(u1.length, u2.length, u3.length), na.rm = TRUE),
                 mlu.sd = sd(c(u1.length, u2.length, u3.length), na.rm = TRUE),
                 n = 3 - sum(is.na(u1), is.na(u2), is.na(u3)),
                 mlu.se = mlu.sd / sqrt(n))


ggplot(mlu2, aes(x = demo.age_bin, y = mlu)) +
  geom_point() + 
  geom_errorbar(aes(ymin = mlu - mlu.se, ymax = mlu + mlu.se),
                width = 0.2) +
  geom_smooth()

lm(mlu~demo.age_bin, data = mlu2) %>% summary()

sent.mlu <- merge(sent2.score.2, mlu2[, c("demo.CandID", "demo.age_bin", 
                                          "mlu")])

ggplot(sent.mlu, aes(x = mlu)) +
  geom_point(aes(y = lexical), color= "blue") +
  geom_point(aes(y = syntactic), color = "red") +
  geom_smooth(aes(y = lexical), color= "blue", method = "lm") +
  geom_smooth(aes(y = syntactic), color = "red", method = "lm")

summary(lm(mlu ~ lexical, data = sent.mlu))
summary(lm(mlu ~ syntactic, data = sent.mlu))

ggplot(sent.mlu, aes(y = mlu)) +
  geom_point(aes(x = lexical), color= "blue") +
  geom_point(aes(x = syntactic), color = "red") +
  geom_smooth(aes(x = lexical), color= "blue", method = "loess") +
  geom_smooth(aes(x = syntactic), color = "red", method = "loess")

ggplot(sent2.score.2, aes(x = demo.age_bin, y = syntactic)) +
  geom_point() +
  geom_line(aes(group = demo.CandID)) +
  geom_smooth()

fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}

fit <- fit.gompertz(time = sent2.score.2$demo.age_bin, 
                    data = sent2.score.2$syntactic)

a  <- 1.40906
mu <- 0.06099
ld <- 21.8820

pred <- function(x) {
  return(a * exp(-exp(mu * exp(1)/a * (21.8820 - x) + 1)))
}

x <- seq(0, 40, by = .1)
y <- pred(x)

gompertz <- cbind(x, y) %>% as.data.frame()

ggplot(sent2.score.2, aes(x = demo.age_bin, y = syntactic)) +
  geom_point() +
  geom_line(data = gompertz, aes(x = x, y = y)) +
  scale_x_continuous(limits = c(0, 12 * 5)) 

AIC(fit)
AIC(lm(syntactic ~ demo.age_bin, data = sent2.score.2))

library(growthrates)

library(growthcurve)

myfit <- fit_growth_(sent2.score.2, time_col = "demo.age_bin", 
                    data_col = "syntactic", model = "logistic")

ggplot(sent2.score.2, aes(x = demo.age_bin, y = syntactic)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 12 * 5)) +
  stat_growthcurve()

