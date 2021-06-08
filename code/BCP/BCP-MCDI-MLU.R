if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/BCP")
}

# Libraries

library(tidyverse)
library(lme4)
library(MuMIn)
library(viridis)

select <- dplyr::select

# Helper functions
# source("wordbank-functions.R")
# source("format-BCP-funcs.R")

source("../mcdi-setup.R")

date <- "200609"

# Load data

#BCP_WG_scored <- readRDS("data/BCP_WG_scored.rds")
BCP_WS_scored <- read_data(paste0("BCP/BCP_WS_scored-", date, ".rds"))[[1]] %>%
  mutate(
    LEX = action_words + animals + body_parts + clothing + descriptive_words +
      food_drink + furniture_rooms + games_routines + helping_verbs +
      household + outside + people + places + toys + vehicles + sounds,

    SYN = pronouns + quantifiers + question_words + time_words +
      WORD_ENDINGS_NOUNS + WORD_ENDINGS_VERBS + WORD_FORMS_NOUNS +
      WORD_FORMS_VERBS + COMPLEXITY + connecting_words + locations,

    partI = action_words + animals + body_parts + clothing + descriptive_words +
      food_drink + furniture_rooms + games_routines + helping_verbs +
      household + outside + people + places + toys + vehicles + sounds +
      pronouns + quantifiers + question_words + time_words + connecting_words +
      locations,

    partII = WORD_ENDINGS_NOUNS + WORD_ENDINGS_VERBS + WORD_FORMS_NOUNS +
      WORD_FORMS_VERBS + COMPLEXITY
  )

# Read demographics file (just mom ed)
BCP.demographics <- read_data(paste0("BCP/BCP-demographics-", date, ".csv")) %>%
  select(CandID, sex, educ_momed_n)

BCP_MLU <- read_data(paste0("BCP/BCP_WS_MLU3-", date, ".rds"))

wMLU <- right_join(BCP_WS_scored, BCP_MLU) %>%
  left_join(., BCP.demographics, by = c("data_id" = "CandID"))

ggplot(wMLU, aes(x = MLU3w, y = MLU3m, color = age)) +
  geom_point() +
  geom_smooth() +
  geom_line(aes(group = data_id)) +
  scale_color_viridis() +
  labs(x = "Words", y = "Morphemes")

#
#
#

#
# Compare my methods
#

# Main model

model.lex_m <- lmer(MLU3m ~ LEX + (1|data_id), data = wMLU, REML = TRUE)
model.syn_m = lmer(MLU3m ~ SYN + (1|data_id), data = wMLU, REML = TRUE)

AICc(model.lex_m) # 462
AICc(model.syn_m) # 435   diff = 27

# aux: words

model.lex_w <- lmer(MLU3w ~ LEX + (1|data_id), data = wMLU, REML = TRUE)
model.syn_w <- lmer(MLU3w ~ SYN + (1|data_id), data = wMLU, REML = TRUE)

AICc(model.lex_w) # 455
AICc(moedl.syn_w) # 429   diff = 25       W<M = 6

# Control model

model.lex_m_ctrl <- lmer(MLU3m ~ LEX + age + sex + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)
model.syn_m_ctrl <- lmer(MLU3m ~ SYN + age + sex + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)

model.lex_w_ctrl <- lmer(MLU3w ~ LEX + age + sex + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)
model.syn_w_ctrl <- lmer(MLU3w ~ SYN + age + sex + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)

model.lex_m_ctrl2 <- lmer(MLU3m ~ LEX + age + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)
model.syn_m_ctrl2 <- lmer(MLU3m ~ SYN + age + mom_ed_n + (1|data_id),
                    data = wMLU, REML = TRUE)

model.lex_w_ctrl2 <- lmer(MLU3w ~ LEX + age + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)
model.syn_w_ctrl2 <- lmer(MLU3w ~ SYN + age + mom_ed_n + (1|data_id),
                          data = wMLU, REML = TRUE)

AICc(model.lex_m_ctrl) # 454
AICc(model.syn_m_ctrl) # 435  diff = 19

AICc(model.lex_w_ctrl) # 448
AICc(model.syn_w_ctrl) # 430  diff = 18   W<M = 5

# --- incorrect models proposed in registration, the only one that changes is
#      MLUm/syntax due to rounding

AICc(model.lex_m_ctrl2) # 454
AICc(model.syn_m_ctrl2) # 434  diff = 20

AICc(model.lex_w_ctrl2) # 448
AICc(model.syn_w_ctrl2) # 430  diff = 18   W<M = 5

#
#
#

#
# Compare with MCDI categories
#

# mcdi categories are word_* + complexity vs. all others

m.partI  <- lmer(MLU3m ~ partI + (1|data_id), data = wMLU, REML = TRUE)
m.partII <- lmer(MLU3m ~ partII + (1|data_id), data = wMLU, REML = TRUE)
w.partI  <- lmer(MLU3w ~ partI + (1|data_id), data = wMLU, REML = TRUE)
w.partII <- lmer(MLU3w ~ partII + (1|data_id), data = wMLU, REML = TRUE)

AICc(m.partI)   # 458
AICc(m.partII)  # 438
AICc(w.partI)   # 451
AICc(w.partII)  # 431

m.partI.ctrl  <- lmer(MLU3m ~ partI + age + sex + mom_ed_n + (1|data_id),
                      data = wMLU, REML = TRUE)
m.partII.ctrl <- lmer(MLU3m ~ partII + age + sex + mom_ed_n +(1|data_id),
                      data = wMLU, REML = TRUE)
w.partI.ctrl  <- lmer(MLU3w ~ partI + age + sex + mom_ed_n +(1|data_id),
                      data = wMLU, REML = TRUE)
w.partII.ctrl <- lmer(MLU3w ~ partII + age + sex + mom_ed_n +(1|data_id),
                      data = wMLU, REML = TRUE)

AICc(m.partI.ctrl)   # 452  W<M = 6
AICc(m.partII.ctrl)  # 436  W<M = 2
AICc(w.partI.ctrl)   # 446  W<M = 5
AICc(w.partII.ctrl)  # 431  W<M = 0

wMLU %>%
  select(-sex) %>%
  summarize_all(list(M = mean, SD = sd)) %>% round(1) %>% t()

#
# Try it with a regular ANOVA lol
#

# Demographic predictors
demo.predictors <- c("age", "sex", "mom_ed_n")

# All part I/lexical predictors
lexical.predictors <- c("action_words", "animals", "body_parts", "clothing",
                        "descriptive_words", "food_drink", "furniture_rooms",
                        "games_routines", "helping_verbs", "household",
                        "outside", "people", "places", "toys", "vehicles",
                        "sounds")

# Predictors in Part II, but also syntax
swap.predictors <- c("pronouns ", "quantifiers", "question_words", "time_words",
                      "word_endings_nouns", "connecting_words", "locations")

# Part II/syntax
partII.predictors <- c("word_endings_verbs", "word_forms_nouns",
                        "word_forms_verbs", "complexity")

## Models

# Lexical

formula.lex <- paste("MLU3m ~ ", paste(lexical.predictors, collapse = " + "),
                     "+ (1|data_id)")

formula.lex_ctrl <- paste("MLU3m ~ ",
                          paste(c(demo.predictors, lexical.predictors),
                                collapse = " + "),
                          "+ (1|data_id)")

model2.lex <- lmer(formula = formula.lex, data = wMLU, REML = TRUE)
model2.lex_ctrl <- lmer(formula = formula.lex_ctrl, data = wMLU, REML = TRUE)

# Adding in demographic controls improves test, so we always use control in
# syntax comparisons
anova(model2.lex, model2.lex_ctrl)

# Syntactic

formula.syntax_partII <- paste("MLU3m ~ ",
                                paste(c(demo.predictors, partII.predictors),
                                      collapse = " + "),
                                "+ (1|data_id)")

formula.syntax_SYN <- paste("MLU3m ~",
                            paste(c(demo.predictors, swap.predictors,
                                    partII.predictors),
                                  collapse = " + "),
                            "+ (1|data_id)")

syntax1 <- lmer(formula = formula.syntax_partII, data = wMLU, REML = TRUE)
syntax2 <- lmer(formula = formula.syntax_SYN, data = wMLU, REML = TRUE)

# Adding additional items DOES NOT improve estimation of MLU3m
anova(syntax1, syntax2)

###

wMLU.long <- wMLU %>%
  select(data_id, age, LEX, SYN, MLU3m) %>%
  mutate_at(c("LEX", "SYN", "MLU3m"), scale) %>%
  pivot_longer(-c(data_id, age))

ggplot(wMLU.long, aes(value, fill = name)) +
  geom_density(alpha = 0.25)

ggplot(wMLU, aes(x = age, y  = MLU3m)) +
  geom_point() +
  geom_line(aes(group = data_id))

