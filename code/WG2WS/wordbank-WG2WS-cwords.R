path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(MuMIn)

source("../mcdi-setup.R")

# Estimating connecting words ====

training <- readRDS("WS_training_subs.rds")
shared_words <- read_data("other/sharedwords.csv")

WS <- read_data("Wordbank/WS-230215.rds")
WS_demo <- read_data("Wordbank/WS-demographics-230215.rds") %>%
  select(data_id, age, sex)

cwords <- WS %>%
  filter(
    category == "connecting_words"
  ) %>%
  group_by(data_id) %>%
  summarize(
    connecting_words = sum(produces, na.rm = TRUE)
  )

WS %>%
  filter(
    data_id == "250183",
    category == "connecting_words"
  )

# The columns here are WG scores, except connecting words
WG2WS <- WS %>%
  filter(
    data_id %in% training,
    item_definition %in% shared_words$definition,
    item_kind == "word"
  ) %>%
  left_join(shared_words, by = c("item_definition" = "definition",
                                 "category" = "s.cat")) %>%
  group_by(data_id, g.cat) %>%
  summarize(
    # NAs are 0s
    WG_total = sum(produces, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = g.cat, values_from = WG_total) %>%
  ungroup() %>%
  mutate(
    WG_total = select(., all_of(unique(shared_words$g.cat))) %>%
      rowSums()
  ) %>%
  left_join(WS_demo, by = "data_id") %>%
  left_join(cwords, by = "data_id") %>%
  mutate(
    age_c = age - 18
  ) %>%
  select(-sex) %>%
  na.omit()

lm0_total <- lm(connecting_words ~ WG_total + I(WG_total^2) + I(WG_total^3),
                data = WG2WS)

AIC(lm0_total) # 17937
summary(lm0_total)$r.squared

lm1_age <- lm(connecting_words ~ WG_total + I(WG_total^2) + I(WG_total^3) +
                age_c*WG_total + I(age_c^2)*I(WG_total^2) +
                I(age_c^3)*I(WG_total^3),
              data = WG2WS)

AIC(lm1_age)  # 17770
summary(lm1_age)$r.squared

lm2_cats <- lm(connecting_words ~ WG_total + I(WG_total^2) + I(WG_total^3) +
                  age_c*WG_total + I(age_c^2)*I(WG_total^2) +
                  I(age_c^3)*I(WG_total^3) +
                  action_words + animals + body_parts + clothing +
                  descriptive_words + food_drink + furniture_rooms +
                  games_routines + household + locations + outside + people +
                  pronouns + quantifiers + question_words + sounds +
                  time_words + toys + vehicles,
              data = WG2WS)

AIC(lm2_cats)  # 16011
summary(lm2_cats)$r.squared

anova(lm0_total, lm1_age, lm2_cats)

n_coef <- length(coef(lm2_cats))

summary(lm2_cats)$coefficients %>%
  as_tibble(rownames = "coef") %>%
  filter(
    `Pr(>|t|)` < (.05 / n_coef)
  )

# Terms sig at p < .05
lm3_cats_sigterms <- lm(connecting_words ~ WG_total + I(WG_total^2) +
                          I(WG_total^3) +
                          action_words + clothing + food_drink +
                          games_routines + locations + pronouns + quantifiers +
                          question_words + sounds + time_words + toys +
                          WG_total:age_c + I(WG_total^2):I(age_c^2) +
                          I(WG_total^3):I(age_c^3),
                        data = WG2WS)

# Terms sig at p < (.05 / n_coef); n_coef = 29, .05 / 29 = 0.0017
lm3_cats_sigterms2 <- lm(connecting_words ~ WG_total + I(WG_total^2) +
                           I(WG_total^3)*I(age_c^3) +
                           quantifiers + question_words + sounds + time_words,
                        data = WG2WS)

AICc(lm3_cats_sigterms)  # 16009
AICc(lm3_cats_sigterms2)  # 16089
summary(lm3_cats_sigterms2)$r.squared

saveRDS(lm3_cats_sigterms, "cwords_model.rds")

# Test ====

WG2WS_test <- WS %>%
  filter(
    !(data_id %in% training),
    item_definition %in% shared_words$definition,
    item_kind == "word"
  ) %>%
  left_join(shared_words, by = c("item_definition" = "definition",
                                 "category" = "s.cat")) %>%
  group_by(data_id, g.cat) %>%
  summarize(
    WG_total = sum(produces, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = g.cat, values_from = WG_total) %>%
  ungroup() %>%
  mutate(
    WG_total = select(., all_of(unique(shared_words$g.cat))) %>%
      rowSums()
  ) %>%
  left_join(WS_demo, by = "data_id") %>%
  left_join(cwords, by = "data_id") %>%
  mutate(
    age_c = age - 18
  )

estimate_cwords <- function(data, model) {

  cwords_hat <- predict(model, data) %>%
    round() %>%
    unname()

  cwords_hat <- if_else(cwords_hat < 0, 0, cwords_hat)
  cwords_hat <- if_else(cwords_hat > 6, 6, cwords_hat)

  return(cwords_hat)

}

WG2WS_test$cwords_hat <- estimate_cwords(WG2WS_test, lm3_cats_sigterms2)

saveRDS(WG2WS_test, "WG2WS_test_cwords.rds")

ggplot(WG2WS_test, aes(x = connecting_words, y = cwords_hat)) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(limits = c(-0.5, 6), breaks = 0:6) +
  labs(x = "True CW score",
       y = "Predicted CW score") +
  theme_bw()

# Extract error column in order to calculate rmse by hand
cwords_err <- WG2WS_test %>%
  mutate(
    error = cwords_hat - connecting_words
  ) %>%
  pull(error)

# Calculate RMSE
cwords_test_rmse <- sqrt(sum(cwords_err**2) / length(cwords_err))

## ICC ====

irr::icc(select(WG2WS_test, cwords_hat, connecting_words)) # 0.816
