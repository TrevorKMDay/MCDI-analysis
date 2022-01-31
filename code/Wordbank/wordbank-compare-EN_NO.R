setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")

library(tidyverse)


source("../mcdi-setup.R")
source("wordbank-functions.R")

english <- read_data("Wordbank/WS-scored.rds")$p %>%
  mutate(
    lg = "English"
  )


norsk   <- (read_data("Wordbank/Wordbank-Norwegian-WS-210812.csv") %>%
  score.WS(include.totals = TRUE))$p %>%
  mutate(
    data_id = as.factor(data_id),
    lg = "Norsk"
  )

mean(english$age)
mean(norsk$age)
mean(c(english$age, norsk$age))

english2 <- lm(SYNTAX ~ age + I(age^2) + LEXICAL + I(LEXICAL^2), data = english)
norsk2 <- lm(SYNTAX ~ age + I(age^2) + LEXICAL + I(LEXICAL^2), data = norsk)

steps <- tibble(
    age = mean(c(english$age, norsk$age)),
    LEXICAL = seq(0, 1, by = 0.1),
  ) %>%
  mutate(
    english_SYNTAX = predict(english2, .),
    norsk_SYNTAX = predict(norsk2, .)
  )

steps_long <- steps %>%
  pivot_longer(ends_with("SYNTAX"), names_to = "lg", values_to = "SYNTAX") %>%
  separate(lg, into = c("lg", NA))

ggplot(steps_long, aes(x = LEXICAL, y = SYNTAX, color = lg)) +
  geom_point() +
  geom_line()

ribbon <- bind_rows(english, norsk) %>%
  select(lg, data_id, age, SYNTAX, LEXICAL) %>%
  group_by(lg, age) %>%
  summarize(
    across(c(SYNTAX, LEXICAL), .fns = list(m = mean, sd = sd, iqr = IQR))
  ) %>%
  pivot_longer(-c(lg, age)) %>%
  separate(name, into = c("score", "stat")) %>%
  pivot_wider(values_from = value, names_from = stat)

ggplot(ribbon, aes(x = age, y = m, color = lg)) +
  geom_line(aes(linetype = lg), size = 1) +
  geom_ribbon(aes(ymin = m - 0.5 * iqr, ymax = m + 0.5 * iqr, fill = lg),
              alpha = 0.3) +
  geom_ribbon(aes(ymin = m - 1.5 * iqr, ymax = m + 1.5 * iqr, fill = lg),
              alpha = 0.3) +
  facet_grid(rows = vars(score)) +
  theme_bw()
