setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/growth_curving/")

library(tidyverse)

source("growth-curve-functions.R")

solve.gomp2 <- function(x = NA, y = NA, k_g, A = 681) {
  W_0 <- .Machine$double.eps

  if (is.na(y) & !is.na(x))
    result <- W_0 * (A / W_0) ^ (1 - exp(-k_g * x))
  else if (is.na(x) & !is.na(y))
    result <- -log(1 - log(y / W_0) / log(A / W_0)) / k_g
  else
    message("Exactly one of x/y must be specified")
  return(result)

}

##### Load data

cat_n <- read_csv("../../data/other/s_dict.csv") %>%
  mutate(
    lex_cat = case_when(
                category %in% c("action_words", "helping_verbs")  ~ "verb",
                category == "descriptive_words" ~ "adjectives",
                category %in% c("time_words", "pronouns", "question_words",
                            "locations", "quantifiers", "connecting words") ~
                  "functional",
                TRUE ~ "noun"
              )
  ) %>%
  group_by(lex_cat) %>%
  summarize(
    lex_cat_total = n()
  )

bcp_ws <- readRDS("../../data/BCP/BCP_WS_scored-200609.rds")$n %>%
  select(-matches("^[A-Z]", ignore.case = FALSE), -sounds) %>%
  pivot_longer(-c(data_id, age))

bcp_wg <- readRDS("../../data/BCP/BCP_WG_asWS-200609.rds") %>%
  select(-matches("^[A-Z]", ignore.case = FALSE), -sounds) %>%
  pivot_longer(-c(data_id, age))

bcp <- bind_rows(bcp_wg, bcp_ws) %>%
  mutate(
    lex_cat = case_when(
                  name %in% c("action_words", "helping_verbs")  ~ "verb",
                  name == "descriptive_words" ~ "adjectives",
                  name %in% c("time_words", "pronouns", "question_words",
                              "locations", "quantifiers", "connecting words") ~
                    "functional",
                  TRUE ~ "noun"
                )
  )

bcp_all <- bcp %>%
  group_by(data_id, age) %>%
  summarize(
    TOTAL = sum(value)
  ) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    max = map_dbl(data, ~max(.x$TOTAL))
  ) %>%
  filter(
    n >= 3,
    max >= (680 - 12) / exp(1)
  ) %>%
  mutate(
    fit = map(data, ~gomp2.fit(.x, response_var = "TOTAL", max = 680 - 12)),
    kg  = extract.kg(fit),
    Ti  = solve.gomp2(y = 1 / exp(1), k_g = kg, A = 1)
  )

bcp_lc <- bcp %>%
  group_by(data_id, age, lex_cat) %>%
  summarize(
     n = sum(value)
  ) %>%
  left_join(cat_n) %>%
  mutate(
    p = n / lex_cat_total
  )

ggplot(bcp_lc, aes(x = age, y = p)) +
  geom_line(aes(group = data_id)) +
  facet_wrap(vars(lex_cat))

ggplot(bcp_lc, aes(x = age, y = p, color = lex_cat)) +
  geom_line(aes(group = interaction(data_id, lex_cat)), alpha = 0.1) +
  geom_smooth(se = FALSE) +
  theme_bw()

bcp_lc_models <- bcp_lc %>%
  group_by(data_id, lex_cat) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    max_val = map_dbl(data, ~max(.x$p))
  ) %>%
  filter(
    n >= 3,
    max_val > 1 / exp(1)
  ) %>%
  mutate(
    fit = map(data, ~gomp2.fit(.x, response_var = "p", max = 1)),
    kg  = extract.kg(fit),
    Ti  = solve.gomp2(y = 1 / exp(1), k_g = kg, A = 1)
  )

bcp_lc_wide <- bcp_lc_models %>%
  select(data_id, lex_cat, Ti) %>%
  pivot_wider(names_from = lex_cat, values_from = Ti)

bcp_lc_wide %>%
  ungroup() %>%
  select(-data_id) %>%
  cor(use = "c")

ggplot(bcp_lc_wide, aes(x = Ti, fill = lex_cat)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(NA, 40)) +
  theme_bw()

ggplot(bcp_lc_models, aes(y = as.character(data_id), x = Ti, color = lex_cat)) +
  geom_point() +
  theme_bw()

##### All words vs. lexical categories

BCP_timing <- bcp_all %>%
  select(data_id, Ti) %>%
  rename(
    all_words_T = Ti
  ) %>%
  left_join(bcp_lc_wide) %>%
  pivot_longer(-c(data_id, all_words_T)) %>%
  mutate(
    diff = value - all_words_T
  )

ggplot(BCP_timing, aes(y = diff, x = name)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-2.5, 10)) +
  theme_bw()
