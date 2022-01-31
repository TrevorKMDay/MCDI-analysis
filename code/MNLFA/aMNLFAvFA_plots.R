
library(tidyverse)
source("../mcdi-setup.R")

DIF <- readRDS("lex_syn_DIF.rds") %>%
  filter(
    place == "load"
  ) %>%
  select(-p, -place)

loadings <- DIF %>%
  filter(
    var == "intercept"
  ) %>%
  select(-var, -est_se) %>%
  group_by(score) %>%
  nest() %>%
  mutate(
    data = map(data, ~mutate(.x, est_Z = scale(est),
                             est_max = est / max(est, na.rm = TRUE),
                             est_p = est / sum(abs(est), na.rm = TRUE)))
  ) %>%
  unnest(data)

DIF_wide <- DIF %>%
  select(-est_se) %>%
  pivot_wider(id_cols = c(score, label, lab2), names_from = var,
              values_from = c(est, se)) %>%
  arrange(est_intercept) %>%
  mutate(
    index = str_pad(row_number(), width = 2, "left", "0")
  )

##############################################################################

syn_size <- tibble(
  mplus_label = c("WE_N", "WE_V", "WF_N", "WF_V", "COMPLX"),
  n           = c(14,     31,     5,      2,      37),
  category    = c("WORD_ENDINGS_NOUNS", "WORD_ENDINGS_VERBS",
                  "WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "COMPLEXITY")
)

inv_size <- read_data("other/s_dict.csv") %>%
  group_by(category) %>%
  summarize(n = n()) %>%
  add_column(
    mplus_label = c("ACTION", "ANIMAL", "BODYP", "CLOTH", "CONJ", "DESCRB",
                    "FOOD", "ROOMS", "GAMES", "HELPV", "HOUSEH", "LOCATE",
                    "OUTSDE", "PEOPLE", "PLACES", "PRON", "QUANT", "QWORDS",
                    "SOUNDS", "TIME", "TOYS", "VEHICL"),
  )

sizes <- bind_rows(inv_size, syn_size)

source("../mcdi-setup.R")

ws <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(-data_id, -age, -LEXICAL, -SYNTAX)

fa_weights <- readRDS(.data("Wordbank/WS-FA_scores.rds"))$weights %>%
  as_tibble() %>%
  rename(
    lex = MR1,
    syn = MR2
  ) %>%
  add_column(var = colnames(ws), .before = TRUE) %>%
  left_join(sizes, by = c("var" = "category")) %>%
  left_join(distinct(select(DIF, lab2, score)), by = c("mplus_label" = 'lab2')) %>%
  pivot_longer(-c(var, mplus_label, score, n), values_to = "fa_raw") %>%
  filter(
    score == name
  ) %>%
  group_by(score) %>%
  mutate(
    fa_Z = scale(fa_raw),
    fa_max = fa_raw / max(fa_raw),
    fa_p = fa_raw / sum(fa_raw)
  )

amnlfa <- readRDS("lex_and_syn.rds") %>%
  select(ID, AGE, ETA_L, ETA_S)

fa_scores <- readRDS(.data("Wordbank/WS-FA_scores.rds"))$scores %>%
  as_tibble() %>%
  rename(
    lex = MR1,
    syn = MR2
  ) %>%
  bind_cols(select(read_data("Wordbank/WS-scored.rds")$n, data_id, age))

amnlfa_fa_scores <- left_join(amnlfa, fa_scores,
                              by = c("ID" = "data_id", "AGE" = "age")) %>%
  rename(
    eta_lex = ETA_L,
    eta_syn = ETA_S,
    fa_lex = lex,
    fa_syn = syn
  ) %>%
  pivot_longer(-c(ID, AGE)) %>%
  separate(name, into = c("model", "score")) %>%
  pivot_wider(names_from = model)

ggplot(amnlfa_fa_scores, aes(x = fa, y = eta, color = AGE)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_color_viridis() +
  facet_wrap(vars(score), scales = "free") +
  theme_bw()

##############################################################################

loadings2 <- left_join(loadings, fa_weights,
                       by = c("lab2" = "mplus_label", "score")) %>%
  rename(
    est_raw = est
  ) %>%
  arrange(est_raw) %>%
  mutate(
    index = str_pad(row_number(), 2, "left", 0)
  ) %>%
  select(c(score, index, lab2), ends_with("raw"), ends_with("Z"),
         ends_with("max"), ends_with("p"))

loadings2w <- loadings2 %>%
  pivot_longer(-c(score, index, lab2)) %>%
  separate(name, into = c("estimate", "comp_method"))

loadings2n <- loadings2w %>%
  pivot_wider(names_from = estimate, values_from = value)%>%
  group_by(comp_method) %>%
  nest() %>%
  mutate(
    rp = map(data, ~cor(.x$est, .x$fa, use = "c") %>%
                      round(3)),
    rs = map(data, ~cor(.x$est, .x$fa, use = "c", method = "s") %>%
                      round(3))
  )

ggplot(loadings2w, aes(x = lab2, y = value, fill = estimate)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_wrap(vars(comp_method), scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(loadings2n$data[[2]], aes(x = est, y = fa)) +
  geom_text(aes(label = lab2))
