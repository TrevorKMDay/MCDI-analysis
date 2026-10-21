library(tidyverse)

setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/BCP")
summarize <- dplyr::summarize

ws <- read_csv("BCP_WS-260923.csv", show_col_types = FALSE)
wg_all <- read_csv("BCP_WG_all-261021.csv", show_col_types = FALSE)

by_cat <- wg_all %>%
    mutate(
        yesno = if_else(value %in% c("never", "no", "no_yet") | is.na(value),
                        "no", "yes")
    ) %>%
    group_by(data_id, age, sex, item_kind) %>%
    summarize(
        n = n(),
        yes = sum(yesno == "yes"),
        .groups = "drop_last"
    ) %>%
    mutate(
        p = yes / n
    )

ggplot(by_cat, aes(x = age, y = p)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(group = data_id), alpha = 0.1) +
    geom_smooth(method = "lm") +
    facet_wrap(vars(item_kind)) +
    theme_bw()

wg_subs <- length(unique(wg_all$data_id))

wg_visits <- wg_all %>%
    select(data_id, age) %>%
    distinct() %>%
    nrow()

all_subs <- length(unique(c(ws$data_id, wg_all$data_id)))

# Sanity check

by_word_old <- read_csv("BCP_WG_all-261021.csv", show_col_types = FALSE) %>%
  filter(
    item_kind == "word"
  ) %>%
  group_by(item_definition, item_kind, category) %>%
  summarize(
    n = n(),
    prod = sum(value == "produces", na.rm = TRUE),
    und = sum(value == "understands", na.rm = TRUE)
  ) %>%
  mutate(
    across(c(prod, und), ~ .x / n, .names = "{.col}_pct")
  ) %>%
  arrange(desc(prod_pct))

by_word <- wg_all %>%
  filter(
    item_kind == "word"
  ) %>%
  group_by(item_definition, item_kind, category) %>%
  summarize(
    n = n(),
    prod = sum(value == "produces", na.rm = TRUE),
    und = sum(value == "understands", na.rm = TRUE)
  ) %>%
  mutate(
    across(c(prod, und), ~ .x / n, .names = "{.col}_pct")
  ) %>%
  arrange(desc(prod_pct))

comp <- left_join(by_word_old, by_word, join_by(item_definition))

comp %>%
    filter(category.x != category.y)
comp %>%
    filter(prod_pct.x != prod_pct.y)

old <- read_csv("BCP_WG_all-260923.csv", show_col_types = FALSE)

comp_rows <- left_join(wg_all, old, by = join_by(data_id, age, sex, item_id))

diff_cat <- comp_rows %>%
    filter(
        category.x != category.y
    )

diff_def <- comp_rows %>%
    filter(
        item_definition.x != item_definition.y
    )

diff_value <- comp_rows %>%
    mutate(
        across(starts_with("value"), ~replace_na(.x, "MISSING"))
    ) %>%
    filter(
        value.x != value.y,
        item_kind.x != item_kind.y
    )

nrow(diff_value) / nrow(wg_all)

diff_value_items <- unique(diff_value$item_id)

diff_value_total <- diff_value %>%
    group_by(item_id) %>%
    nest()

diff_cat_total <- diff_value %>%
    group_by(category.x) %>%
    nest()
