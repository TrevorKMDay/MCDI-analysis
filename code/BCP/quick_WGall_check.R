library(tidyverse)

summarize <- dplyr::summarize

ws <- read_csv("BCP_WS-260923.csv", show_col_types = FALSE)
wg_all <- read_csv("BCP_WG_all-260923.csv", show_col_types = FALSE)

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
