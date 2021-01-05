if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
}

library(tidyverse)

source("../mcdi-setup.R")
source("../growth_curving/growth-curve-functions.R")

all <- read_csv(.data("other/test-subjs.csv"))

# Load data as a list
fit_all <- all %>%
  arrange(data_id, age) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    fit = map(data, ~gomp2.fit(.x)),
    best_kg  = extract.kg(fit)
  ) %>%
  arrange(best_kg) %>%
  ungroup() %>%
  mutate(
    order = row_number()
  )


test_all <- all %>%
  arrange(data_id, age) %>%
  mutate(
    lower = ifelse(age < 25, "lower", "upper")
  ) %>%
  group_by(data_id, lower) %>%
  # Nest data frames for ages lt 25 and greater than
  nest() %>%
  # Pivot wider so data is in two columns
  pivot_wider(id_cols = "data_id", names_from = "lower",
              values_from = "data") %>%
  # Remove upper missing (no ages > 25)
  mutate(
    x = map(upper, typeof)
  ) %>%
  filter(x == "list") %>%
  dplyr::select(-x) %>%
  # Rename in dataframes
  mutate(
    lower = map(lower,
                ~rename(.x, lower_age = age, lower_inventory = inventory)),
    upper = map(upper,
                ~rename(.x, upper_age = age, upper_inventory = inventory))
  ) %>%
  # Unnest so each row is each combination of upper/lower ages
  unnest(cols = c(lower, upper))

# Renest for each combo
test_all_renest <- test_all %>%
  mutate(
    row = row_number()
  ) %>%
  group_by(data_id, row) %>%
  nest() %>%
  mutate(
    data2 = map(data, ~tibble(age = c(.x$lower_age, .x$upper_age),
                              inv = c(.x$lower_inventory, .x$upper_inventory))),
    fit = map(data2, ~gomp2.fit(.x, response_var = "inv", max.iter = 500)),
    kg  = extract.kg(fit)
  ) %>%
  ungroup()

plot_me <- fit_all %>%
  select(data_id, order, best_kg) %>%
  full_join(select(test_all_renest, data_id, kg))

ggplot(plot_me, aes(x = order)) +
  geom_point(aes(y = kg * 251), alpha = 0.5) +
  geom_smooth(aes(y = kg * 251), alpha = 0.5, color = "black", se = FALSE) +
  geom_point(aes(y = best_kg * 251), color = "red") +
  geom_smooth(aes(y = best_kg * 251), alpha = 0.5, color = "red", se = FALSE) +
  labs(x = "Index", y = "Growth rate (w/mo)") +
  scale_x_continuous(breaks = seq(1, 59, 2))

max_var <- 758603
min_var <- plot_me$data_id[plot_me$order == 27] %>% unique()

max_df <- test_all_renest %>%
  filter(data_id == max_var) %>%
  mutate(
    curve = calculate.curves(kg, max = 681)
  )

max_df_unnest <- max_df %>%
  select(-data) %>%
  unnest(cols = data2) %>%
  select(data_id, age, inv) %>%
  distinct()

ggplot(max_df_unnest, aes(x = age, y = inv, color = age < 25)) +
  geom_point() +
  scale_x_continuous(limits = c(10, 40), breaks = 10:40) +
  scale_y_continuous(limits = c(0, 700)) +
  max_df$curve[1] +
  max_df$curve[2] +
  max_df$curve[3] +
  max_df$curve[4] +
  geom_hline(yintercept = 251, color = "red") +
  labs(x = "Month", y = "Words", title = "758603: high variability in kg")

min_df <- test_all_renest %>%
  filter(data_id == min_var) %>%
  mutate(
    curve = calculate.curves(kg, max = 681)
  )

min_df_unnest <- min_df %>%
  select(-data) %>%
  unnest(cols = data2) %>%
  select(data_id, age, inv) %>%
  distinct()

ggplot(min_df_unnest, aes(x = age, y = inv, color = age < 25)) +
  geom_point() +
  scale_x_continuous(limits = c(10, 40), breaks = 10:40) +
  scale_y_continuous(limits = c(0, 700)) +
  min_df$curve[1] +
  min_df$curve[2] +
  min_df$curve[3] +
  min_df$curve[4] +
  geom_hline(yintercept = 251, color = "red") +
  labs(x = "Month", y = "Words",
       title = paste(min_var, "low variability in kg"))

ggplot(all, aes(x = age, y = inventory)) +
  geom_line(aes(group = data_id))

