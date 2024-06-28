path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(MuMIn)

library(viridis)
# library(ggpattern)

source("../mcdi-setup.R")
source("../Wordbank/wordbank-functions.R")

dir.create("WG2WS_plots", showWarnings = FALSE)

# Load data ====

# Load in data
WS <- read_data("Wordbank/WS-230215.rds")
# WG <- read_data("Wordbank/WG-230215.rds")


# We only need ages for the WS so no need to load WG demographics
WS_demo <- read_data("Wordbank/WS-demographics-230215.rds") %>%
  select(data_id, age, sex)

# Words shared between forms
shared_words <- read_data("other/sharedwords.csv")
shared_words %>%
  filter(
    g.cat != s.cat
  )

## Definitions ====

s_dict <- read_data("other/s_dict.csv")
g_dict <- read_data("other/g_dict.csv")

dict <- full_join(s_dict, g_dict, by = "item_definition",
                  suffix = c(".s", ".g")) %>%
  mutate(
    inWG = !is.na(category.g)
  )

dict %>%
  filter(
    str_detect(item_definition, "^in")
  )

dict %>%
  filter(
    category.s != category.g
  )



syn_cats <- (s_dict %>%
  pull(category) %>%
  unique())[16:22]

# Total in each named category according to WS assignment
cat_totals <- dict %>%
  group_by(category.s) %>%
  summarize(
    n_WS = n(),
    n_WG = sum(inWG)
  ) %>%
  rename(
    category = category.s
  )

syn_cats_total <- cat_totals %>%
  filter(
    category %in% syn_cats
  ) %>%
  pull(n_WS) %>%
  sum()

# WG words not in WS: only in, inside
g_dict$item_definition[!(g_dict$item_definition %in% s_dict$item_definition)]

## Select training and test participants ====

# UMN zip code
set.seed(55455)

# 7,905 individuals with ages, select 25% from each age group: 1,970 test
WS_test <- WS %>%
  left_join(WS_demo) %>%
  select(data_id, age) %>%
  distinct() %>%
  filter(
    !is.na(age)
  ) %>%
  group_by(age) %>%
  slice_sample(prop = 0.25) %>%
  ungroup() %>%
  pull(data_id)

# Everyone else: 6,883 training individuals
WS_training <- unique(WS$data_id)[!(unique(WS$data_id) %in% WS_test)]

saveRDS(WS_training, "WS_training_subs.rds")

if (!file.exists("WS_words2.rds")) {

  WS_words <- WS %>%
    filter(
      data_id %in% WS_training
    ) %>%
    select(-understands, -language, -form, -english_gloss, -uni_lemma,
           -ends_with("_category")) %>%
    filter(
      item_kind == "word"
    ) %>%
    mutate(
      inWG = item_definition %in% g_dict$item_definition |
        item_definition %in% c("in", "inside"),
      # Score WS inside/in as 2 WG points, all other produces as 1, and
      #   empty strings (and "understands" ?) as 0, leave NAs as is
      value = case_when(
        value == "produces" & item_definition == "inside/in" ~ 2,
        value == "produces" ~ 1,
        value %in% c("understands", "") ~ 0
      )
    ) %>%
    left_join(shared_words, by = c("item_definition" = "definition"))

  ## Calculate category totals ====

  # Total score of only items that appear on WG; but grouped by the WS category
  wg_total <- WS_words %>%
    group_by(data_id, s.cat) %>%
    filter(
      !is.na(s.cat)
    ) %>%
    summarize(
      wg_total = sum(value[inWG])
    )

  ws_total <- WS_words %>%
    group_by(data_id, category) %>%
    summarize(
      # Group by category, not s.cat because s.cat is missing
      ws_total = sum(value)
    )

  WS_words2 <- full_join(ws_total, wg_total,
                         by = c("data_id", "category" = "s.cat"))

  saveRDS(WS_words2, "WS_words2.rds")

  rm(WS_words, wg_total, ws_total)

} else {
  WS_words2 <- readRDS("WS_words2.rds")
}

length(unique(WS_words2$data_id))
length(unique(WS_words2$category))

## Create test set ====

# Test model ====

WS_cats_test <- WS %>%
  select(-understands, -language, -form, -english_gloss, -uni_lemma,
         -ends_with("_category")) %>%
  filter(
    item_kind == "word",
    data_id %in% WS_test
  ) %>%
  mutate(
    inWG = item_definition %in% g_dict$item_definition,
    value = case_when(
      value == "produces" & item_definition == "inside/in" ~ 2,
      value == "produces" ~ 1,
      value %in% c("understands", "") ~ 0
    )
  ) %>%
  group_by(data_id, category) %>%
  nest() %>%
  mutate(
    cat_total_WS = map_dbl(data, ~sum(.x$value, na.rm = TRUE)),
    cat_total_WG = map_dbl(data, ~sum(.x$value[.x$inWG], na.rm = TRUE)),
  ) %>%
  left_join(
    select(WS_demo, data_id, age),
    by = "data_id"
  ) %>%
  filter(
    !is.na(age)
  ) %>%
  ungroup()

saveRDS(WS_cats_test, "WS_cats_test.rds")

WS_test_total <- WS_cats_test %>%
  group_by(data_id, age) %>%
  summarize(
    total_WG = sum(cat_total_WG),
    total_WS = sum(cat_total_WS)
  ) %>%
  na.omit() %>%
  ungroup()

# Total score ====

# Fit WG ~ WS + WS^2 model
WS_training_WGWS <- WS_words2 %>%
  group_by(data_id) %>%
  summarize(
    ws = sum(ws_total),
    total_WG = sum(wg_total, na.rm = TRUE)
  ) %>%
  left_join(WS_demo, "data_id") %>%
  filter(
    !is.na(age)
  ) %>%
  mutate(
    age_c = age - 18
  )

range(WS_training_WGWS$ws, na.rm = TRUE)

# Force 0=0
lm_WSWG2 <- lm(ws ~ -1 + total_WG + I(total_WG^2), data = WS_training_WGWS)

lm_WSWG3 <- lm(ws ~ -1 + total_WG + I(total_WG^2) + I(total_WG^3),
               data = WS_training_WGWS)

lm_WSWG3age <- lm(ws ~ -1 + age_c*total_WG + I(age_c^2)*I(total_WG^2) +
                    I(age_c^3)*I(total_WG^3),
                  data = WS_training_WGWS)

lm_WSWG3age %>%
  coef() %>%
  as_tibble(rownames = "coef") %>%
  write_csv("WG2WS_coefs.csv")

round(sapply(list(lm_WSWG2, lm_WSWG3, lm_WSWG3age), AIC))

WS_test_total_lms <- WS_test_total %>%
  mutate(
    age_c = age - 18,
  ) %>%
  mutate(
    # Predict totals
    T1_hat = predict(lm_WSWG2, .) %>%
      unname(),
    T2_hat = predict(lm_WSWG3, .) %>%
      unname(),
    T3_hat = predict(lm_WSWG3age, .) %>%
      unname(),

    # Round totals and apply ceiling/floor
    across(ends_with("_hat"), ~round(.x) %>%
             replace(. < 0, 0) %>%
             replace(. > 680, 680)),

    across(ends_with("_hat"), ~(.x - total_WS), .names = "{.col}_errwords")

  )

WS_test_total_lms %>%
  pivot_longer(ends_with("errwords")) %>%
  group_by(name) %>%
  summarize(
    RMSEmax = sqrt(sum((value/680)**2)),
    RMSEw = sqrt(sum(value**2)),
  )

saveRDS(lm_WSWG3, "total_WG_to_WS_noage.rds")
saveRDS(lm_WSWG3age, "total_WG_to_WS.rds")

## Total error ====

### Error by age =====

WS_test_err_age <- WS_test_total_lms %>%
  pivot_longer(ends_with("errwords")) %>%
  group_by(age, name) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    upper = m + 2 * sd,
    lower = m - 2 * sd,
    name = str_remove(name, "_hat_errwords")
  )

WS_test_err_age_sex <- WS_test_total_lms %>%
  left_join(WS_demo) %>%
  pivot_longer(ends_with("errwords")) %>%
  group_by(age, sex, name) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    upper = m + 2 * sd,
    lower = m - 2 * sd,
    name = str_remove(name, "_hat_errwords")
  ) %>%
  na.omit()

plot_ageErrW <- ggplot(WS_test_err_age_sex, aes(x = age)) +
  geom_ribbon(aes(fill = sex, ymin = lower, ymax = upper),
              alpha = 1 / 3) +
  geom_line(aes(y = m, linetype = sex), linewidth = 0.5) +
  scale_x_continuous(breaks = seq(16, 30, 2)) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6))) +
  facet_wrap(vars(name)) +
  labs(x = "Age", y = "Error (words)") +
  theme_bw() +
  theme(legend.position = "none")

### Error by total =====

WS_test_err_total <- WS_test_total_lms %>%
  arrange(age, data_id) %>%
  mutate(
    group = cut(total_WG, breaks = 15)
  ) %>%
  pivot_longer(ends_with("errwords")) %>%
  group_by(group, name) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    label = as.character(group) %>%
      str_remove_all("(.*,|])") %>%
      as.numeric(),
    upper = m + 2 * sd,
    lower = m - 2 * sd,
    name = str_remove(name, "_hat_errwords")
  )

WS_test_err_total_sex <- WS_test_total_lms %>%
  left_join(WS_demo) %>%
  na.omit() %>%
  arrange(age, data_id) %>%
  mutate(
    group = cut(total_WG, breaks = 15)
  ) %>%
  pivot_longer(ends_with("errwords")) %>%
  group_by(group, name, sex) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    label = as.character(group) %>%
      str_remove_all("(.*,|])") %>%
      as.numeric(),
    upper = m + 2 * sd,
    lower = m - 2 * sd,
    name = str_remove(name, "_hat_errwords")
  )

WS_test_err <- list()
WS_test_err$age <- WS_test_err_age
WS_test_err$total <- WS_test_err_total

saveRDS(WS_test_err, "WS_total_err.rds")

plot_totalErrW <- ggplot(WS_test_err_total_sex, aes(x = label)) +
  geom_ribbon(aes(fill = sex, ymin = lower, ymax = upper),
              alpha = 1 / 3) +
  geom_line(aes(y = m, linetype = sex), linewidth = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6))) +
  facet_wrap(vars(name)) +
  labs(x = "Total WG inventory", y = "Error (words)") +
  theme_bw() +
  theme(legend.position = "bottom")

library(patchwork)

png("WG2WS_plots/total_model_error_sex.png", width = 6.5, height = 5, units = "in",
    res = 300)

plot_ageErrW / plot_totalErrW

dev.off()

# Check gender difference ====


WS_test_total_lms_sex <- left_join(WS_test_total_lms, WS_demo) %>%
  na.omit() %>%
  select(-matches("^T[12]"))

ggplot(WS_test_total_lms_sex, aes(x = age, y = T3_hat_errwords, color = sex)) +
  geom_point() +
  geom_smooth()

lm(T3_hat_errwords ~ age*sex + total_WG*sex, data = WS_test_total_lms_sex) %>%
  summary()
