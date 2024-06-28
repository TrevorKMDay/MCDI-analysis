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
source("wordbank-WG2WS-funcs.R")

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

legible_categories <- c("Sounds", "Animals", "Vehicles", "Toys", "Food/Drink",
                        "Clothing", "Body Parts", "Househ. Items",
                        "Furniture/Rooms", "Outside Things", "Places",
                        "People", "Games/Routines", "Act. Words",
                        "Desc. Words", "Time Words", "Pronouns", "Q. Words",
                        "Prep./Loc.", "Quant./Art.", "Help. Verbs", "C. Words")

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
  )

range(WS_training_WGWS$ws, na.rm = TRUE)

# Force 0=0
lm_WSWG2 <- lm(ws ~ -1 + total_WG + I(total_WG^2), data = WS_training_WGWS)

lm_WSWG3 <- lm(ws ~ -1 + total_WG + I(total_WG^2) + I(total_WG^3),
               data = WS_training_WGWS)

lm_WSWG3age <- lm(ws ~ -1 + age*total_WG + I(age^2)*I(total_WG^2) +
                    I(age^3)*I(total_WG^3),
                  data = WS_training_WGWS)

round(sapply(list(lm_WSWG2, lm_WSWG3, lm_WSWG3age), AIC))

WS_test_total_lms <- WS_test_total %>%
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
  pivot_longer(ends_with("errwords"), values_to = "error") %>%
  group_by(name) %>%
  summarize(
    mse = mean(error ** 2)
    # RMSEmax = sqrt(sum(error**2)),
    # RMSEw = sqrt(sum(error**2)),
  ) %>%
  mutate(
    rmse = sqrt(mse)
  )

saveRDS(lm_WSWG3age, "total_WG_to_WS.rds")

## Total error ====

## Error based on age ====

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


ggplot(WS_test_err_age, aes(x = age, fill = name)) +
  geom_line(aes(y = m, color = name), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(16, 30, 2)) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6))) +
  facet_wrap(vars(name)) +
  labs(x = "Age", y = "Error (words)") +
  theme_bw() +
  theme(legend.position = "none")

## Error based on total WG score ====

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

ggplot(WS_test_err_total, aes(x = label, fill = name, group = name)) +
  geom_line(aes(y = m, color = name), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6))) +
  facet_wrap(vars(name)) +
  labs(x = "Age", y = "Error (words)") +
  theme_bw() +
  theme(legend.position = "none")

## Error based on total score by gender ====

WS_test_err_total_gender <- WS_test_total_lms %>%
  left_join(WS_demo) %>%
  filter(
    !is.na(sex)
  ) %>%
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

ggplot(WS_test_err_total_gender, aes(x = label)) +
  geom_line(aes(y = m, linetype = sex)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = sex, fill = sex),
              alpha = 0.25) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6))) +
  facet_wrap(vars(name)) +
  labs(x = "Age", y = "Error (words)") +
  theme_bw()



ggplot(WS_test_err_total, aes(x = label)) +
  geom_ribbon(aes(fill = name, group = name, ymin = lower, ymax = upper),
              alpha = 0.5) +
  geom_line(data = WS_test_err_total_gender,
            aes(y = m, linetype = sex), linewidth = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 6, by = 2))) +
  scale_fill_discrete(guide = "none") +
  facet_wrap(vars(name)) +
  labs(x = "Age", y = "Error (words)", linetype = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom")

# Categories ====

png("WG2WS_plots/all_WSagainstWG.png", width = 7.5, height = 5, units = "in",
    res = 300)

ggplot(WS_words2, aes(x = wg_total, y = ws_total)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme_bw() +
  facet_wrap(vars(category), scales = "free") +
  labs(x = "WG", y = "WS")

dev.off()

models_by_category <- WS_words2 %>%
  filter(
    !(category %in% c("sounds", "connecting_words"))
  ) %>%
  group_by(category) %>%
  nest() %>%
  mutate(
    model1 = map(data, ~lm(ws_total ~ wg_total + I(wg_total^2), data = .x)),
    R2 = map_dbl(model1, ~summary(.x)$r.squared)
  )

paste0(models_by_category$category, ": ", round(models_by_category$R2, 3)) %>%
  paste(collapse = "; ")

models_by_category$R2 %>%
  sort()

# R^2 = 0.997

WS_cats <- WS %>%
  select(-understands, -language, -form, -english_gloss, -uni_lemma,
         -ends_with("_category")) %>%
  filter(
    item_kind == "word",
    data_id %in% WS_training,
    !(category %in% c("sounds", "connecting_words"))
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
    cat_total_WS = map_dbl(data, ~sum(.x$value)),
    cat_total_WG = map_dbl(data, ~sum(.x$value[.x$inWG])),
    diff = cat_total_WG - cat_total_WS
  ) %>%
  left_join(WS_demo, by = "data_id") %>%
  na.omit()

saveRDS(WS_cats, "WS_cats.rds")

round(c(cor(WS_cats$cat_total_WG, WS_cats$cat_total_WS, method = "p"),
        cor(WS_cats$cat_total_WG, WS_cats$cat_total_WS, method = "s")),
      3)

WS_cats_mean <- WS_cats %>%
  group_by(age, category) %>%
  nest() %>%
  mutate(

    mean_diff = map_dbl(data, ~mean(.x$diff)),
    t_test = map(data, ~t.test(.x$cat_total_WG, .x$cat_total_WS,
                               paired = TRUE)),
    p = map_dbl(t_test, ~.x$p.value),
    sig = p < .05 / 330,

    # Get rid of n.s. values and those that round to 0
    mean_diff2 = replace(mean_diff, (!sig | mean_diff > -0.5), NA),
    label = if_else(!is.na(mean_diff), as.character(round(mean_diff2)), "")

  )

png("WG2WS_plots/WGWS_diff.png", width = 5, height = 6, units = "in", res = 300)

cats_order <- rev(unique(s_dict$category))[2:21]

ggplot(WS_cats_mean, aes(x = age,  y = category, fill = mean_diff2)) +
  geom_tile() +
  geom_text(aes(label = label), size = 3) +
  scale_fill_viridis() +
  scale_x_continuous(limits = c(15, 31), breaks = 16:30) +
  scale_y_discrete(limits = cats_order) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Age (mo.)", y = "Category", fill = "Difference")

dev.off()

# lms ====

WS_cats_lms <- WS_cats %>%
  group_by(data_id) %>%
  mutate(
    WG_total_score = sum(cat_total_WG)
  ) %>%
  filter(
    !(category %in% c("sounds", "connecting_words"))
  ) %>%
  group_by(category) %>%
  mutate(
    age_c = age - 18
  ) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),

    lm1 = map(data, ~lm(cat_total_WS  ~ cat_total_WG + I(cat_total_WG^2),
                        data = .x)),

    lm2 = map(data, ~lm(cat_total_WS  ~ age_c*cat_total_WG +
                          I(age_c^2)*I(cat_total_WG^2),
                       data = .x)),

    lm3 = map(data, ~lm(cat_total_WS  ~ age_c*cat_total_WG +
                          I(age_c^2)*I(cat_total_WG^2) + WG_total_score +
                          I(WG_total_score^2) + I(WG_total_score^3),
                       data = .x)),
  ) %>%
  rowwise() %>%
  mutate(
    across(starts_with("lm"), MuMIn::AICc, .names = "{.col}_AICc")
  )

mean(sapply(WS_cats_lms$lm1, function(x) summary(x)$r.squared))
mean(sapply(WS_cats_lms$lm2, function(x) summary(x)$r.squared))
mean(sapply(WS_cats_lms$lm3, function(x) summary(x)$r.squared))

## Visualize ====

WS_cats_coefs <- WS_cats_lms %>%
  select(category, lm3) %>%
  ungroup() %>%
  mutate(
    coef = map(lm3, ~summary(.x)$coefficients %>%
                      as_tibble(rownames = "var"))
  ) %>%
  select(-lm3) %>%
  unnest(coef) %>%
  rename(
    p = `Pr(>|t|)`
  ) %>%
  mutate(
    sig = p < .005,
    est2 = if_else(sig, Estimate, NA_real_),
    log_est2 = log10(abs(est2)),
    pos_est2 = if_else(est2 > 0, "p", "n", "NA"),
    var2 = str_replace(var, "cat_total_WG", "C") %>%
      str_replace("age_c", "age") %>%
      str_replace("WG_total_score", "WG") %>%
      str_remove_all("(I[(]|[()^])")
  )

table(WS_cats_coefs$var2)

WS_cats_coefs %>%
  pivot_wider(id_cols = category, names_from = var2, values_from = log_est2)

# png("WS_cats_coefs_tileplot.png", width = 6.5, height = 4.5, units = "in",
#     res = 300)
#
# ggplot(WS_cats_coefs, aes(x = var2, y = category, fill = log_est2,
#                           pattern = pos_est2)) +
#   geom_tile_pattern(width = 0.9, height = 0.9,
#                     pattern_fill = "red", pattern_alpha = 0.5,
#                     pattern_frequency = 0.8) +
#   scale_fill_viridis(na.value = "transparent") +
#   scale_pattern_manual(values = c("stripe", "none", "none")) +
#   scale_y_discrete(limits = cats_order, labels = rev(legible_categories[2:21])) +
#   scale_x_discrete(limits = c("Intercept", "age", "C", "age:C", "age2", "C2",
#                               "age2:C2", "WG", "WG2", "WG3"),
#                    labels = c("Int.", "age", "C", "age:C", bquote(age^2),
#                               bquote(C^2), bquote(age^2:C^2), "WG",
#                               bquote(WG^2), bquote(WG^3))) +
#   labs(x = "Coefficient", y = "Category", fill = bquote(log[10]("|β|")),
#        color = "Syntax", pattern = "Direction") +
#   theme_minimal() +
#   theme(legend.position = "bottom", axis.text.x = element_text(vjust = 0))
#
# dev.off()

## Trim models ====

source("wordbank-WG2WS-trim.R")

WS_cats_lms$lm3_trim <- lapply(WS_cats_lms$lm3, mTrim)
WS_cats_lms$lm3_trim_AICc <- sapply(WS_cats_lms$lm3_trim, AICc)

WS_cats_lms_nodata <- select(WS_cats_lms, -data, -ends_with("AICc"))
saveRDS(WS_cats_lms_nodata, "WS_category_models.rds")

## AICc ====

AICc <- WS_cats_lms %>%
  select(category, ends_with("AICc")) %>%
  pivot_longer(ends_with("AICc"))

ggplot(AICc, aes(x = name, y = value)) +
  geom_line(aes(group = category)) +
  scale_x_discrete(labels = 1:4) +
  facet_wrap(vars(category), scales = "free") +
  theme_bw()

## Function declaration


# Test
# WG_to_WS("body_parts", wg = 28, wg_total = 1, age = 30)



hat_file <- "predicted_WS_categoryscores_in_test.rds"

if (!file.exists(hat_file)) {

  # Separate actual model estimation so we don't have to rerun it when code
  # changes
  WS_cats_test_models <- WS_cats_test %>%
    group_by(data_id) %>%
    mutate(
      WG_total_score = sum(cat_total_WG)
    ) %>%
    rowwise() %>%
    filter(
      !(category %in% c("sounds", "connecting_words"))
    ) %>%
    mutate(
      # Category total only model
      WS_hat1 = WG_to_WS(category = category, wg = cat_total_WG, age = NA,
                         wg_total = NA),

      # Category and age model
      WS_hat2 = WG_to_WS(category = category, wg = cat_total_WG, age = age,
                          wg_total = NA),

      # Category, age, and total score
      WS_hat3 = WG_to_WS(category = category, wg = cat_total_WG, age = age,
                         wg_total = WG_total_score, trimmed = FALSE),

      WS_hat3t = WG_to_WS(category = category, wg = cat_total_WG, age = age,
                          wg_total = WG_total_score, trimmed = TRUE)
    )

  saveRDS(WS_cats_test_models, "predicted_WS_categoryscores_in_test.rds")

} else {
  WS_cats_test_models <- readRDS(hat_file)
}

ggplot(WS_cats_test_models, aes(x = cat_total_WS, y = WS_hat3)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(vars(category), scales = "free")

# WS_cats_test_models %>%
#   ungroup() %>%
#   select(starts_with("WS_hat")) %>%
#   apply(2, min)

WS_cats_test_models2 <- WS_cats_test_models %>%
  mutate(
    err1 = WS_hat1 - cat_total_WS,
    err2 = WS_hat2 - cat_total_WS,
    err3 = WS_hat3 - cat_total_WS,
    err3t = WS_hat3t - cat_total_WS
  ) %>%
  select(-data)

WS_cats_test_models2 %>%
  ungroup() %>%
  select(starts_with("err")) %>%
  apply(2, function(x) sum(x**2))

# Identify problems
WS_cats_test_models %>%
  filter(
    is.na(WS_hat1) | is.na(WS_hat2) | is.na(WS_hat3) | is.na(WS_hat3t)
  )

## Differences by category ====

WS_cats_test_summary <- WS_cats_test_models2 %>%
  pivot_longer(starts_with("err")) %>%
  group_by(category, name) %>%
  summarize(
    mean_err = mean(value),
    mean_abs_err = mean(abs(value)),
    sd_err = sd(value),
  ) %>%
  mutate(
    lower = mean_err - sd_err,
    upper = mean_err + sd_err,
    range = upper - lower
  )

ggplot(WS_cats_test_summary, aes(x = name, y  = range * 2, color = category)) +
  geom_point() +
  geom_line(aes(group = category)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## Total score error ====

WS_test_total <- WS_cats_test_models2  %>%
  group_by(data_id, age) %>%
  summarize(
    total_WG = sum(cat_total_WG, na.rm = TRUE),
    total_true = sum(cat_total_WS, na.rm = TRUE),
    across(starts_with("WS_hat"), sum, .names = "{.col}_total"),
    across(starts_with("err"), sum, .names = "{.col}_errtotal"),
    across(ends_with("errtotal"), ~ .x / total_true * 100,
           .names = "{.col}_pct")
  )

WS_test_total_long <- WS_test_total %>%
  pivot_longer(starts_with("total"), names_to = "sum",
               values_to = "instrument_total") %>%
  pivot_longer(starts_with("err")) %>%
  separate(name, into = c("model", "metric"), extra = "merge") %>%
  pivot_wider(names_from = metric, values_from = value)


range(WS_test_total_long$errtotal_pct, na.rm = TRUE)

WS_test_err3 <- WS_test_total_long %>%
  filter(
    model == "err3t"
  )

errSD <- sd(WS_test_err3$errtotal)
errpctSD <- sd(WS_test_err3$errtotal_pct[WS_test_err3$instrument_total >= 50])

png("WG2WS_plots/error_in_words.png", width = 4.75, height = 3, units = "in",
    res = 300)

ggplot(WS_test_err3, aes(x = instrument_total, y = errtotal)) +
  geom_point(aes(color = age), alpha = 0.2) +
  geom_smooth(group = 1) +
  geom_hline(yintercept = errSD * c(-1, 1), linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = errSD * c(-2, 2), linetype = "solid", linewidth = 1) +
  scale_color_viridis(option = "plasma") +
  facet_wrap(vars(sum), scales = "free") +
  theme_bw() +
  labs(x = "Instrument Total", y = "Error (in words)", color = "Age") +
  theme(legend.position = "bottom")

dev.off()

png("WG2WS_plots/error_in_pct.png", width = 4.75, height = 3, units = "in",
    res = 300)

ggplot(WS_test_err3, aes(x = instrument_total, y = abs(errtotal_pct + 0.1))) +
  geom_point(aes(color = age), alpha = 0.2) +
  geom_smooth() +
  geom_vline(xintercept = 50, color = "red", linewidth = 1) +
  geom_hline(yintercept = errpctSD * 1, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = errpctSD * 2, linetype = "solid", linewidth = 1) +
  scale_color_viridis(option = "plasma") +
  scale_y_log10() +
  facet_wrap(vars(sum), scales = "free") +
  theme_bw() +
  labs(x = "Instrument total", y = "% error", color = "Age") +
  theme(legend.position = "bottom")

dev.off()

png("WG2WS_plots/error_in_pct_by_age.png", width = 4.75, height = 3, units = "in",
    res = 300)

WS_test_err3_2 <- WS_test_err3 %>%
  filter(
    instrument_total >= 50
  )

ggplot(WS_test_err3_2, aes(x = jitter(age), y = abs(errtotal_pct + 0.1))) +
  geom_point(aes(color = instrument_total), alpha = 0.2) +
  geom_smooth() +
  geom_vline(xintercept = 17.5, color = "blue", linewidth = 1) +
  geom_hline(yintercept = errpctSD * 1, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = errpctSD * 2, linetype = "solid", linewidth = 1) +
  scale_color_viridis(option = "viridis", na.value = "red") +
  # scale_y_log10() +
  facet_grid(cols = vars(sum), scales = "free") +
  theme_bw() +
  labs(x = "Age (months)", y = "% error", color = "Instrument\ntotal") +
  theme(legend.position = "bottom")

dev.off()

# Percentiles ====

WS_test_err3_3 <- WS_test_err3 %>%
  filter(
    instrument_total >= 100
  )

WS_training_df <- WS %>%
  left_join(WS_demo, by = "data_id") %>%
  group_by(data_id, age) %>%
  filter(
    !(data_id %in% WS_test),
    item_kind == "word",
    !(category %in% c("sounds", "connecting_words"))
  ) %>%
  summarize(
    produces = sum(produces, na.rm = TRUE)
  )

ggplot(WS_training_df, aes(x = as.factor(age),  y = produces)) +
  geom_boxplot()

WS_training_df %>%
  filter(
    !is.na(age)
  ) %>%
  group_by(age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    ecdf = map(data, ~ecdf(.x$produces)),
    pctile50 = map_dbl(ecdf, ~.x(50)),
    pctile100 = map_dbl(ecdf, ~.x(100))
  )

words_wo_sounds_cwords <- s_dict %>%
  filter(
    !(category %in% c("sounds", "connecting_words"))
  ) %>%
  nrow()

c(50, 100) * (680 / words_wo_sounds_cwords)



# Total score models ====

max <- cat_totals %>%
  filter(
    !(category %in% c("sounds", "connecting_words"))
  ) %>%
  pull(n_WS) %>%
  sum()


pcterr <- WS_test_total %>%
  ungroup() %>%
  select(-ends_with("_pct")) %>%
  mutate(
    # Simple quadratic model
    WS_hat0 = round(unname(predict(lm_WSWG2, newdata = .))),
    WS_hat03 = round(unname(predict(lm_WSWG3, newdata = .))),
    err0 = WS_hat0 - total_true,
    err03 = WS_hat03 - total_true
  ) %>%
  select_all(~str_remove(., "_(errtotal|total)")) %>%
  select(data_id, age, total_true, total_WG, starts_with("WS_hat"),
         starts_with("err")) %>%
  pivot_longer(starts_with("WS_hat"), names_to = "model.x",
               values_to = "y_hat") %>%
  pivot_longer(starts_with("err"), names_to = "model.z",
               values_to = "errwords") %>%
  mutate(
    across(starts_with("model."), ~str_remove_all(.x, "(WS_hat|err|_max)")),
    errmax = errwords / max
  ) %>%
  filter(
    model.x == model.z
  ) %>%
  select(-model.z)

pcterr_rmse <- pcterr %>%
  group_by(model.x) %>%
  summarize(
    RMSE_max = sqrt(sum(errmax^2)),
    RMSE_Words = sqrt(sum(errwords^2))
  )

ggplot(pcterr, aes(x = total_true, y = errwords, color = age)) +
  geom_point(alpha = 0.05) +
  geom_smooth(color = "black") +
  scale_color_viridis() +
  facet_wrap(vars(model.x)) +
  theme_bw()

