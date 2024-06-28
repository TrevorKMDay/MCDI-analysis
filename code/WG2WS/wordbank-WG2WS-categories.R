path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
source("../mcdi-setup.R")
source("wordbank-WG2WS-funcs.R")

# Load data ====

WS_words2 <- readRDS("WS_words2.rds")
WS_training <- readRDS("WS_training_subs.rds")
s_dict <- read_data("other/s_dict.csv")
g_dict <- read_data("other/g_dict.csv")

lex_cats <- unique(s_dict$category)[1:15]

s_dict %>%
  mutate(
    lex = category %in% lex_cats
  ) %>%
  group_by(lex) %>%
  summarize(
    n = n()
  )

cwords <- readRDS("WG2WS_test_cwords.rds")

legible_categories <- c("Sounds", "Animals", "Vehicles", "Toys", "Food/Drink",
                        "Clothing", "Body Parts", "Househ. Items",
                        "Furniture/Rooms", "Outside Things", "Places",
                        "People", "Games/Routines", "Act. Words",
                        "Desc. Words", "Time Words", "Pronouns", "Q. Words",
                        "Prep./Loc.", "Quant./Art.", "Help. Verbs", "C. Words")

leg_cats_tbl <- tibble(legible_cat = legible_categories,
                       category = unique(s_dict$category))

# We only need ages for the WS so no need to load WG demographics
WS_demo <- read_data("Wordbank/WS-demographics-230215.rds") %>%
  select(data_id, age, sex)

# Categories ====

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

range(models_by_category$R2)

#           load this guy
WS_cats <- read_data("Wordbank/WS-230215.rds") %>%
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

    lm4 = map(data, ~lm(cat_total_WS  ~ age_c*cat_total_WG +
                          I(age_c^2)*I(cat_total_WG^2) +
                          age_c*WG_total_score +
                          I(age_c^2)*I(WG_total_score^2) +
                          I(age_c^3)*I(WG_total_score^3),
                        data = .x)),

  )

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

# library(ggpattern)
# library(viridis)
#
# png("WG2WS_plots/WS_cats_coefs_tileplot.png", width = 6.5, height = 4.5,
#     units = "in", res = 300)
#
# cats_order <- unique(s_dict$category)[2:21]
#
# ggplot(WS_cats_coefs, aes(x = var2, y = category, fill = log_est2,
#                           pattern = pos_est2)) +
#   geom_tile_pattern(width = 0.9, height = 0.9,
#                     pattern_fill = "red", pattern_alpha = 0.5,
#                     pattern_frequency = 0.8) +
#   scale_fill_viridis(na.value = "transparent") +
#   scale_pattern_manual(values = c("stripe", "none", "none")) +
#   scale_x_discrete(limits = c("Intercept", "age", "C", "age:C", "age2", "C2",
#                               "age2:C2", "WG", "WG2", "WG3", "age:WG",
#                               "age2:WG2", "age3:WG3"),
#                    labels = c("Int.", "age", "C", "age:C", bquote(age^2),
#                               bquote(C^2), bquote(age^2:C^2), "WG",
#                               bquote(WG^2), bquote(WG^3), "age:WG",
#                               bquote(age^2:WG^2), bquote(age^3:WG^3))) +
#   scale_y_discrete(limits = rev(cats_order),
#                    labels = rev(legible_categories[2:21])) +
#   labs(x = "Coefficient", y = "Category", fill = bquote(log[10]("|β|")),
#        color = "Syntax", pattern = "Direction") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0))
#
# dev.off()

## Calculate R2 ====

apply_model <- function(cat_total_WG, age_c, WG_total_score, category, model) {

  model_name <- paste0("lm", model)
  models <- ungroup(WS_cats_lms)[, model_name]
  model <- models[which(WS_cats_lms$category == category), ][[1]][[1]]

  new_data <- tibble(age_c = age_c, cat_total_WG = cat_total_WG,
                      WG_total_score = WG_total_score)

  y_hat <- predict(model, newdata = new_data) %>%
    unname() %>%
    round()

  y_hat <- if_else(y_hat < 0, 0, y_hat)

  return(y_hat)

}

apply_model(10, 5:10, 100, "animals", 1)

cwords <- select(cwords, data_id, sounds, cwords_hat)

WS_training_R2 <- WS_cats %>%
  select(-data, -diff, -sex) %>%
  arrange(data_id) %>%
  group_by(data_id) %>%
  mutate(
    age_c = as.integer(age - 18),
    WG_total_score = sum(cat_total_WG)
  )

for (i in 1:4) {

  new_col <- paste0("WS_hat", i)

  WS_training_R2[, new_col] <- apply_model(WS_training_R2$cat_total_WG,
                                            WS_training_R2$age_c,
                                            WS_training_R2$WG_total_score,
                                            WS_training_R2$category,
                                            i)
}

WS_training_R2_sums <- WS_training_R2 %>%
  group_by(data_id, age) %>%
  summarize(
    WS = sum(cat_total_WS),
    WS_hat1_sum = sum(WS_hat1),
    WS_hat2_sum = sum(WS_hat2),
    WS_hat3_sum = sum(WS_hat3),
    WS_hat4_sum = sum(WS_hat4),
  )

round((WS_training_R2_sums %>%
        ungroup() %>%
        select(starts_with("WS")) %>%
        cor()) ** 2,
      3)



## Trim models ====

source("wordbank-WG2WS-trim.R")

for (i in 1:length(WS_cats_lms$lm4)) {
  message(i)
  x <- mTrim(WS_cats_lms$lm4[[i]])
}

WS_cats_lms$lm3_trim <- lapply(WS_cats_lms$lm3, mTrim)
WS_cats_lms$lm3_trim_AIC <- sapply(WS_cats_lms$lm3_trim, AIC)

WS_cats_lms %>%
  select(-data, -ends_with("AICc")) %>%
  saveRDS("WS_category_models.rds")

## AIC ====

AIC <- WS_cats_lms %>%
  rowwise() %>%
  mutate(
    lm1_AIC = AIC(lm1),
    lm2_AIC = AIC(lm2),
    lm3_AIC = AIC(lm3),
    lm4_AIC = AIC(lm4),
  ) %>%
  select(category, ends_with("AIC")) %>%
  pivot_longer(ends_with("AIC"))

ggplot(AIC, aes(x = name, y = value)) +
  geom_line(aes(group = category)) +
  facet_wrap(vars(category), scales = "free") +
  theme_bw()

# Test
# WG_to_WS("body_parts", wg = 28, wg_total = 1, age = 30)

hat_file <- "predicted_WS_categoryscores_in_test.rds"

if (!file.exists(hat_file)) {

  # Separate actual model estimation so we don't have to rerun it when code
  # changes
  WS_cats_test_models <- readRDS("WS_cats_test.rds") %>%
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

      # Cubic interaction model
      WS_hat4 = WG_to_WS(category = category, wg = cat_total_WG, age = age,
                          wg_total = WG_total_score, cubic = TRUE,
                          trimmed = FALSE),

      WS_hat3t = WG_to_WS(category = category, wg = cat_total_WG, age = age,
                         wg_total = WG_total_score, trimmed = TRUE)
    )

  saveRDS(WS_cats_test_models, "predicted_WS_categoryscores_in_test.rds")

} else {
  WS_cats_test_models <- readRDS(hat_file)
}

WS_cats_test_models <- left_join(WS_cats_test_models, leg_cats_tbl)

category_plots <- ggplot(WS_cats_test_models, aes(x = cat_total_WS, y = WS_hat1)) +
  geom_point(alpha = 0.1, shape = 20) +
  geom_smooth() +
  scale_y_continuous(n.breaks = 3) +
  facet_wrap(vars(legible_cat), scales = "free") +
  theme_bw() +
  labs(x = "Ground truth WS score", y = "Predicted WS score, (C3t)",
       title = "(A) Category predictions") +
  theme(strip.text.x = element_text(size = 8))

# WS_cats_test_models %>%
#   ungroup() %>%
#   select(starts_with("WS_hat")) %>%
#   apply(2, min)

## RMSE ====

### Total ====

WS_cats_test_models2 <- WS_cats_test_models %>%
  group_by(data_id, age) %>%
  summarize(
    WG_total = sum(cat_total_WG),
    WS_total = sum(cat_total_WS),
    WS_hat1 = sum(WS_hat1),
    WS_hat2 = sum(WS_hat2),
    WS_hat3 = sum(WS_hat3),
    WS_hat4 = sum(WS_hat4),
    WS_hat3t = sum(WS_hat3t)
  ) %>%
  left_join(cwords) %>%
  ungroup() %>%
  mutate(
    total_group = cut(WG_total, breaks = 15),
    across(starts_with("WS_hat"), ~(.x + sounds + cwords_hat),
           .names = "{.col}_2"),
    across(ends_with("_2"), ~(.x - WS_total), .names = "{.col}_err")
  )

WS_cats_test_models2 %>%
  pivot_longer(ends_with("err"), values_to = "error") %>%
  group_by(name) %>%
  summarize(
    mse = mean(error ** 2)
  ) %>%
  mutate(
    rmse = sqrt(mfse)
  )

WS_cats_err_age <- WS_cats_test_models2 %>%
  select(data_id, age, ends_with("_err")) %>%
  pivot_longer(cols = ends_with("_err")) %>%
  group_by(name, age) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    name = str_remove_all(name, "(WS_hat|_2_err)"),
    upper = m + 2 * sd,
    lower = m - 2 * sd,
  )

WS_cats_err_total <- WS_cats_test_models2 %>%
  select(data_id, total_group, ends_with("_err")) %>%
  pivot_longer(cols = ends_with("_err")) %>%
  group_by(name, total_group) %>%
  summarize(
    m = mean(value),
    sd = sd(value)
  ) %>%
  ungroup() %>%
  mutate(
    group_upper = as.numeric(str_remove_all(total_group, "(.*,|])")),
    name = str_remove_all(name, "(WS_hat|_2_err)"),
    upper = m + 2 * sd,
    lower = m - 2 * sd,
  )

total_err <- readRDS("WS_total_err.rds")

total_err_T3_age <- total_err$age %>%
  filter(
    name == "T3"
  )

total_err_T3_wg <- total_err$total %>%
  filter(
    name == "T3"
  ) %>%
  mutate(
    group_upper = as.numeric(str_remove_all(group, "(.*,|])"))
  )

err_age <- ggplot(filter(WS_cats_err_age, name == "3t"),
       aes(x = age, fill = name, group = name)) +
  geom_line(aes(y = m, color = name), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  scale_x_continuous(breaks = seq(16, 30, 2)) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = NULL,
                                         breaks = seq(-6, 10))) +
  geom_line(data = total_err_T3_age, aes(y = upper), linewidth = 1,
            linetype = "dashed") +
  geom_line(data = total_err_T3_age, aes(y = lower), linewidth = 1,
            linetype = "dashed") +
  labs(x = "Age (mo.)", y = "Error (words)", title = "(B) Error by age") +
  theme_bw() +
  theme(legend.position = "none")

err_wg <- ggplot(filter(WS_cats_err_total, name == "3t"),
       aes(x = group_upper, fill = name, group = name)) +
  geom_line(aes(y = m, color = name), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(sec.axis = sec_axis(~ .x / 680 * 100,
                                         name = "Error (% total)",
                                         breaks = seq(-6, 10))) +
  geom_line(data = total_err_T3_wg, aes(y = upper), linewidth = 1,
            linetype = "dashed") +
  geom_line(data = total_err_T3_wg, aes(y = lower), linewidth = 1,
            linetype = "dashed") +
  labs(x = "WG total score", y = NULL,
       title = "(C) Error by WG score") +
  theme_bw() +
  theme(legend.position = "none")

png("WG2WS_plots/category_plot.png", width = 6.5, height = 9, units = "in",
    res = 300)

category_plots / (err_age + err_wg)

dev.off()

### Lex/syn ====

WS_cats_test_models2 <- WS_cats_test_models %>%
  mutate(
    lex = if_else(category %in% lex_cats, "lex", "syn")
  ) %>%
  group_by(data_id, age, lex) %>%
  summarize(
    WS_total = sum(cat_total_WS),
    WS_hat1 = sum(WS_hat1),
    WS_hat2 = sum(WS_hat2),
    WS_hat3 = sum(WS_hat3),
    WS_hat4 = sum(WS_hat4),
    WS_hat3t = sum(WS_hat3t),
  ) %>%
  left_join(cwords) %>%
  mutate(
    across(starts_with("WS_hat"), ~if_else(lex == "lex", .x + sounds,
                                           .x + cwords_hat),
           .names = "{.col}_2"),
    across(ends_with("_2"), ~(.x - WS_total), .names = "{.col}_err")
  )

WS_cats_test_models2 %>%
  pivot_longer(ends_with("err"), values_to = "error") %>%
  group_by(name, lex) %>%
  summarize(
    mse = mean(error ** 2)
  ) %>%
  mutate(
    rmse = sqrt(mse)
  ) %>%
  pivot_wider(id_cols = name, names_from = lex, values_from = rmse)

