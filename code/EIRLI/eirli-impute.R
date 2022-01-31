setwd("~/Desktop/UMNDrive/Research/MCDI/MCDI-analysis/code/EIRLI")

library(tidyverse)

source("../mcdi-setup.R")

##############################################################################

WS_scored <- read_data("Wordbank/WS-scored.rds")$n

size <- WS_scored %>%
  select(-data_id, -age, -LEXICAL, -SYNTAX) %>%
  apply(2, max) %>%
  as.data.frame() %>%
  rownames_to_column("name") %>%
  rename(n = '.')

# Read in "training data"
WS <- left_join(read_data("Wordbank/WS-demographics.rds"), WS_scored) %>%
  mutate(
    male = if_else(sex == "Male", 1, -1),
  ) %>%
  select(-birth_order, -ethnicity, -instrument, -LEXICAL, -SYNTAX, -sex,
         -mom_ed) %>%
  na.omit()

# Vars missing from EIRLI to impute
vars2impute <- c("outside", "places", "time_words",
                 "WORD_FORMS_NOUNS", "WORD_FORMS_VERBS",
                 "WORD_ENDINGS_NOUNS", "WORD_ENDINGS_VERBS")

# WS w/o data needing imputation for prediction
WS_source <- WS %>%
  select(-any_of(vars2impute))

# For each model, fit vars in desc. order of correlation w/ Y
cors <- WS %>%
  select(-data_id, -age) %>%
  cor() %>%
  as_tibble() %>%
  mutate(
    cross_var = colnames(.)
  )

imputation <- tibble(var = vars2impute) %>%
  mutate(
    # Get correlation of var to all other vars
    r = map(var, ~cors[c("cross_var", .x)] %>%
                    rename(r = .x))
  ) %>%
  unnest(r) %>%
  filter(
    # Remove self-to-self var
    var != cross_var,
    # Remove imputing vars as predictors (need to leave them in to get the r
    #   vals)
    !(cross_var %in% vars2impute)
  ) %>%
  group_by(var) %>%
  arrange(var, desc(r)) %>%
  nest() %>%
  mutate(
    # Construct prediction model based on order by desc r
    model_str = map2(var, data, ~paste(.x, "~ age + male + ",
                                       paste(.y$cross_var, collapse = " + "))),
    # Actually run the lm
    model     = map(model_str, ~lm(formula = .x, data = WS)),
    # Create predicted
    pred      = map(model, ~predict(.x, WS))
  )

# Add predicted columns to new DF
WS_pred <- WS
for (i in 1:nrow(imputation))
  WS_pred[paste0(imputation$var[i], "_pred")] <- imputation$pred[i]

# Pivot wider
WS_pred2 <- WS_pred %>%
  select(data_id, age, all_of(vars2impute), ends_with("pred")) %>%
  pivot_longer(-c(data_id, age)) %>%
  mutate(
    pred = if_else(str_detect(name, "_pred$"), "predicted", "original"),
    name = str_remove(name, "_pred$")
  ) %>%
  pivot_wider(names_from = pred, values_from = value) %>%
  left_join(size) %>%
  mutate(
    # Round, then threshold at 0 and category max
    predicted = round(predicted) %>%
                  if_else(. < 0, 0, .) %>%
                  if_else(. > n, n, .)
  )

# Plot predicted vs. original
ggplot(WS_pred2, aes(x = original, y = predicted)) +
  geom_jitter(alpha = 0.1, width = 0.25) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(name), scales = "free") +
  theme_bw()

# Sum of square errors
WS_pred2_sqerr <- WS_pred2 %>%
  mutate(
    sq_err_p  = (predicted - original) ^ 2,
  ) %>%
  group_by(name) %>%
  summarize(
    n = n(),
    sse_p  = sum(sq_err_p)
  )

imputation_save <- imputation %>%
  select(var, model)

save_data(imputation_save, "EIRLI/WS_imputation_models.rds")

##############################################################################

# Read data
E1 <- read_data("EIRLI/eirli_clean.rds")

# Remove everythign except age/male and predictors
E2 <- E1 %>%
  mutate(
    male = if_else(gender == "Male", 1, -1),
  ) %>%
  select(data_id, age, male, any_of(colnames(WS)))

# Predict
for (v in vars2impute)
  E2[v] <- predict(imputation$model[imputation$var == v][[1]], E2)

# Truncate vals at 0/max
E3 <- E2 %>%
  pivot_longer(-c(data_id, age, male)) %>%
  left_join(size) %>%
  mutate(
    new_value = if_else(value < 0, 0, value) %>%
                  if_else(. > n, n, .) %>%
                  as.integer()
  )

E4 <- E3 %>%
  mutate(
    p = new_value / n,
  )

# Pivot back into format
E4_n <- E4 %>%
  pivot_wider(id_cols = c(data_id, age, male), names_from = name,
              values_from = new_value) %>%
  mutate(
    # remove annoying attr
    across(everything(), unname)
  ) %>%
  select(
    # Order based on WS order
    all_of(colnames(WS))
  )

E4_p <- E4 %>%
  pivot_wider(id_cols = c(data_id, age, male), names_from = name,
              values_from = p) %>%
  mutate(
    # remove annoying attr
    across(everything(), unname)
  ) %>%
  select(
    # Order based on WS order
    all_of(colnames(WS))
  )

# Reset back into orginal format
E5_n <- E1 %>%
  select(data_id, age, exact_age, gender, follow_up, hx_dx, dx, dx_count,
         contains("ed")) %>%
  full_join(select(E4_n, -male))

E5_p <- E1 %>%
  select(data_id, age, exact_age, gender, follow_up, hx_dx, dx, dx_count,
         contains("ed")) %>%
  full_join(select(E4_p, -male))

save_data(list(n = E5_n, p = E5_p), "EIRLI/eirli_clean_impute.rds")
