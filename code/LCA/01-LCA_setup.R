path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(pracma)

source("../mcdi-setup.R")

dir.create("plots/setup", showWarnings = FALSE, recursive = TRUE)

# Category labels ####

# EIRLI is missing some columns, here's the lex cols that DO exist
eirli_cols <- c("action_words", "animals", "body_parts", # "COMPLEXITY",
                "clothing", "connecting_words", "descriptive_words",
                "food_drink", "furniture_rooms", "games_routines",
                "helping_verbs", "people", "locations", "pronouns",
                "quantifiers", "question_words", "household", "sounds",
                "toys", "vehicles")

# WS 1A inventory category labels
WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

# WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
#            "WORD_ENDINGS_VERBS", "COMPLEXITY")

lex_cols <- WS_1A[1:15]
syn_cols <- WS_1A[16:22]

# Read in data ####

# In the following steps, use the vectors defined above to create accurate
# counts based on missing data (any_of). In this case, the syntax total is only
# COMPLEXITY due to missing data

## EIRLI ####

# Read in cleaned EIRLI data with imputations for missing categories
eirli <- read_data("EIRLI/EIRLI_clean.csv") %>%
  mutate(

    inventory_total = select(., any_of(WS_1A)) %>%
      rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
      rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
      rowSums(),

    # EIRLI inventory columns
    eirli_total = rowSums(select(., all_of(eirli_cols))),

    status = case_when(
      !follow_up      ~ "no_follow_up",
      follow_up &  dx ~ "lg_dx",
      follow_up & !dx ~ "no_lg_dx"
    ),

    proj = "EIRLI"

  ) %>%
  rename(
    sex = gender
  ) %>%
  select(proj, status, data_id, age, exact_age, status,
         any_of(WS_1A), COMPLEXITY, ends_with("_total"))

# EIRLI demographics
eirli_d <- read_data("EIRLI/eirli_clean_impute.rds")$n %>%
  select(data_id, gender, father_ed, mother_ed) %>%
  rename(
    sex = gender
  ) %>%
  distinct() %>%
  mutate(
    proj = "EIRLI"
  )

## BCP ####

bcp_all <- read_data("BCP/BCP_all_data_rescored.rds")

# ALL BCP data
bcp <- bcp_all %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  select(data_id, age, all_of(WS_1A)) %>%
  mutate(
    exact_age       = age,
    age             = round(age),
    inventory_total = rowSums(select(., all_of(WS_1A))),
    eirli_total     = rowSums(select(., all_of(eirli_cols))),
    # noS_total       = rowSums(select(., all_of(eirli_cols), -sounds)),
    lex_total       = rowSums(select(., all_of(lex_cols))),
    syn_total       = rowSums(select(., all_of(syn_cols))),
    proj            = "BCP",
    status          = "BCP"
  )

## Identify BCP participants with drops ####

# i.e. particpants who have a timepoint that has fewer words than a previous
#   one, probably a date entry error
#   TO DO: follow up

bcp_bad <- bcp %>%
  select(data_id, age, inventory_total) %>%
  group_by(data_id) %>%
  arrange(age) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    max = map_dbl(data, ~max(.x$inventory_total)),
    last = map_dbl(data, ~tail(.x$inventory_total, 1))
  ) %>%
  filter(
    n >= 2,
    last < max
  )

ggplot(filter(bcp, data_id %in% bcp_bad$data_id),
       aes(x = age, y = inventory_total, color = data_id)) +
  geom_point() +
  geom_line(aes(group = data_id)) +
  scale_x_continuous(breaks = seq(10, 40, 2)) +
  theme_bw()

# And remove

bcp <- bcp %>%
  filter(
    !(data_id %in% bcp_bad$data_id)
  )

# BCP and EIRLI demographics
BplusE_d <- bind_rows(bcp_d, eirli_d) %>%
  mutate(
    across(ends_with("ed"), ~replace(.x, .x == "graduate", "grad") %>%
             replace(. == "high", "secondary") %>%
             replace(. == "not_answered", NA)),
    mother_college = mother_ed %in% c("college", "grad", "some_grad"),
    father_college = if_else(!is.na(father_ed),
                             father_ed %in% c("college", "grad", "some_grad"),
                             NA)
  )

# Finalize B+E dataset
BplusE <- bind_rows(bcp, eirli) %>%
  select(proj, status, data_id, age, exact_age,
         all_of(eirli_cols), COMPLEXITY, ends_with("total")) %>%
  arrange(proj, status, data_id, exact_age) %>%
  group_by(data_id) %>%
  mutate(

    # Numeric identifier for data_id, leave original as char
    data_id_num = as.numeric(cur_group_id()),

    # If exact age is missing (BCP), replace it with the age column (BCP has
    # exact age, EIRLI has target age as "age" and exact age. No reason not
    # to use exact_age going forward)
    exact_age = if_else(is.na(exact_age), age, exact_age),

    # Harmonize education across projects, and order by amount of education so
    # it plots right
    across(ends_with("_ed"),
           ~case_when(
             .x %in% c("grad", "graduate")  ~ "graduate",
             .x %in% c("high", "secondary") ~ "secondary",
             .x == "not_answered" ~ NA_character_,
             # Don't change any of the others
             TRUE ~ .x
           ) %>%
             ordered(levels = c("some_secondary", "secondary", "some_college",
                                "college", "some_grad", "graduate", NA))
    ),


    dx_prior = case_when(
      status == "no_lg_dx" ~ 1,
      status == "lg_dx"    ~ 2,
      # All others
      TRUE ~ 0
    )
  ) %>%
  select(proj, status, data_id, data_id_num, age, exact_age,
         ends_with("_ed"), ends_with("_total"), everything())

## Calculate delay status ####

Badj <- read_data("LCA/BCP_to_EIRLI.rds")

# Determine value at 24 months
BplusE_24mo_1 <- BplusE %>%
  select(proj, status, data_id, age, exact_age, inventory_total) %>%
  group_by(proj, status, data_id) %>%
  arrange(age) %>%
  nest() %>%
  arrange(data_id) %>%
  mutate(

    # Separate data into dfs with data from before 24mo and 24mo and after
    lt24mo = map(data, ~filter(.x, exact_age < 24)),
    lt24mo_n = map_int(lt24mo, nrow),

    ge24mo = map(data, ~filter(.x, exact_age >= 24)),
    ge24mo_n = map_int(ge24mo, nrow),

    # Get the last inventory reached before age 24mo
    #   - If this is >50w, confirmed no delay; but if <50w, not confirmed
    lt24mo_last = map_if(lt24mo, .p = lt24mo_n > 0,
                         .f = ~.x$inventory_total[length(.x$inventory_total)],
                         .else = ~NA) %>%
      unlist(),

    # Get the first value reached after age 24mo.
    #   If this is <50w, confirmed delay, but if >50w, not confirmed
    ge24mo_first = map_if(ge24mo, .p = ge24mo_n > 0,
                          .f = ~.x$inventory_total[1],
                          .else = ~NA) %>%
      unlist(),

    delay = case_when(
      lt24mo_last > 50 ~ "no_delay",
      ge24mo_first < 50 ~ "delay",
      TRUE ~ "delay_unk"
    )

  ) %>%
  select(-starts_with("lt24mo"), -starts_with("ge24mo"))

BplusE_24mo_unk <- BplusE_24mo_1 %>%
  filter(
    delay == "delay_unk"
  ) %>%
  mutate(
    min = map_dbl(data, ~min(.x$exact_age)),
    max = map_dbl(data, ~max(.x$exact_age)),
  ) %>%
  filter(
    min < 24,
    max > 24
  ) %>%
  mutate(

    # If they haven't, interpolate and round the resulting value at 24 months
    newdata = map(data,
                  ~tibble(age = min(.x$age):max(.x$age),
                          yi  = interp1(x = .x$age, y = .x$inventory_total,
                                        xi = age))),

    # If the interpolated values don't include 24, then it returns numeric(0),
    #   which to get rid of, we find the length (bad is 0), then reformat
    #   the column to be a proper double vector
    est_at_24_mo = map(newdata, ~round(.x$yi[.x$age == 24])),
    est_at_24_mo_len = map_dbl(est_at_24_mo, length),
    est_at_24_mo = map_dbl(est_at_24_mo,
                           ~if_else(est_at_24_mo_len == 1, .x[1], NA_real_)),

    # Interpolated value OK
    est_at_50w_at_24mo = est_at_24_mo > 50,

    delay2 = if_else(est_at_50w_at_24mo, "no_delay", "delay")

  ) %>%
  select(data_id, data, est_at_24_mo, delay2)

# Clean up results
BplusE_24mo <- left_join(BplusE_24mo_1, BplusE_24mo_unk) %>%
  mutate(
    delay_status = if_else(is.na(delay2), delay, delay2)
  ) %>%
  select(proj, status, data_id, delay_status)

table(BplusE_24mo$status, BplusE_24mo$delay_status)

# Add delay status to saved file
BplusE <- left_join(BplusE, BplusE_24mo,by = c("proj", "status", "data_id"))

save_data(BplusE_d, "LCA/BplusE_demographics.rds")
save_data(BplusE, "LCA/BplusE.rds")

# Load Wordbank as double check ####

wb_wg <- read_data("Wordbank/WG-scored.rds")$n
wb_ws <- read_data("Wordbank/WS-scored.rds")$n

wb <- bind_rows(wb_wg, wb_ws) %>%
  select(data_id, age, all_of(eirli_cols)) %>%
  mutate(
    inventory_total = select(., -data_id, -age) %>%
                        rowSums(na.rm = TRUE),
    status = "WB"
  ) %>%
  select(data_id, age, status, inventory_total)

# Trajectory comparisons ####

B_E_WB <- BplusE  %>%
  select(data_id, exact_age, age, status, inventory_total) %>%
  bind_rows(wb) %>%
  mutate(
    exact_age = if_else(!is.na(exact_age), exact_age, age),
    project = if_else(status %in% c("lg_dx", "no_follow_up", "no_lg_dx"),
                      "EIRLI", status)
  )

png("plots/setup/comparison-facet.png", width = 6, height = 4, units = "in",
    res = 72)

ggplot(B_E_WB, aes(x = jitter(exact_age), y = inventory_total,
                   color = status)) +
  geom_point(alpha = 0.2, size = 1, shape = 20) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(color = "black") +
  facet_wrap(vars(status)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Age (mo.)", y = "IA total")

dev.off()

B_WB <- B_E_WB %>%
  filter(
    status %in% c("BCP", "WB")
  )

range(B_WB$inventory_total)

png("plots/setup/comparison-BCP_v_Wordbank.png", width = 6, height = 4,
    units = "in", res = 72)

ggplot(B_WB, aes(x = age, y = inventory_total, color = status)) +
  geom_point(aes(shape = status), alpha = 0.5) +
  scale_shape_manual(values = c(16, NA)) +
  geom_line(aes(group = interaction(data_id, status)), alpha = 0.25,
            color = "black") +
  geom_smooth(aes(fill = status)) +
  theme_bw()

dev.off()

png("plots/setup/comparison-trajectories_only.png", width = 6, height = 4,
    units = "in", res = 72)

ggplot(B_E_WB, aes(x = exact_age, y = inventory_total, color = status,
                   fill = status)) +
  geom_smooth(aes(linetype = project)) +
  scale_linetype_manual(values = c("solid", "dotted", "twodash")) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

# Norming ####

norming_subs <- read_data("Wordbank/vocabulary_norms_data.csv")

## Create norms based on full data #####

wb_full_norm <- norming_subs %>%
  group_by(age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    wb_ecdf = map(data, ~ecdf(.x$vocab))
  ) %>%
  select(-data)

## Create norms with only EIRLI columns #####

wb_eirli_norm <- bind_rows(wb_wg, wb_ws) %>%
  select(data_id, age, all_of(eirli_cols)) %>%
  filter(
    data_id %in% norming_subs$data_id
  ) %>%
  mutate(
    inventory_total = select(., -data_id, -age) %>%
                        rowSums()
  ) %>%
  select(data_id, age, inventory_total) %>%
  group_by(age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    wb_e_ecdf = map(data, ~ecdf(.x$inventory_total))
  ) %>%
  select(-data)

## without sounds ####

wb_noS_norm <- bind_rows(wb_wg, wb_ws) %>%
  select(data_id, age, all_of(eirli_cols), -sounds) %>%
  filter(
    data_id %in% norming_subs$data_id
  ) %>%
  mutate(
    inventory_total = select(., -data_id, -age) %>%
      rowSums()
  ) %>%
  select(data_id, age, inventory_total) %>%
  group_by(age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    wb_noS_ecdf = map(data, ~ecdf(.x$inventory_total))
  ) %>%
  select(-data)

# Compare BCP to various norms ####

bcp_norm <- bcp %>%
  select(data_id, age, ends_with("_total")) %>%
  mutate(
    age = round(age)
  ) %>%
  filter(
    age >= 16,
    age <= 30
  ) %>%
  left_join(wb_full_norm, by = "age") %>%
  left_join(wb_eirli_norm, by = "age") %>%
  left_join(wb_noS_norm, by = "age") %>%
  rowwise() %>%
  mutate(
    q     = map_dbl(inventory_total, ~wb_ecdf(.x)),
    q_E   = map_dbl(eirli_total, ~wb_e_ecdf(.x)),
    q_noS = map_dbl(noS_total, ~wb_noS_ecdf(.x)),
  )

png("plots/setup/BCP_quantile_trends.png", width = 8, height = 6, units = "in",
    res = 72)

ggplot(bcp_norm, aes(x = age, y = q)) +
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue", aes(group = data_id)) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_hline(yintercept = mean(bcp_norm$q), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Quantile",
       title = "Full BCP vs. Wordbank norm")

dev.off()

ggplot(bcp_norm, aes(x = age, y = q_E)) +
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue", aes(group = data_id)) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_hline(yintercept = mean(bcp_norm$q_E), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Quantile",
       title = "Full BCP vs. Wordbank norm using EIRLI columns")

ggplot(bcp_norm, aes(x = age, y = q_noS)) +
  geom_point(color = "lightblue") +
  geom_line(color = "lightblue", aes(group = data_id)) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_hline(yintercept = mean(bcp_norm$q_noS), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Quantile",
       title = "Full BCP vs. Wordbank norm using EIRLI columns (less sounds)")

bcp_norm %>%
  select(starts_with("q")) %>%
  colMeans()
