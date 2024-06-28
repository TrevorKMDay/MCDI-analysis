path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)

source("../mcdi-setup.R")
source("00-LCA_functions.R")

eirli_cols <- c("action_words", "animals", "body_parts", # "COMPLEXITY",
                "clothing", "connecting_words", "descriptive_words",
                "food_drink", "furniture_rooms", "games_routines",
                "helping_verbs", "people", "locations", "pronouns",
                "quantifiers", "question_words", "household", "sounds",
                "toys", "vehicles")

# Load data =====

wg <- read_data("Wordbank/WG-scored.rds")$n %>%
  select(data_id, age, any_of(eirli_cols)) %>%
  mutate(
    inventory_total = select(., any_of(eirli_cols)) %>%
      rowSums(),
    inst = "WG",
  ) %>%
  select(-any_of(eirli_cols))

ws <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(data_id, age, all_of(eirli_cols)) %>%
  mutate(
    inventory_total = select(., all_of(eirli_cols)) %>%
      rowSums(),
    inst = "WS"
  ) %>%
  select(-any_of(eirli_cols))

wb <- bind_rows(wg, ws) %%
  group_by(inst, age) %>%
  nest() %>%
  arrange(age) %>%
  mutate(
    n    = map_int(data, nrow),
    ecdf = map(data, ~ecdf(.x$inventory_total))
  ) %>%
  filter(
    # There is some overlap in when the different forms were given; to be
    #   consistent, use only Words & Sentences at 16 months or older
    (inst == "WG" & age <= 15) | (inst == "WS" & age >= 16)
  )

rm(ws, wg)

Badj <- read_data("LCA/BCP_to_EIRLI.rds") %>%
  select(age, diff)

BE <- read_data("LCA/BplusE.rds") %>%
  select(proj, status, data_id, data_id_num, age, exact_age,
         eirli_total) %>%
  left_join(Badj, by = "age") %>%
  ungroup() %>%
  filter(
    age >= 8,
    age <= 30
  ) %>%
  left_join(select(wb, -data), by = "age") %>%
  rowwise() %>%
  mutate(
    eirli_total_adj = if_else(proj == "BCP", eirli_total - diff, eirli_total),
    pctile = map_dbl(eirli_total, ~ecdf(.x)),
    pctile_adj = map_dbl(eirli_total_adj, ~ecdf(.x))
  ) %>%
  ungroup() %>%
  pivot_longer(starts_with("pctile"), values_to = "pctile") %>%
  mutate(
    name = recode(name, "pctile" = "raw", "pctile_adj" = "adj")
  ) %>%
  filter(
    # !is.na(eirli_total_adj)
  )

ggplot(BE, aes(x = exact_age, y = pctile, color = status)) +
  geom_line(aes(group = interaction(data_id, name)), alpha = 0.1) +
  geom_smooth(aes(linetype = name)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw()

no_lg_dx <- BE %>%
  filter(
    status %in% c("no_lg_dx", "no_follow_up"),
    # Use the adjusted values (this only affects BCP)
    name == "adj"
  ) %>%
  select(-proj, -status, -ecdf)

# Run percentile models ====

if (!file.exists(.data("LCA/pctile_LCA_ng1-6.rds"))) {

  pctile_LCA_ng1 <- lcmm(pctile ~ exact_age,
                         data = no_lg_dx,
                         subject = "data_id_num",
                         ng = 1,
                         link = "4-equi-splines",
                         nproc = detectCores() - 1)

  pctile_LCA_ng2_6 <- lapply(2:6,
                             function(ng)
                               lcmm(pctile ~ exact_age,
                                    data = no_lg_dx,
                                    subject = "data_id_num",
                                    ng = ng,
                                    mixture = ~exact_age,
                                    B = pctile_LCA_ng1,
                                    link = "4-equi-splines",
                                    nproc = detectCores() - 1))

  save_data(list(pctile_LCA_ng1, pctile_LCA_ng2_6), "LCA/pctile_LCA_ng1-6.rds")

} else {

  pctile_LCA <- read_data("LCA/pctile_LCA_ng1-6.rds")
  pctile_LCA_ng1 <- pctile_LCA[[1]]
  pctile_LCA_ng2_6 <- pctile_LCA[[2]]

}

pctile_fitstats <- get_fit_stats(pctile_LCA_ng1, pctile_LCA_ng2_6)
plot_fit_stats(pctile_fitstats)

## Selected model ====
class_results <- left_join(no_lg_dx, pctile_LCA_ng2_6[[2]]$pprob)

ggplot(class_results,
       aes(x = exact_age, y = pctile, color = as.factor(class))) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth() +
  theme_bw() +
  labs(title = "Percentile-based LCA", x = "Age (mo.)", y = "%ile",
       color = "Class")

# Lexical composition =====

func_words <- WS_1A[16:22]
nouns <- WS_1A[!(WS_1A %in% c(func_words, "action_words",
                              "descriptive_words"))]

BE_lc <- read_data("LCA/BplusE.rds") %>%
  select(proj, status, data_id, data_id_num, age, exact_age,
         all_of(eirli_cols)) %>%
  ungroup() %>%
  pivot_longer(all_of(eirli_cols), names_to = "category") %>%
  group_by(proj, data_id, age) %>%
  mutate(

    total_words = sum(value),

    lexical_class = case_when(
      category == "action_words" ~ "verbs",
      category == "descriptive_words" ~ "adjectives",
      category %in% func_words ~ "function_words",
      category %in% nouns ~ "nouns",
      # TRUE ~ NA
    )

  ) %>%
  group_by(proj, status, data_id, data_id_num, age, exact_age, total_words,
           lexical_class) %>%
  summarize(
    class_total = sum(value)
  ) %>%
  mutate(
    class_pct = class_total / total_words
  )

BE_lc_pct <- BE_lc %>%
  pivot_wider(id_cols = c(proj, data_id, data_id_num, age, exact_age),
              names_from = lexical_class,
              values_from = c(class_pct, class_total)) %>%
  select_all(~str_remove(., "class_")) %>%
  ungroup() %>%
  as.data.frame()

if (!file.exists(.data("LCA/lexcomp_multLCA_ng1-6.rds"))) {

  # 433.91 sec = 7 minutes (1 core)
  lexcomp_pct_LCA_ng1 <- multlcmm(pct_adjectives + pct_function_words +
                                    pct_nouns + pct_verbs ~ exact_age,
                                  data = BE_lc_pct,
                                  subject = "data_id_num",
                                  random = ~1 + age,
                                  link = "4-equi-splines")

  # ng=2 283.89 sec (4.7 min) on 7 cores
  # ng=6 1079.59 sec (18.0 min) on 7 cores
  lexcomp_pct_LCA_ng2_6 <- lapply(2:6,
                                  function(ng)
                                    multlcmm(pct_adjectives +
                                               pct_function_words +
                                                pct_nouns + pct_verbs ~
                                               exact_age,
                                              data = BE_lc_pct,
                                              subject = "data_id_num",
                                              random = ~1 + age,
                                              mixture = ~exact_age,
                                              link = "4-equi-splines",
                                              B = lexcomp_pct_LCA_ng1,
                                              ng = ng,
                                              nproc = 7))


  save_data(list(lexcomp_pct_LCA_ng1, lexcomp_pct_LCA_ng2_6),
            "LCA/lexcomp_multLCA_ng1-6.rds")

} else {

  lexcomp_multLCA <- read_data("LCA/lexcomp_multLCA_ng1-6.rds")
  lexcomp_pct_LCA_ng1 <- lexcomp_multLCA[[1]]
  lexcomp_pct_LCA_ng2_6 <- lexcomp_multLCA[[2]]

}

lexcomp_pct_fitstats <- get_fit_stats(lexcomp_pct_LCA_ng1,
                                      lexcomp_pct_LCA_ng2_6)
plot_fit_stats(lexcomp_pct_fitstats)
