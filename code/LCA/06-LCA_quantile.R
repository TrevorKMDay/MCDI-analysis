path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)
library(viridis)

source("../mcdi-setup.R")
source("00-LCA_functions.R")

# Load data ####

BplusE_d <- read_data("LCA/BplusE_demographics.rds")

Badj <- read_data("LCA/BCP_to_EIRLI.rds")

BplusE <-read_data("LCA/BplusE.rds") %>%
  ungroup() %>%
  select(proj, status, data_id, age, exact_age, inventory_total) %>%
  mutate(
    age = round(age)
  ) %>%
  left_join(select(Badj, age, diff), by = "age") %>%
  mutate(
    # Only adjust BCP
    inv_total_adj = if_else(proj == "BCP", inventory_total - diff,
                            inventory_total),
    # Don't let BCP exceed ceiling
    inv_total_adj = if_else(inv_total_adj > 616, 616, inv_total_adj)
  ) %>%
  group_by(data_id) %>%
  mutate(
    data_id_num = cur_group_id(),
    empty_prior = 0
  )

BE_Bn <- BplusE %>%
  filter(
    status %in% c("BCP", "no_lg_dx"),
    !(age == 24 & inventory_total <= 50)
  )

BE_Bn0 <- BplusE %>%
  filter(
    status %in% c("BCP", "no_lg_dx", "no_follow_up"),
    !(age == 24 & inventory_total <=50)
  )

# LCA ####

k <- 2:5

LCA_ng1_Bn <- lcmm(inv_total_adj ~ age,
                    data     = BE_Bn,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    B        = rep(1, 6),
                    prior    = "empty_prior",
                    intnodes = 615 / exp(1))

LCA_ng1_Bn0 <- lcmm(inv_total_adj ~ age,
                     data     = BE_Bn0,
                     subject  = "data_id_num",
                     ng       = 1,
                     link     = "3-manual-splines",
                     B        = rep(1, 6),
                     prior    = "empty_prior",
                     intnodes = 615 / exp(1))

save_data(LCA_ng1, "results/BplusE_nodx_LCA-1.rds")

LCAs_Bn <- lapply(k,
               function(k)
                 lcmm(inv_total_adj ~ age,
                       data     = BE_Bn,
                       subject  = "data_id_num",
                       ng       = k,
                       link     = "3-manual-splines",
                       mixture  = ~age,
                       B        = LCA_ng1_Bn,
                       prior    = "empty_prior",
                       intnodes = 615 / exp(1)))

LCAs_Bn0 <- lapply(k,
                  function(k)
                    lcmm(inv_total_adj ~ age,
                         data     = BE_Bn0,
                         subject  = "data_id_num",
                         ng       = k,
                         link     = "3-manual-splines",
                         mixture  = ~age,
                         B        = LCA_ng1_Bn0,
                         prior    = "empty_prior",
                         intnodes = 615 / exp(1)))

save_data(LCAs, "results/BplusE_nodx_LCA-2_6.rds")

fit_stats <- get_fit_stats(LCA_ng1_Bn, LCAs_Bn)
plot_fit_stats(fit_stats, bg = "#ffffff")

fit_stats <- get_fit_stats(LCA_ng1_Bn0, LCAs_Bn0)
plot_fit_stats(fit_stats, bg = "#ffffff")

## Compare Bn/Bn0 =====

compare_Bn_Bn0 <- BE_Bn0 %>%
  ungroup() %>%
  left_join(LCAs_Bn[[2]]$pprob, by = "data_id_num") %>%
  left_join(LCAs_Bn0[[2]]$pprob, by = "data_id_num", suffix = c(".Bn", ".Bn0"))

compare_Bn_Bn0_long <- compare_Bn_Bn0 %>%
  select(status, data_id, exact_age, inv_total_adj, starts_with("class")) %>%
  pivot_longer(starts_with("class"), values_to = "class") %>%
  separate(name, into = c(NA, "analysis"), sep = "[.]")

compare_tbl <- compare_Bn_Bn0 %>%
  group_by(status, class.Bn, class.Bn0) %>%
  summarize(
    n = n()
  ) %>%
  group_by(status, class.Bn) %>%
  mutate(
    class1.n = sum(n),
    p = round(100 * n / class1.n)
  )

ggplot(compare_Bn_Bn0_long, 
       aes(x = exact_age, y = inv_total_adj, color = as.factor(class))) +
  geom_point() +
  geom_line(aes(group = data_id), alpha = 0.5) +
  geom_smooth(color = "black", aes(linetype = as.factor(class))) +
  scale_x_continuous(breaks = c(16, 20, 24, 28, 33, 36), 
                     minor_breaks = 11:38) +
  facet_grid(cols = vars(analysis), rows = vars(status)) +
  theme_bw()

## Selected model ####

sel_model_groups <- LCAs[[2]]$pprob
BplusE_groups <- left_join(BplusE2, sel_model_groups)

png("plots/lca/5-TD_trajectories.png", width = 8, height = 5, units = "in",
    res = 300)

ggplot(BplusE_groups, aes(x = exact_age, y = inv_total_adj, color = proj)) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(aes(linetype = as.factor(class))) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Total inventory size", color = "Project",
       linetype = "Class")

dev.off()

### BCP groups ####

BCP_groups <- BplusE_groups %>%
  filter(
    proj == "BCP"
  ) %>%
  select(-proj, -status) %>%
  ungroup() %>%
  mutate(
    prob = if_else(class == 1, prob1,
                   if_else(class == 2, prob2, prob3))
  )

ggplot(BCP_groups, aes(x = exact_age, y = inventory_total, color = prob)) +
  geom_point() +
  geom_line(aes(group = data_id)) +
  geom_smooth() +
  geom_hline(yintercept = 50, color = "red") +
  geom_vline(xintercept = 24, color = "red") +
  scale_color_viridis() +
  facet_wrap(vars(class)) +
  theme_bw()

BCP_groups_d <- BplusE_d %>%
  filter(
    data_id %in% BCP_groups$data_id
  ) %>%
  select(-proj) %>%
  left_join(
    select(BCP_groups, data_id, class)
  ) %>%
  distinct()

BCP_g_dsummary <- BCP_groups_d %>%
  group_by(class) %>%
  dplyr::summarize(
    n = n(),
    male = sum(sex == "Male"),
    female = sum(sex == "Female"),
    # n_gmissing = sum(is.na(sex)),
    momed_missing = sum(is.na(mother_college)),
    mom_coll = sum(mother_college),
    mom_no_coll = sum(!mother_college)
  )

BCP_g_dsummary %>%
  select(male, female) %>%
  chisq.test(simulate.p.value = TRUE)

BCP_g_dsummary %>%
  select(mom_coll, mom_no_coll) %>%
  chisq.test(simulate.p.value = TRUE)

# Analyze BCP classes =====

mullen <- read_data("BCP/bcp-vl_mullen-220209.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, mullen.Candidate_Age,
         mullen.Administration, matches("mullen..*_t$")) %>%
  rename(
    data_id = demographics.CandID,
    exact_age = mullen.Candidate_Age
  ) %>%
  filter(
    mullen.Administration == "All"
  ) %>%
  mutate(
    data_id = as.character(data_id),
    across(c(exact_age, ends_with("_t")), as.numeric)
  ) %>%
  select(-mullen.Administration) %>%
  pivot_longer(ends_with("_t"), names_to = "behavior") %>%
  mutate(
    behavior = str_replace(behavior, "mullen.", "m.") %>%
      str_remove("_t$") %>%
      str_replace("language", "lg")
  ) %>%
  group_by(data_id, behavior) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    mean = map_dbl(data, ~mean(.x$value, na.rm = TRUE)),
    wmean = map_dbl(data, ~weighted.mean(.x$value, .x$exact_age,
                                         na.rm = TRUE)),
    zwmean = scale(wmean)[, 1]
  )

vl <- readxl::read_xlsx("../../data/BCP/20210210_Vineland_BCP.xlsx") %>%
  select(CandID, Candidate_Age, ends_with("STD_SCORE")) %>%
  rename(
    data_id = CandID,
    exact_age = Candidate_Age
  ) %>%
  pivot_longer(-c(data_id, exact_age), names_to = "behavior") %>%
  mutate(
    value = as.numeric(value),
    behavior = paste0("vl.", behavior) %>%
      str_remove("_STD_SCORE")
  ) %>%
  group_by(data_id, behavior) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    mean = map_dbl(data, ~mean(.x$value, na.rm = TRUE)),
    wmean = map_dbl(data, ~weighted.mean(.x$value, .x$exact_age,
                                         na.rm = TRUE)) %>%
      replace(., is.nan(.), NA),
  ) %>%
  na.omit() %>%
  mutate(
    data_id = as.character(data_id),
    zwmean = scale(wmean)[, 1]
  )

behav <- bind_rows(mullen, vl)

BCP <- BCP_groups_d %>%
  left_join(select(behav, -data), by = "data_id") %>%
  filter(
    !is.na(behavior)
  ) %>%
  mutate(
    instrument = if_else(str_detect(behavior, "^m."), "Mullen", "Vineland")
  )

ggplot(BCP, aes(x = behavior, y = zwmean, fill = as.factor(class))) +
  geom_boxplot(notch = TRUE, notchwidth = .6) +
  theme_bw() +
  labs(x = "Behavior", y = "Z score", fill = "Class") +
  facet_grid(cols = vars(instrument), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## MLU ####

mlu <- read_data("BCP/BCP_WS_MLU3-200609.rds") %>%
  select(data_id, age, MLU3m) %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  left_join(
    select(BCP, data_id, class)
  )

ggplot(mlu, aes(x = age, y = MLU3m, color = as.factor(class))) +
  geom_point(shape = 1) +
  geom_line(aes(group = data_id), alpha = 0.5) +
  geom_smooth(method = "lm")+
  theme_bw() +
  labs(x = "Age (mo.)", y = "MLU3-morphemes", color = "Class")

# EIRLI DX0 ====


