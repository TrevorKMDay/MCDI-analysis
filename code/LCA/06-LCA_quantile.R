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

Badj <- read_data("LCA/BCP_to_EIRLI.rds") %>%
  select(age, diff)

BplusE_d <- read_data("LCA/BplusE_demographics.rds") %>%
  select(proj, data_id, sex, mother_college) %>%
  mutate(
    male = sex == "Male"
  ) %>%
  select(-sex)

BplusE <- read_data("LCA/BplusE.rds") %>%
  left_join(Badj) %>%
  mutate(
    # For BCP participants, adjust the EIRLI total upward, but for EIRLI,
    # keep it. Also, truncate values at 615
    inv_total_adj = if_else(proj == "BCP", round(eirli_total - diff),
                            eirli_total) %>%
      if_else(. > 615, 615, .),
    empty_prior   = 0
  ) %>%
  ungroup() %>%
  left_join(BplusE_d)

delay <- BplusE %>%
  select(proj, status, data_id, delay_status) %>%
  distinct()

# table(delay$status, delay$delay_status.x) %>%
#   addmargins()

BplusE %>%
  group_by(status) %>%
  summarize(
    min = min(inv_total_adj),
    max = max(inv_total_adj)
  )

####

BE_Bn <- BplusE %>%
  filter(
    status %in% c("BCP", "no_lg_dx"),
    # !(age == 24 & inventory_total <= 50)
  )

BE_Bn0 <- BplusE %>%
  filter(
    status %in% c("BCP", "no_lg_dx", "no_follow_up"),

  )

# Covariate selection ####

LCA_ng1_Bn0_0 <- lcmm(inv_total_adj ~ age,
                       data     = BE_Bn0,
                       subject  = "data_id_num",
                       ng       = 1,
                       link     = "3-manual-splines",
                       prior    = "empty_prior",
                       intnodes = 615 / exp(1))

LCA_ng1_Bn0_1 <- lcmm(inv_total_adj ~ age + male,
                      data     = BE_Bn0,
                      subject  = "data_id_num",
                      ng       = 1,
                      link     = "3-manual-splines",
                      prior    = "empty_prior",
                      intnodes = 615 / exp(1))

LCA_ng1_Bn0_2 <- lcmm(inv_total_adj ~ age + male + mother_college,
                      data     = BE_Bn0,
                      subject  = "data_id_num",
                      ng       = 1,
                      link     = "3-manual-splines",
                      prior    = "empty_prior",
                      intnodes = 615 / exp(1))

LCA_ng1_Bn0_demosel <- list(LCA_ng1_Bn0_0, LCA_ng1_Bn0_1, LCA_ng1_Bn0_2)
fitstats_ng1_Bn0_demosel <- get_fit_stats(list_of_models = LCA_ng1_Bn0_demosel)

png("plots/covariate_selection.png", width = 6, height = 3, units = "in",
    res = 300)

plot_fit_stats(fitstats_ng1_Bn0_demosel)

dev.off()

# Use this as the "master" ng1 model
LCA_ng1_Bn0 <- LCA_ng1_Bn0_1

# Latent classes =====
#

k <- 2:6

LCAs_Bn0_2_6 <- lapply(k,
                 function(k)
                   lcmm(inv_total_adj ~ age + male,
                         data     = BE_Bn0,
                         subject  = "data_id_num",
                         ng       = k,
                         link     = "3-manual-splines",
                         mixture  = ~age,
                         B        = LCA_ng1_Bn0,
                         prior    = "empty_prior",
                         intnodes = 615 / exp(1)))

LCAs_Bn0_nosex_2_6 <- lapply(k,
                       function(k)
                         lcmm(inv_total_adj ~ age,
                              data     = BE_Bn0,
                              subject  = "data_id_num",
                              ng       = k,
                              link     = "3-manual-splines",
                              mixture  = ~age,
                              B        = LCA_ng1_Bn0_0,
                              prior    = "empty_prior",
                              intnodes = 615 / exp(1)))

save_data(LCAs, "results/BplusE_nodx_LCA-2_6.rds")

LCAs_fitstats <- get_fit_stats(LCA_ng1_Bn0, LCAs_Bn0)
LCAs_nosex_fitstats <- get_fit_stats(LCA_ng1_Bn0_0, LCAs_Bn0_nosex_2_6)

png("plots/lca/fitstats_ng1_Bn0_agesex.png", width = 5, height = 4,
    units = "in", res = 300)

plot_fit_stats(LCAs_fitstats, title = "Age + sex")

dev.off()

png("plots/lca/fitstats_ng1_Bn0_age.png", width = 5, height = 4,
    units = "in", res = 300)

plot_fit_stats(LCAs_nosex_fitstats, title = "Age only")

dev.off()

# Bn only =====

LCA_ng1_Bn_0 <- lcmm(inv_total_adj ~ age,
                      data     = BE_Bn,
                      subject  = "data_id_num",
                      ng       = 1,
                      link     = "3-manual-splines",
                      prior    = "empty_prior",
                      intnodes = 615 / exp(1))

LCAs_Bn_2_6 <- lapply(k,
                       function(k)
                         lcmm(inv_total_adj ~ age,
                              data     = BE_Bn,
                              subject  = "data_id_num",
                              ng       = k,
                              link     = "3-manual-splines",
                              mixture  = ~age,
                              B        = LCA_ng1_Bn_0,
                              prior    = "empty_prior",
                              intnodes = 615 / exp(1)))

LCAs_Bn_fitstats <- get_fit_stats(LCA_ng1_Bn_0, LCAs_Bn_2_6)
plot_fit_stats(LCAs_Bn_fitstats)

## Four-group solutions ====

groups_Bn <- LCAs_Bn_2_6[[2]]$pprob
groups_Bn0 <- LCAs_Bn0_nosex_2_6[[3]]$pprob

# Full join to include participants in either
groups_both <- full_join(groups_Bn, groups_Bn0, by = "data_id_num",
                         suffix = c(".Bn", ".Bn0"))

groups_tbl <- table(groups_both$class.Bn, groups_both$class.Bn0, useNA = "a")

stability <- apply(groups_tbl, 1, max) / rowSums(groups_tbl)
weighted.mean(stability[1:3], w = rowSums(groups_tbl)[1:3], na.rm = TRUE)

groups_Bn0 %>%
  pivot_longer(-c(data_id_num, class), names_to = "prob") %>%
  group_by(class, prob) %>%
  mutate(
    prob = as.numeric(str_remove(prob, "prob"))
  ) %>%
  summarize(
    min = min(value),
    max = max(value)
  ) %>%
  filter(
    class == prob
  )

BE_results <- BE_Bn0 %>%
  select(proj, status, data_id, data_id_num, male, ends_with("age"),
         inventory_total, inv_total_adj) %>%
  left_join(groups_Bn0) %>%
  left_join(groups_Bn, by = "data_id_num", suffix = c(".Bn0", ".Bn")) %>%
  mutate(
    across(starts_with("class"), as.factor),
    status = case_when(
              status == "BCP" ~ "BCP",
              status == "no_follow_up" ~ "EIRLI Dx(unk.)",
              status == "no_lg_dx" ~ "EIRLI Dx(-)"
            ),
    # prob = case_when(
    #           class == 1 ~ prob1,
    #           class == 2 ~ prob2,
    #           class == 3 ~ prob3,
    #           class == 4 ~ prob4,
    #         )
  ) %>%
  filter(
    !is.na(class.Bn0)
  )

save_data(BE_Bn0_results, "LCA/BN0_NG4_results.rds")

png("plots/lca/BN0_ng4_trajectories.png", width = 6.5, height = 4,
    units = "in", res = 300)

ggplot(BE_results, aes(x = exact_age, y = inventory_total, color = class.Bn0)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(aes(group = class.Bn0), color = "black") +
  annotate("rect", xmin = 24, ymin = -Inf, xmax = Inf, ymax = 50,
           color = "black", linetype = "dashed", fill = "red", alpha = 0.25) +
  annotate("text", x = 32, y = 0, label = "Delay 3", size = 3.5,
           color = "darkred") +
  scale_x_continuous(limits = c(11, 40), breaks = seq(6, 40, by = 6),
                     minor_breaks = 6:40) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(status), scales = "free_y") +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Inventory", color = "Class") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom")

dev.off()

## Reanalyze without delay3 candidates ====

BE_Bn0_nodelay <- BplusE %>%
  filter(
    status != "lg_dx"
  )

BE_Bn0_nodelay %>%
  select(status, data_id, delay_status) %>%
  distinct() %>%
  select(-data_id) %>%
  table()

BE_Bn0_nodelay <- BplusE %>%
  filter(
    status != "lg_dx",
    delay_status != "delay"
  )

### Run models ----

LCA_ng1_Bn0nd <- lcmm(inv_total_adj ~ age,
                       data     = BE_Bn0_nodelay,
                       subject  = "data_id_num",
                       ng       = 1,
                       link     = "3-manual-splines",
                       prior    = "empty_prior",
                       intnodes = 615 / exp(1))

LCAs_ng2_6_Bn0nd <- lapply(k,
                            function(k)
                              lcmm(inv_total_adj ~ age,
                                   data     = BE_Bn0_nodelay,
                                   subject  = "data_id_num",
                                   ng       = k,
                                   link     = "3-manual-splines",
                                   mixture  = ~age,
                                   B        = LCA_ng1_Bn0nd,
                                   prior    = "empty_prior",
                                   intnodes = 615 / exp(1)))

LCAs_nodelay_fitstats <- get_fit_stats(LCA_ng1_Bn0nd, LCAs_ng2_6_Bn0nd)
plot_fit_stats(LCAs_nodelay_fitstats)

BE_results2 <- BE_results %>%
  full_join(LCAs_ng2_6_Bn0nd[[2]]$pprob, by = "data_id_num") %>%
  select(-starts_with("prob")) %>%
  rename(
    # Rename class from third model
    class.Bn0_nd = class
  ) %>%
  mutate(
    class.Bn0_nd = as.factor(class.Bn0_nd)
  ) %>%
  filter(
    # Remove those that weren't included in the model (BE_results has everyone)
    data_id %in% BE_Bn0_nodelay$data_id
  )

BE_results2_indiv <- BE_results2 %>%
  select(data_id, starts_with("class.")) %>%
  distinct() %>%
  select(class.Bn, class.Bn0_nd)

groups_nd_tbl <- table(BE_results2_indiv, useNA = "a")

stability2 <- apply(groups_nd_tbl, 1, max) / rowSums(groups_nd_tbl)
weighted.mean(stability2[1:3], w = rowSums(groups_nd_tbl)[1:3], na.rm = TRUE)
