path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)

source("../mcdi-setup.R")

# Load data ####

BplusE_d <- read_data("LCA/BplusE_demographics.rds")

Badj <- read_data("LCA/BCP_to_EIRLI.rds")

BplusE <- read_data("LCA/BplusE.rds") %>%
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
  )

ggplot(BplusE, aes(x = jitter(age), y = inv_total_adj, color = status)) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(aes(group = status)) +
  scale_x_continuous(breaks = 11:38, minor_breaks = NULL) +
  theme_bw()

# LCA ####

k <- 2:5

BplusE2 <- BplusE %>%
  group_by(data_id) %>%
  mutate(
    data_id_num = cur_group_id()
  )

LCA_ng1 <- lcmm(inventory_total ~ age,
                data     = BplusE2,
                subject  = "data_id_num",
                ng       = 1,
                link     = "3-manual-splines",
                intnodes = 615 / exp(1))

LCAs <- lapply(k,
               function(k)
                 lcmm(inventory_total ~ age,
                 data     = BplusE2,
                 subject  = "data_id_num",
                 ng       = k,
                 link     = "3-manual-splines",
                 mixture  = ~age,
                 intnodes = 615 / exp(1)))

fit_stats <- get_fit_stats(LCA_ng1, LCAs)
plot_fit_stats(fit_stats, bg = "#ffffff")

## Selected model ####

sel_model_groups <- LCAs[[2]]$pprob
BplusE_groups <- left_join(BplusE2, sel_model_groups)

ggplot(BplusE_groups, aes(x = exact_age, y = inv_total_adj, color = proj)) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(aes(group = class)) +
  theme_bw()

BplusE_groups %>%
  select(data_id, status, class) %>%
  distinct() %>%
  group_by(status, class) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(names_from = status, values_from = n)

# Healthy development only ####

BplusE3 <- BplusE %>%
  group_by(data_id) %>%
  mutate(
    data_id_num = cur_group_id()
  ) %>%
  filter(
    status %in% c("BCP", "no_lg_dx")
  )

TD_LCA_ng1 <- lcmm(inventory_total ~ age,
                    data     = BplusE3,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    intnodes = 615 / exp(1))

TD_LCAs <- lapply(k,
                   function(k)
                     lcmm(inventory_total ~ age,
                          data     = BplusE3,
                          subject  = "data_id_num",
                          ng       = k,
                          link     = "3-manual-splines",
                          mixture  = ~age,
                          intnodes = 615 / exp(1)))

TD_fit_stats <- get_fit_stats(LCA_ng1, LCAs)
plot_fit_stats(TD_fit_stats, bg = "#ffffff")

TD <- BplusE3 %>%
  left_join(
    TD_LCAs[[2]]$pprob
  ) %>%
  mutate(
    class = as.factor(class)
  )

ggplot(TD, aes(x = exact_age, y = inv_total_adj, color = class)) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(aes(linetype = proj)) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "Inventory total")

TD %>%
  select(data_id, proj, class) %>%
  distinct() %>%
  group_by(proj, class) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(names_from = proj, values_from = n) %>%
  arrange(class)


# Summary stats ####

TD_stats <- BplusE_d %>%
  left_join(
    select(TD, data_id, class, starts_with("prob"))
  ) %>%
  filter(
    !is.na(class)
  )

TD_sex <- TD_stats %>%
  group_by(class, sex) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(names_from = sex, values_from = n)

chisq.test(TD_sex[, 2:3])

TD_med <- TD_stats %>%
  group_by(class, mother_ed) %>%
  filter(
    !is.na(mother_ed)
  ) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(names_from = mother_ed, values_from = n) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )

chisq.test(TD_med[, -1])
