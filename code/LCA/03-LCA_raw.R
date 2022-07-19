path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)
library(viridis)
library(patchwork)

source("../mcdi-setup.R")
source("00-LCA_functions.R")

# Load data ####

BE <- read_data("LCA/BplusE.rds") %>%
  mutate(
    empty_prior = 0
  ) %>%
  ungroup()

BEd <- read_data("LCA/BplusE_demographics.rds")

group_size <- BE %>%
  select(proj, status, data_id) %>%
  filter(
    proj == "EIRLI"
  ) %>%
  group_by(status) %>%
  summarize(
    n = n()
  ) %>%
  pull(n)

dx_rate <- group_size[1] / (group_size[1] + group_size[3])

# Maximum is NOT 680 since EIRLI is included
max_inv <- max(BE$inventory_total)

# WS 1A inventory category labels
WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")


# LCA: Part IA ####

set.seed(55455)

# LCA on part IA outcome only

# For all LCAs, run the one-group LCA first as to establish baseline, it is
# used in the multi-group LCAs

# TO DO: Grid searches

lca_IA_ng1 <- lcmm(inventory_total ~ ageC,
                   data     = BE,
                   subject  = "data_id_num",
                   ng       = 1,
                   link     = "3-manual-splines",
                   intnodes = max_inv / exp(1),
                   B        = rep(1, 6),
                   prior    = "empty_prior")

lca_1A_ng2_6 <- lapply(2:6, function(x)
                        lcmm(inventory_total ~ ageC,
                             data     = BE,
                             subject  = "data_id_num",
                             ng       = x,
                             mixture  = ~ageC,
                             link     = "3-manual-splines",
                             intnodes = max_inv / exp(1),
                             B        = lca_IA_ng1,
                             prior    = "empty_prior"))

# Extract the seven fit stats (AIC, BIC, CAIC, CLC, ICL.BIC, ss.BIC, NEC)
# from the one-group model and the list of higher-order models for plotting
fitstats_IA <- get_fit_stats(lca_IA_ng1, lca_1A_ng2_6)

dir.create("plots/modeling/", showWarnings = FALSE, recursive = TRUE)

png("plots/modeling/fitstats_IA.png", width = 6.91, height = 3.28,
    units = "in", res = 150)

plot_fit_stats(fitstats_IA)

dev.off()

## Unsupervised two-group LCA ####

# Leave BE object alone, BE2 is going to start collecting LCA values
BE2 <- BE %>%
  select(-any_of(WS_1A), -COMPLEXITY) %>%
  left_join(lca_1A_ng2_6[[1]]$pprob) %>%
  rename(
    u2g_class = class
  ) %>%
  mutate(
    u2g_prob = if_else(u2g_class == 1, prob1, prob2)
  ) %>%
  select(-starts_with("prob")) %>%
  left_join(lca_1A_ng2_6[[2]]$pprob) %>%
  rename(
    u3g_class = class
  ) %>%
  mutate(
    u3g_prob = if_else(u3g_class == 1, prob1,
                       if_else(u3g_class == 2, prob2, prob3))
  ) %>%
  select(-starts_with("prob"))

status_by_u2g <- BE2 %>%
  select(data_id, status, u2g_class) %>%
  distinct() %>%
  group_by(status) %>%
  mutate(
    total_status = n()
  ) %>%
  group_by(status, total_status, u2g_class) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    u2g_class = as.factor(u2g_class),
    p = n / total_status,
    label = paste0(n, " (", round(p * 100), "%)")
  )

status_by_u3g <- BE2 %>%
  select(data_id, status, u3g_class) %>%
  distinct() %>%
  group_by(status) %>%
  mutate(
    total_status = n()
  ) %>%
  group_by(status, total_status, u3g_class) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    u3g_class = as.factor(u3g_class),
    p = n / total_status,
    label = paste0(n, " (", round(p * 100), "%)")
  )

u2g_ass <- calc_ass(BE2, column = "u2g_class", dx_class = 2)
u3g_ass <- calc_ass(BE2, column = "u3g_class", dx_class = 3)

u2g_tile <- ggplot(status_by_u2g, aes(x = u2g_class, y = status, fill = p)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  scale_fill_viridis(limits = 0:1) +
  scale_y_discrete(labels = c("BCP", "E Dx+", "E Dx0", "E Dx-")) +
  theme_bw() +
  labs(x = "Class", y = "Status", caption = ass2str(u2g_ass))

u3g_tile <- ggplot(status_by_u3g, aes(x = u3g_class, y = status, fill = p)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  scale_fill_viridis(limits = 0:1) +
  scale_y_discrete(labels = c("BCP", "E Dx+", "E Dx0", "E Dx-")) +
  theme_bw() +
  labs(x = "Class", y = "Status", caption = ass2str(u3g_ass))

dir.create("plots/lca", showWarnings = FALSE, recursive = TRUE)

png("plots/lca/1_unsupervised_tileplots.png", width = 9, height = 4,
    units = "in", res = 300)

u2g_tile + u3g_tile

dev.off()

# Supervised 2-group analysis ####

lca_IA_ng1s <- lcmm(inventory_total ~ ageC,
                    data     = BE,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    intnodes = max_inv / exp(1))

lca_IA_ng2s <- lcmm(inventory_total ~ ageC,
                      data     = BE,
                      subject  = "data_id_num",
                      ng       = 2,
                      mixture  = ~ ageC,
                      link     = "3-manual-splines",
                      B        = lca_IA_ng1s,
                      prior    = "dx_prior",
                      intnodes = max_inv / exp(1))

save_data(lca_IA_ng2s, "results/BplusE_dxsupervised_LCA-2.rds")

BE3 <- BE2 %>%
  left_join(lca_IA_ng2s$pprob) %>%
  rename(
    s2g_class = class,
    s2g_prob1 = prob1,
    s2g_prob2 = prob2
  )

save_data(BE3, "LCA/raw_LCA_results.rds")

status_by_s2g <- BE3 %>%
  select(data_id, status, s2g_class) %>%
  distinct() %>%
  group_by(status) %>%
  mutate(
    total_status = n()
  ) %>%
  group_by(status, total_status, s2g_class) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    s2g_class = as.factor(s2g_class),
    p = n / total_status,
    label = paste0(n, " (", round(p * 100), "%)")
  )

png("plots/lca/2_supervised_tileplot.png", width = 5, height = 4,
    units = "in", res = 300)

ggplot(status_by_s2g, aes(x = s2g_class, y = status, fill = p)) +
  geom_tile() +
  geom_text(aes(label = label)) +
  scale_fill_viridis(limits = 0:1) +
  scale_y_discrete(labels = c("BCP", "E Dx+", "E Dx0", "E Dx-")) +
  theme_bw() +
  labs(x = "Class", y = "Status", caption = ass2str(u3g_ass))

dev.off()

top_16p_BCP <- quantile(BE3$s2g_prob2[BE3$status == "BCP"],
                        probs = 1 - dx_rate)


s2g_assn <- BE3 %>%
  filter(
    status %in% c("BCP", "no_follow_up")
  ) %>%
  select(status, data_id, s2g_prob2) %>%
  distinct()

sum(s2g_assn$s2g_prob2[s2g_assn$status == "BCP"] >= top_16p_BCP)

png("plots/lca/2b_bcp_supervised_prob.png", width = 4, height = 2,
    units = "in", res = 300)

ggplot(s2g_assn, aes(x = s2g_prob2, fill = status)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = top_16p_BCP) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Dx group assn. prob.")

dev.off()


