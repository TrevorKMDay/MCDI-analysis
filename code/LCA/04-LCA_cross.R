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

WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")

# Load data ####

BE <- read_data("LCA/BplusE.rds") %>%
  mutate(
    empty_prior = 0
  ) %>%
  ungroup()

max_inv <- c("lex" = 513, syn = 139)

# Run LCA models ####

## Multi-lcmm ####

lca_lexsyn_ng1 <- multlcmm(lex_total + syn_total ~ ageC,
                           data     = as.data.frame(BE),
                           subject  = "data_id_num",
                           ng       = 1,
                           random   = ~ageC,
                           link     = "3-manual-splines",
                           prior    = "empty_prior",
                           intnodes = max_inv / exp(1))

lca_lexsyn_ng2_6 <- lapply(2:6, function(ng)
                                  multlcmm(lex_total + syn_total ~ ageC,
                                           data     = as.data.frame(BE),
                                           subject  = "data_id_num",
                                           ng       = ng,
                                           random   = ~ageC,
                                           mixture  = ~ageC,
                                           link     = "3-manual-splines",
                                           B        = lca_lexsyn_ng1,
                                           prior    = "empty_prior",
                                           intnodes = max_inv / exp(1)))


fitstats_lexsyn <- get_fit_stats(lca_lexsyn_ng1, lca_lexsyn_ng2_6)
plot_fit_stats(fitstats_lexsyn)

lca_lexsyn_ng1s <- multlcmm(lex_total + syn_total ~ ageC,
                           data     = as.data.frame(BE),
                           subject  = "data_id_num",
                           ng       = 1,
                           random   = ~ageC,
                           link     = "3-manual-splines",
                           prior    = "dx_prior",
                           intnodes = max_inv / exp(1))

lca_lexsyn_ng2_6s <- lapply(2:6, function(ng)
  multlcmm(lex_total + syn_total ~ ageC,
           data     = as.data.frame(BE),
           subject  = "data_id_num",
           ng       = ng,
           random   = ~ageC,
           mixture  = ~ageC,
           link     = "3-manual-splines",
           B        = lca_lexsyn_ng1,
           prior    = "dx_prior",
           intnodes = max_inv / exp(1)))

fitstats_lexsyn_sup <- get_fit_stats(lca_lexsyn_ng1s, lca_lexsyn_ng2_6s)
plot_fit_stats(fitstats_lexsyn_sup)

## Cross ####

cor(BE$lex_total, BE$syn_total)

lca_lex_ng1 <- lcmm(lex_total ~ ageC,
                    data     = BE,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    B        = rep(1, 6),
                    prior    = "empty_prior",
                    intnodes = max_inv["lex"] / exp(1))

lca_syn_ng1 <- lcmm(syn_total ~ ageC,
                    data     = BE,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    B        = rep(1, 6),
                    prior    = "empty_prior",
                    intnodes = max_inv["syn"] / exp(1))

lca_lex_ng2_6 <- lapply(2:6, function(ng)
                              lcmm(lex_total ~ ageC,
                                   data     = BE,
                                   subject  = "data_id_num",
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   B        = lca_lex_ng1,
                                   prior    = "empty_prior",
                                   link     = "3-manual-splines",
                                   intnodes = max_inv["lex"] / exp(1)))

lca_syn_ng2_6 <- lapply(2:6, function(ng)
                              lcmm(syn_total ~ ageC,
                                   data     = BE,
                                   subject  = "data_id_num",
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   B        = lca_syn_ng1,
                                   prior    = "empty_prior",
                                   link     = "3-manual-splines",
                                   intnodes = max_inv["syn"] / exp(1)))

fitstats_lex <- get_fit_stats(lca_lex_ng1, lca_lex_ng2_6)

png("plots/lca/3a_lex_fitstats.png", width = 8, height = 5, units = "in",
    res = 300)

plot_fit_stats(fitstats_lex)

dev.off()

fitstats_syn <- get_fit_stats(lca_syn_ng1, lca_syn_ng2_6)

png("plots/lca/3b_syn_fitstats.png", width = 8, height = 5, units = "in",
    res = 300)

plot_fit_stats(fitstats_syn)

dev.off()

# Next analysis ####

dx <- read_data("EIRLI/EIRLI_dx.csv") %>%
  select(sid, `Count of lang dx's`) %>%
  rename(
    data_id = sid,
    dx_count = `Count of lang dx's`
  )

BEx <- BE %>%
  select(-any_of(WS_1A), -any_of(WS_II)) %>%
  left_join(
    select(lca_lex_ng2_6[[2]]$pprob, -starts_with("prob"))
  ) %>%
  left_join(
    select(lca_syn_ng2_6[[2]]$pprob, -starts_with("prob")),
    by = "data_id_num"
  ) %>%
  rename(
    lex_class = class.x,
    syn_class = class.y
  ) %>%
  mutate(
    lex_syn = paste(lex_class, syn_class)
  )

png("plots/lca/4_cross.png", width = 9, height = 5, units = "in", res = 300)

ggplot(BEx, aes(x = lex_total, y = syn_total)) +
  geom_line(aes(group = data_id), alpha = 0.25) +
  geom_smooth(aes(color = as.factor(syn_class))) +
  geom_smooth(color = "red") +
  facet_grid(cols = vars(lex_class)) +
  theme_bw() +
  theme(legend.position = "bottom")

dev.off()

BEx_count <- BEx %>%
  group_by(status, lex_syn) %>%
  summarize(
    n = n()
  ) %>%
  ungroup() %>%
  group_by(status) %>%
  mutate(
    total = sum(n),
    p = n / total
  )

writeClipboard(as.matrix(BEx_count))
