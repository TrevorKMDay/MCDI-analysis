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

BEd <- read_data("LCA/BplusE_demographics.rds")

BEd %>%
  filter(
    proj == "EIRLI"
  ) %>%
  select(ends_with("college")) %>%
  group_by(mother_college, father_college) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    total = sum(n),
    p = round(100 * n / nrow(BEd))
  )

BE <- read_data("LCA/BplusE.rds") %>%
  mutate(
    empty_prior = 0
  ) %>%
  ungroup() %>%
  left_join(
    select(BEd, proj, data_id, sex, mother_college)
  ) %>%
  mutate(
    male = sex == "Male",
    prior = case_when(
      status == "lg_dx" ~ 1,
      status == "no_lg_dx" ~ 2,
      TRUE ~ 0
    ),
    BCP = proj == "BCP"
  ) %>%
  select(-sex)

# TODO: Missing demographics for n=98 BCP participants
BE_nodx <- BE %>%
  filter(
    status %in% c("BCP", "no_lg_dx", "no_follow_up"),
    exact_age > 15,
    exact_age < 38
  ) %>%
  mutate(
    BCP = proj == "BCP"
  )

BE_nodx %>%
  group_by(proj) %>%
  summarize(
    age_min = min(exact_age),
    age_max = max(exact_age)
  )

# Maximum is NOT 680 since EIRLI is included
max_inv <- max(BE$eirli_total)

# WS 1A inventory category labels
WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")


# lm

lm(eirli_total ~ exact_age + I(exact_age^2) + I(exact_age^3) + proj,
   data = BE_nodx) %>%
  summary()

ggplot(BE_nodx, aes(x = exact_age, y = eirli_total, color = proj)) +
  geom_point() +
  geom_smooth()

# LCA: Part IA ####

set.seed(55455)

# LCA on part IA outcome only

# For all LCAs, run the one-group LCA first as to establish baseline, it is
# used in the multi-group LCAs

# TO DO: Grid searches

max_inv_over_e <- max_inv / exp(1)

lca_IA_ng1_ageonly <- lcmm(eirli_total ~ exact_age,
                           data     = BE_nodx,
                           subject  = "data_id_num",
                           ng       = 1,
                           link     = "3-manual-splines",
                           intnodes = max_inv_over_e,
                           prior    = "empty_prior")

lca_IA_ng1_agemale <- lcmm(eirli_total ~ exact_age + male,
                           data     = BE_nodx,
                           subject  = "data_id_num",
                           ng       = 1,
                           link     = "3-manual-splines",
                           intnodes = max_inv_over_e,
                           prior    = "empty_prior")

lca_IA_ng1_agemaleBCP <- lcmm(eirli_total ~ exact_age + male + BCP,
                           data     = BE_nodx,
                           subject  = "data_id_num",
                           ng       = 1,
                           link     = "3-manual-splines",
                           intnodes = max_inv_over_e,
                           prior    = "empty_prior")

lca_IA_ng1_agemalemed <- lcmm(eirli_total ~ exact_age + male + mother_college,
                               data     = BE_nodx,
                               subject  = "data_id_num",
                               ng       = 1,
                               link     = "3-manual-splines",
                               intnodes = max_inv_over_e,
                               prior    = "empty_prior")

ng1_models <- list(lca_IA_ng1_ageonly, lca_IA_ng1_agemale, lca_IA_ng1_agemaleBCP,
                   lca_IA_ng1_agemalemed)

ng1_fitstats <- get_fit_stats(list_of_models = ng1_models)
plot_fit_stats(ng1_fitstats)

lca_1A_ng2_6_age <- lapply(2:6, function(x)
                              lcmm(eirli_total ~ exact_age,
                                   data     = BE_nodx,
                                   subject  = "data_id_num",
                                   ng       = x,
                                   mixture  = ~exact_age,
                                   link     = "3-manual-splines",
                                   intnodes = max_inv_over_e,
                                   B        = lca_IA_ng1_ageonly,
                                   prior    = "empty_prior",
                                   nproc    = 6)
                            )

lca_1A_ng2_6_agemale <- lapply(2:6, function(x)
                        lcmm(eirli_total ~ exact_age + male,
                             data     = BE_nodx,
                             subject  = "data_id_num",
                             ng       = x,
                             mixture  = ~exact_age + male,
                             link     = "3-manual-splines",
                             intnodes = max_inv_over_e,
                             B        = lca_IA_ng1_agemale,
                             prior    = "empty_prior",
                             nproc    = 6)
                       )

lca_1A_ng2_6_agemaleBCP <- lapply(2:6, function(x)
                                lcmm(eirli_total ~ exact_age + male + BCP,
                                     data     = BE_nodx,
                                     subject  = "data_id_num",
                                     ng       = x,
                                     mixture  = ~exact_age + male + BCP,
                                     link     = "3-manual-splines",
                                     intnodes = max_inv_over_e,
                                     B        = lca_IA_ng1_agemaleBCP,
                                     prior    = "empty_prior",
                                     nproc    = 6)
                              )

# Extract the seven fit stats (AIC, BIC, CAIC, CLC, ICL.BIC, ss.BIC, NEC)
# from the one-group model and the list of higher-order models for plotting
fitstats_IA_ageonly <- get_fit_stats(lca_IA_ng1_ageonly, lca_1A_ng2_6_age)
fitstats_IA_agemale <- get_fit_stats(lca_IA_ng1_agemale, lca_1A_ng2_6_agemale)
fitstats_IA_agemaleBCP <- get_fit_stats(lca_IA_ng1_agemaleBCP,
                                        lca_1A_ng2_6_agemaleBCP)

plot_fit_stats(fitstats_IA_ageonly, title = "Age only")
plot_fit_stats(fitstats_IA_agemale, title = "Age + male")
plot_fit_stats(fitstats_IA_agemaleBCP, title = "Age + male + BCP")

lca_ng_pprob <- lca_1A_ng2_6[[3]]$pprob

BE_ng3 <- left_join(BE_nodx, lca_ng_pprob, by = "data_id_num")

ggplot(BE_ng3, aes(x = exact_age, y = eirli_total, color = proj)) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth() +
  theme_minimal()

ggplot(BE_ng3, aes(x = exact_age, y = eirli_total, color = as.factor(class))) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth(aes(linetype = proj)) +
  theme_minimal()

table(BE_ng3$proj, BE_ng3$class) / rep(table(BE_ng3$proj), 4)

# DX groups ====

lca_dx <- lcmm(eirli_total ~ exact_age + male + BCP,
               data     = BE,
               subject  = "data_id_num",
               ng       = 2,
               mixture  = ~exact_age + male + BCP,
               B        = rep(1, 13),
               link     = "3-manual-splines",
               intnodes = max_inv_over_e,
               prior    = "prior")

# Muli-LCMM

lex_syn_nodes <- c(max(BE_nodx$lex_total), max(BE_nodx$syn_total)) / exp(1)

lca_mult <- multlcmm(lex_total + syn_total ~ 1 + exact_age,
                     data     = as.data.frame(BE_nodx),
                     subject  = "data_id_num",
                     random   = ~1 + exact_age,
                     ng       = 1,
                     link     = "3-manual-splines",
                     intnodes = lex_syn_nodes,
                     prior    = "empty_prior")

lca_mult_26 <- lapply(2:6, function(ng)
  multlcmm(lex_total + syn_total ~ 1 + exact_age,
           data     = as.data.frame(BE_nodx),
           subject  = "data_id_num",
           mixture  = ~1 + exact_age,
           random   = ~1 + exact_age,
           ng       = ng,
           link     = "3-manual-splines",
           intnodes = lex_syn_nodes,
           B        = lca_mult,
           prior    = "empty_prior"))

multlcmm_fs <- get_fit_stats(lca_mult, lca_mult_26)
plot_fit_stats(multlcmm_fs)
