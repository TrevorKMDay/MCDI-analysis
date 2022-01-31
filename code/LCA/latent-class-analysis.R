path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(lcmm)
library(patchwork)

source("../mcdi-setup.R")

###############################################################################

# WS 1A inventory category labels
WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")

lex_cols <- WS_1A[1:15]
syn_cols <- c(WS_1A[16:22], WS_II)

###############################################################################
# Read in data

# In the following steps, use the vectors defined above to create accurate
# counts based on missing data (any_of). In this case, the syntax total is only
# COMPLEXITY due to missing data

# Read in cleaned EIRLI data with imputations for missing categories
eirli <- read_data("EIRLI/EIRLI_clean.rds") %>%
  mutate(

    inventory_total = select(., any_of(WS_1A)) %>%
                        rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
                        rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
                        rowSums(),

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
  select(proj, data_id, age, exact_age, sex, status, ends_with("_ed"),
         any_of(WS_1A), COMPLEXITY, ends_with("_total"))

# EIRLI demographics
# eirli_d <- read_data("EIRLI/eirli_clean_impute.rds")$n %>%
#   select(data_id, gender, father_ed, mother_ed) %>%
#   rename(
#     sex = gender
#   ) %>%
#   distinct() %>%
#   mutate(
#     proj = "EIRLI"
#   )

bcp_ws <- read_data("BCP/BCP_WS_scored-200609.rds")$n
bcp_wg <- read_data("BCP/BCP_WG_asWS-200609.rds")

bcp_d <- read_data("BCP/BCP-demographics-200609.csv") %>%
  select(CandID, sex, educ_momed) %>%
  rename(
    mother_ed = educ_momed,
    data_id = CandID
  ) %>%
  mutate(
    data_id = as.character(data_id),
    proj = "BCP"
  ) %>%
  filter(
    # BCP demographics include all subjects, drop those extra ones
    data_id %in% unique(c(bcp_ws$data_id, bcp_wg$data_id))
  )

bcp <- bind_rows(bcp_ws, bcp_wg) %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  # Join so demo is on the left
  left_join(bcp_d, .) %>%
  select(-outside, -places, -time_words, -starts_with("WORD_"), -LEXICAL,
         -SYNTAX, -TOTAL) %>%
  mutate(
    inventory_total = select(., any_of(WS_1A)) %>%
                        rowSums(),
    lex_total = select(., any_of(lex_cols)) %>%
                        rowSums(),
    syn_total = select(., any_of(syn_cols)) %>%
                        rowSums(),
    status = "BCP"
  )

###############################################################################

BplusE <- bind_rows(bcp, eirli) %>%
  select(proj, status, data_id, age, exact_age, sex, mother_ed, father_ed,
         any_of(WS_1A), COMPLEXITY, ends_with("total")) %>%
  arrange(proj, status, data_id, exact_age) %>%
  group_by(data_id) %>%
  mutate(

    # Numeric identifier for data_id, leave original as char
    data_id_num = as.numeric(cur_group_id()),

    # If exact age is missing (BCP), replace it with the age column (BCP has
    # exact age, EIRLI has target age as "age" and exact age. No reason not
    # to use exact_age going forward)
    exact_age = if_else(is.na(exact_age), age, exact_age),

    # Center age
    ageC = exact_age - 11,

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
  ) %>%
  select(proj, status, data_id, data_id_num, age, exact_age, ageC, sex,
         ends_with("_ed"), ends_with("_total"), everything())

# Check for missing vals anywhere but the ed columns
# BplusE_NAs <- BplusE %>%
#   filter(
#     rowSums(is.na(select(., -ends_with("_ed")))) > 0,
#   )
# rm(BplusE_NAs)

# Clean up workspace
rm(bcp_d, bcp_ws, bcp_wg)

###############################################################################

set.seed(55455)

# LCA on part IA outcome only
lca_IA_ng1 <- lcmm(inventory_total ~ ageC,
                   data     = BplusE,
                   subject  = "data_id_num",
                   ng       = 1,
                   link     = "3-manual-splines",
                   intnodes = 680 / exp(1))

grid_IA_file <- .data("LCA/gridsearch_IA.rds")

if (!file.exists(grid_IA_file)) {

  # Perform a grid search at increasing # groups
  grid_IA <- lapply(2:6,
                    function(num_groups)
                        gridsearch(
                          lcmm(inventory_total ~ ageC,
                                data     = BplusE,
                                subject  = "data_id_num",
                                ng       = num_groups,
                                mixture  = ~ageC,
                                link     = "3-manual-splines",
                                intnodes = 680 / exp(1)),
                          # Control options for gridsearch()
                          rep = 1000, maxiter = 100, minit = lca_IA_ng1)
                    )


  saveRDS(grid_IA, grid_IA_file)

} else {

  grid_IA <- readRDS(grid_IA_file)

}

# Perform model selection
post <- lapply(grid_IA, function(x) x$pprob[, -c(1, 2)])
lcga_entropy <- sapply(post, function(x) sum(-1 * x * log(x)))

# Fit indices
#computing fit indices for class enumeration
fit <- tibble(
    K       = rep(1:6),
    n       = rep(lca_IA_ng1$ns, 6),
    p       = c(3, 6, 9, 12, 15, 18),
    LL      = c(lca_IA_ng1$loglik, sapply(grid_IA, function(x) x$loglik)),
    entropy = c(0, lcga_entropy)
  ) %>%
  mutate(
    AIC = -2*LL + 2*p,
    BIC = -2*LL + p*log(n),
    CAIC = -2*LL + p*(log(n) + 1),
    ssBIC = -2*LL + p*log((n + 2)/24),
    CLC = -2*LL + 2*entropy,
    ICL.BIC = -2*LL + p*log(n) + 2*entropy,
    NEC = entropy/(LL - LL[1]),
  )

fit_long <- fit %>%
  pivot_longer(cols = ends_with("C"))

###############################################################################

fit_plot1 <- ggplot(filter(fit_long, name != "NEC"), 
                    aes(x = K, y = value, color = name)) +
  geom_line() +
  theme_bw()

fit_plotNEC  <- ggplot(filter(fit_long, name == "NEC"), 
                       aes(x = K, y = value, color = name)) +
  geom_line() +
  theme_bw()

fit_plot1 + fit_plotNEC

##############################################################################

# Unsupervised two-group LCA
unsup_2g <- grid_IA[[1]]

BplusE_u2g <- BplusE %>%
  left_join(unsup_2g$pprob) %>%
  mutate(
    prob = if_else(class == 1, prob1, prob2)
  )

u2g_table <- table(BplusE_u2g$status, BplusE_u2g$class)
round((u2g_table / rowSums(u2g_table)) * 100)

BplusE_u2g %>%
  group_by(proj, status, class) %>%
  summarize(
    mean_pprob = mean(prob)
  )

ggplot(BplusE_u2g, aes(x = exact_age, y = inventory_total,
                       color = prob)) +
  geom_point() + 
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(color = "black") +
  scale_color_viridis(limits = c(0.5, 1)) + 
  facet_grid(cols = vars(status), rows = vars(class)) +
  theme_bw()

BplusE_u2g_ass <- BplusE_u2g %>%
  filter(
    status %in% c("lg_dx", "no_lg_dx")
  ) %>%
  mutate(
    TP = class == 2 & status == "lg_dx",
    TN = class == 1 & status == "no_lg_dx",
    FP = class == 2 & status == "no_lg_dx",
    FN = class == 1 & status == "lg_dx"
  ) %>%
  ungroup() %>%
  select(matches("[TF][PN]")) %>%
  apply(2, sum)

BplusE_u2g_accuracy <- (BplusE_u2g_ass["TP"] + BplusE_u2g_ass["TN"]) / sum(BplusE_u2g_ass)
BplusE_u2g_sensitiv <- BplusE_u2g_ass["TP"] / (BplusE_u2g_ass["TP"] + BplusE_u2g_ass["FN"])
BplusE_u2g_specific <- BplusE_u2g_ass["TN"] / (BplusE_u2g_ass["TN"] + BplusE_u2g_ass["FP"])
