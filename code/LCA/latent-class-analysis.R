

##### Poster plot

BplusE_nodx <- BplusE %>%
  filter(
    status %in% c("BCP", "no_lg_dx")
  )

png("poster_Fig1_BCPvEIRLI.png", width = 12, height = 6, units = "in",
    res = 300)

ggplot(BplusE_nodx, aes(x = exact_age, y = inventory_total, color = status)) +
  geom_smooth(size = 2) +
  scale_x_continuous(limits = c(10, 40)) +
  theme_minimal() +
  labs(x = "Age (mo.)", y = "Inventory size", color = "Study") +
  theme(legend.position = "bottom", text = element_text(size = 24))

dev.off()

status_tbl <- BplusE %>%
  select(data_id, status) %>%
  distinct() %>%
  pull(status) %>%
  table()

round(100 * status_tbl / sum(status_tbl))

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

get_fit_stats <- function(g1_model, list_of_models) {

  n_models <- 1 + length(list_of_models)

  # Perform model selection
  post <- lapply(list_of_models, function(x) x$pprob[, -c(1, 2)])
  lcga_entropy <- sapply(post, function(x) sum(-1 * x * log(x)))

  g1_param <- length(g1_model$best)
  list_param <- sapply(list_of_models, function(x) length(x$best))

  # Fit indices
  #computing fit indices for class enumeration
  fit <- tibble(

      K       = 1:n_models,

      # Number of participants should be constant
      n       = rep(g1_model$ns, n_models),

      # Number of parameters in the model
      p       = c(g1_param, list_param),

      # Log-likelihood
      LL      = c(g1_model$loglik,
                  sapply(list_of_models, function(x) x$loglik)),

      # Measures classification uncertainty: 0 when all pprobs are 0/1, gets
      # bigger as uncertainty increases
      entropy = c(0, lcga_entropy)
    ) %>%
    mutate(

      # AIC has no penalty and favors more classes
      AIC     = -2*LL + 2*p,

      # Penalizes for model complexity
      # BIC favores fewer classes
      BIC     = -2*LL + p*log(n),

      # Corrected AIC for small sample sizes
      # CAIC favores fewer classes as well
      CAIC    = -2*LL + p*(log(n) + 1),

      # Sample-size corrected BIC - for small sample sizes
      ssBIC   = -2*LL + p*log((n + 2) / 24),

      # Classification-likelihood criterion penalizes poor entropy
      CLC     = -2*LL + 2*entropy,

      # Integrated completed likelihood criterion penalizes for poor entropy
      # and model complexity
      ICL.BIC = -2*LL + p*log(n) + 2*entropy,

      # Smaller is better - 0 is perfect clustering, undefined for 1 group.
      # Suggest that if no values are <1, there's no clusters
      NEC     = entropy / (LL - LL[1]),

    )

  return(fit)

}

plot_fit_stats <- function(fit_stats, plot_NEC = TRUE, text_size = 11,
                           title = NULL, bg = "#000000") {

  maxK <- max(fit_stats$K)

  fit_long <- pivot_longer(fit_stats,
                           cols = c(AIC, BIC, CAIC, ssBIC, CLC, ICL.BIC, NEC))

  fit_plot1 <- ggplot(filter(fit_long, name != "NEC"),
                      aes(x = K, y = value, color = name)) +
    geom_line() +
    scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
    theme_bw() +
    theme(text = element_text(size = text_size),
          plot.background = element_rect(fill = bg, color = NA)) +
    labs(title = title)

  if (plot_NEC) {

    fit_plotNEC  <- ggplot(filter(fit_long, name == "NEC"),
                           aes(x = K, y = value, color = name)) +
      geom_line() +
      scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
      theme_bw() +
      theme(text = element_text(size = text_size)) +
      labs(title = title)

    final_plot <- fit_plot1 + fit_plotNEC

  } else {

    final_plot <- fit_plot1

  }

  return(final_plot)

}

fitstats_IA <- get_fit_stats(lca_IA_ng1, grid_IA)

dir.create("plots")

png("plots/fitstats_IA.png", width = 6.91, height = 3.28, units = "in",
    res = 150)

plot_fit_stats(fitstats_IA)

dev.off()

##############################################################################

# Unsupervised two-group LCA
unsup_2g <- grid_IA[[1]]
unsup_3g <- grid_IA[[2]]

BplusE_u2g <- BplusE %>%
  left_join(unsup_2g$pprob) %>%
  mutate(
    prob = if_else(class == 1, prob1, prob2)
  )

BplusE_u3g <- BplusE %>%
  left_join(unsup_3g$pprob) %>%
  mutate(
    prob = if_else(class == 1, prob1, if_else(class == 2, prob2, prob3))
  ) %>%
  select(-any_of(WS_1A), -any_of(WS_II))


u2g_table <- BplusE_u2g %>%
  select(data_id, status, class) %>%
  distinct() %>%
  group_by(status, class) %>%
  summarize(
    n = n()
  ) %>%
  group_by(status) %>%
  mutate(
    sum = sum(n),
    pct = round(n / sum * 100)
  )

png("poster_Fig2_u2g-dist.png", width = 5, height = 3, units = "in", res = 300)

ggplot(u2g_table, aes(x = status, y = class, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = paste0(pct, "%")), size = 10, color = "white") +
  scale_x_discrete(labels = c("BCP", "E DX+", "E DX0", "E DX-")) +
  scale_y_continuous(breaks = c(1, 2)) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        plot.background = element_rect(fill = "#ffde7a", color = NA)) +
  guides(fill = "none") +
  labs(x = NULL, y = "LCA class")

dev.off()

u3g_table <- BplusE_u3g %>%
  select(data_id, status, class) %>%
  distinct() %>%
  group_by(status, class) %>%
  summarize(
    n = n()
  )

BplusE_u2g %>%
  group_by(proj, status, class) %>%
  summarize(
    mean_pprob = mean(prob)
  )

BplusE_u3g %>%
  group_by(proj, status, class) %>%
  summarize(
    mean_pprob = mean(prob)
  )

ggplot(BplusE_u2g, aes(x = exact_age, y = inventory_total, color = prob)) +
  geom_point() +
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(color = "black") +
  scale_color_viridis(limits = c(0.5, 1)) +
  facet_grid(cols = vars(status), rows = vars(class)) +
  theme_bw()

ggplot(BplusE_u2g, aes(x = exact_age, y = inventory_total,
                       color = as.factor(class))) +
  geom_smooth() +
  scale_color_viridis(discrete = TRUE) +
  facet_grid(cols = vars(status)) +
  theme_bw()

ggplot(BplusE_u3g, aes(x = exact_age, y = inventory_total, color = prob)) +
  geom_point() +
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(color = "black") +
  scale_color_viridis(limits = c(0.5, 1)) +
  facet_grid(cols = vars(status), rows = vars(class)) +
  theme_bw()

ggplot(BplusE_u3g, aes(x = exact_age, y = inventory_total,
                       color = as.factor(class))) +
  geom_smooth() +
  scale_color_viridis(discrete = TRUE) +
  facet_grid(cols = vars(status)) +
  theme_bw()

calc_ass <- function(df, dx_class) {

  result <- df %>%
    filter(
      status %in% c("lg_dx", "no_lg_dx")
    ) %>%
    mutate(
      TP =  (class %in% dx_class) & (status == "lg_dx"),
      TN = !(class %in% dx_class) & (status == "no_lg_dx"),
      FP =  (class %in% dx_class) & (status == "no_lg_dx"),
      FN = !(class %in% dx_class) & (status == "lg_dx")
    ) %>%
    ungroup() %>%
    select(matches("[TF][PN]")) %>%
    apply(2, sum)

  acc  <- unname((result["TP"] + result["TN"]) / sum(result))
  sens <- unname(result["TP"] / (result["TP"] + result["FN"]))
  spec <- unname(result["TN"] / (result["TN"] + result["FP"]))

  return(c("accuracy" = acc, "sensitivity" = sens, "specificity" = spec))

}

calc_ass(BplusE_u2g, 2)
calc_ass(BplusE_u3g, c(2, 3))


###############################################################################

# Supervised 2-group analysis

lca_IA_ng2s <- lcmm(inventory_total ~ ageC,
                      data     = BplusE,
                      subject  = "data_id_num",
                      ng       = 2,
                      mixture  = ~ ageC,
                      link     = "3-manual-splines",
                      prior    = "dx_prior",
                      intnodes = 680 / exp(1))

BplusE_s2g <- BplusE %>%
  left_join(lca_IA_ng2s$pprob) %>%
  group_by(status) %>%
  mutate(
    prob = if_else(class == 1, prob1, prob2),
    prob2_top12pctile = prob2 > quantile(prob2, 1 - 0.12)
  )

BplusE_s2g_table <- BplusE_s2g %>%
  select(data_id, status, class) %>%
  distinct() %>%
  group_by(status) %>%
  summarize(
    n = n(),
    ng1 = sum(class == 1),
    ng2 = sum(class == 2)
  )

##############################################################################

lca_lex_1g <- lcmm(lex_total ~ ageC,
                   data     = BplusE,
                   subject  = "data_id_num",
                   ng       = 1,
                   link     = "3-manual-splines",
                   intnodes = 513 / exp(1))

lca_syn_1g <- lcmm(syn_total ~ ageC,
                    data     = BplusE,
                    subject  = "data_id_num",
                    ng       = 1,
                    link     = "3-manual-splines",
                    intnodes = 139 / exp(1))

lca_lex_26gs <- lapply(2:6, function(ng)
                              lcmm(lex_total ~ ageC,
                                   data     = BplusE,
                                   subject  = "data_id_num",
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   link     = "3-manual-splines",
                                   intnodes = 513 / exp(1)))

lca_syn_26gs <- lapply(2:6, function(ng)
                              lcmm(syn_total ~ ageC,
                                   data     = BplusE,
                                   subject  = "data_id_num",
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   link     = "3-manual-splines",
                                   intnodes = 139 / exp(1)))

fitstats_lex <- get_fit_stats(lca_lex_1g, lca_lex_26gs)
plot_fit_stats(fitstats_lex)

png("fitstats_lex.png", width = 6, height = 6, units = "in", res = 300)
plot_fit_stats(fitstats_lex, plot_NEC = FALSE, text_size = 24,
               title = "Lexical", bg = "#ffde7a")
dev.off()

fitstats_syn <- get_fit_stats(lca_syn_1g, lca_syn_26gs)
plot_fit_stats(fitstats_syn)

png("fitstats_syn.png", width = 6, height = 6, units = "in", res = 300)
plot_fit_stats(fitstats_syn, plot_NEC = FALSE, text_size = 24,
               title = "Syntax", bg = "#ffde7a")
dev.off()

dx <- read_data("EIRLI/EIRLI_dx.csv") %>%
  select(sid, `Count of lang dx's`) %>%
  rename(
    data_id = sid,
    dx_count = `Count of lang dx's`
  )

BplusE3 <- BplusE %>%
  select(-any_of(WS_1A), -any_of(WS_II)) %>%
  left_join(select(lca_lex_26gs[[1]]$pprob, data_id_num, class)) %>%
  left_join(select(lca_lex_26gs[[2]]$pprob, data_id_num, class),
            by = c("data_id_num"), suffix = c(".l2g", ".l3g")) %>%
  left_join(select(lca_syn_26gs[[2]]$pprob, data_id_num, class)) %>%
  rename(
    class.s3g = class
  ) %>%
  mutate(
    l2_s3 = paste(class.l2g, class.s3g),
    l3_s3 = paste(class.l3g, class.s3g)
  ) %>%
  left_join(dx)

ggplot(BplusE3, aes(x = lex_total, y = syn_total, color = status)) +
  geom_point(alpha = 0.25, size = .2) +
  geom_line(aes(group = data_id), alpha = 0.25) +
  geom_smooth(color = "black") +
  facet_grid(rows = vars(class.s3g), cols = vars(class.l2g)) +
  theme_bw() +
  labs(x = "Lexical Inventory", y = "Syntactic Inventory")

ggplot(BplusE3, aes(x = lex_total, y = syn_total)) +
  geom_smooth(aes(color = as.factor(class.s3g),
                  linetype = as.factor(class.l2g))) +
  theme_bw() +
  labs(x = "Lexical Inventory", y = "Syntactic Inventory")

BplusE3 <- BplusE3 %>%
  mutate(
    across(c(class.s3g, class.l3g),
           ~case_when(.x == 1 ~ "1_high", .x == 2 ~ "2_avg", .x == 3 ~ "3_low"))
  )

png("lca_3x3.png", width = 7, height = 4, units = "in", res = 300)

ggplot(BplusE3, aes(x = lex_total, y = syn_total, color = status)) +
  geom_point(alpha = 0.25, size = .2) +
  geom_line(aes(group = data_id), alpha = 0.25) +
  facet_grid(rows = vars(class.s3g), cols = vars(class.l3g)) +
  theme_bw() +
  labs(x = "Lexical Inventory", y = "Syntactic Inventory")

dev.off()

png("poster_Fig3_3x3.png", width = 8.3, height = 8.3, units = "in", res = 300)

ggplot(BplusE3, aes(x = lex_total, y = syn_total, color = status)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_line(aes(group = data_id), alpha = 0.25) +
  facet_grid(rows = vars(class.s3g), cols = vars(class.l3g)) +
  scale_x_continuous(breaks = c(0, 250, 500)) +
  theme_bw() +
  labs(x = "Lexical Inventory", y = "Syntactic Inventory") +
  theme(text = element_text(size = 24), legend.position = "bottom",
        plot.background = element_rect(fill = "#ffde7a", color = NA))

dev.off()

BplusE3 %>%
  select(data_id, status, class.s3g, class.l3g, dx_count) %>%
  distinct() %>%
  group_by(class.l3g, class.s3g) %>%
  summarize(
    n = n(),
  ) %>%
  mutate(
    pct = n / length(unique(BplusE$data_id))
  )

BplusE3 %>%
  select(data_id, status, class.s3g, class.l3g, dx_count) %>%
  distinct() %>%
  filter(
    status == "lg_dx"
  ) %>%
  group_by(class.l3g, class.s3g)

BpE3_lgdxonly <- BplusE3 %>%
  filter(
    status == "lg_dx"
  ) %>%
  mutate(
    across(starts_with("class."), as.factor)
  )

dx_aov <- aov(dx_count ~ class.s3g + class.l3g, data = BpE3_lgdxonly)
TukeyHSD(dx_aov)

# Percent of cell with status
BplusE3 %>%
  select(data_id, status, class.s3g, class.l3g) %>%
  distinct() %>%
  group_by(status, class.s3g, class.l3g) %>%
  summarize(
    n = n()
  ) %>%
  group_by(class.s3g, class.l3g) %>%
  mutate(
    total = sum(n),
    pct = round(n / total * 100)
  ) %>%
  arrange(class.s3g, class.l3g) %>%
  View()

# Percent of those with status appearing in cell
in_cell <- BplusE3 %>%
  select(data_id, status, class.s3g, class.l3g) %>%
  distinct() %>%
  group_by(status, class.s3g, class.l3g) %>%
  summarize(
    n = n()
  ) %>%
  left_join(
    as_tibble(status_tbl, n = "total"),
    by = c("status" = ".")
  ) %>%
  mutate(
    pct = round(100 * n / total)
  ) %>%
  arrange(class.l3g, class.s3g)

pct_status <- BplusE3 %>%
  filter(
    status %in% c("lg_dx", "no_lg_dx")
  ) %>%
  group_by(class.l3g, class.s3g, status) %>%
  summarize(
    n = n()
  ) %>%
  pivot_wider(names_from = status, values_from = n) %>%
  mutate(
    pct_dx = round(lg_dx / (lg_dx + no_lg_dx) * 100)
  )

png("poster_Fig4_dxpct.png", width = 5, height = 3, units = "in", res = 300)

ggplot(pct_status, aes(x = class.l3g, y = class.s3g, fill = pct_dx)) +
  geom_tile() +
  geom_text(aes(label = paste0(pct_dx, "%")), color = "white", size = 12) +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  theme(text = element_text(size = 20), legend.position = "none",
        plot.background = element_rect(fill = "#ffde7a", color = NA)) +
  labs(x = "Lexical Class", y = "Syntax Class")

dev.off()

##### Analyze those in category 3.3

# Delay status for all individuals
bcp_delay <- read_csv("delay3.csv") %>%
  mutate(
    data_id = as.character(data_id)
  )

# Information about those BCP participants in class 3.3
bcp_33 <- BplusE3 %>%
  filter(
    proj == "BCP",
    class.l3g == 3,
    class.s3g == 3
  ) %>%
  select(data_id, exact_age, inventory_total) %>%
  arrange(data_id, exact_age) %>%
  group_by(data_id) %>%
  nest() %>%
  left_join(bcp_delay)

# 0      = no delay
# >0, <1 = probably no delay
# >1     = delay at different levels, some guesses
table(bcp_33$delay)

# Load Mullen
mullen <- read_data("BCP/bcp-vl_mullen-220209.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label,
         matches("mullen.*language_percentile")) %>%
  separate(demographics.Visit_label, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    visit = as.numeric(str_remove(visit, "m")),
    across(starts_with("mullen"), as.numeric)
  ) %>%
  na.omit() %>%
  arrange(demographics.CandID, desc(visit)) %>%
  group_by(demographics.CandID) %>%
  nest() %>%
  rename(
    data_id = demographics.CandID
  ) %>%
  mutate(
    data_id = as.character(data_id),
    exp = map_dbl(data, ~.x$mullen.expressive_language_percentile[1]),
    rec = map_dbl(data, ~.x$mullen.receptive_language_percentile[1])
  )

# Load Vineland
vl <- readxl::read_xlsx(.data("BCP/20210210_Vineland_BCP.xlsx"),
                        na = "NULL") %>%
  select(CandID, Visit_label, ends_with("_SCORE")) %>%
  separate(Visit_label, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    CandID = as.character(CandID),
    visit = as.numeric(str_remove(visit, "m")),
  ) %>%
  rename(
    data_id    = CandID,

    # Standard scores
    social_std = SOC_STD_SCORE,
    motor_std  = MS_STD_SCORE,
    comm_std   = COM_STD_SCORE,
    daily_std  = DLS_STD_SCORE,
    comp_std   = ABC_STD_SCORE
  )

vl_summary <- vl %>%
  group_by(data_id) %>%
  summarize(
    comm_max = max(comm_std, na.rm = TRUE),
    comm_median = median(comm_std, na.rm = TRUE),
    comm_mean = mean(comm_std, na.rm = TRUE)
  )
#
# vl_summary %>%
#   select(starts_with("comm_")) %>%
#   cor()

ggplot(vl, aes(x = visit, y = comm_std)) +
  geom_line(aes(group = data_id)) +
  geom_smooth() +
  theme_bw()

# BCP values
BCP_mullen_vl <- BplusE3 %>%
  filter(
    proj == "BCP"
  ) %>%
  left_join(mullen) %>%
  left_join(vl_summary) %>%
  select(data_id, class.l3g, class.s3g, exp, rec, comm_max) %>%
  mutate(
    l3_s3 = paste(class.l3g, class.s3g)
  )

BCP_mullen_vl_long <- BCP_mullen_vl %>%
  ungroup() %>%
  rename(
    mullen_exp = exp,
    mullen_rec = rec,
    vineland_comm = comm_max
  ) %>%
  mutate(
    across(c("mullen_exp", "mullen_rec", "vineland_comm"), ~scale(.x)[, 1])
  ) %>%
  pivot_longer(c(mullen_exp, mullen_rec, vineland_comm))

png("SRCLD_poster_3x3_stats.png", width = 12, height = 6, units = "in",
    res = 300)

ggplot(BCP_mullen_vl_long, aes(x = name, y = value,
                               fill = interaction(class.l3g, class.s3g))) +
  geom_boxplot() +
  theme_bw() +
  scale_x_discrete(labels = c("Mullen expressive", "Mullen receptive",
                              "Vineland communicative")) +
  scale_fill_manual(labels = c("1.1: on-track",    "2.1: high syntax",
                                 "1.2: low syntax",  "2.2: on-track",
                                 "3.2: high syntax", "2.3: low syntax",
                                 "3.3: possible Dx"),
                      values = c("aquamarine3", "darkorange2",
                                 "darkorchid2", "aquamarine4",
                                 "darkorange3", "darkorchid3",
                                 "deeppink3") ) +
  labs(x = "Measure", y = "Z score", fill = "Class") +
  theme(text = element_text(size = 24), legend.position = "bottom")

dev.off()

BCP_mullen_vl2 <- BCP_mullen_vl %>%
  mutate(
    flagged = l3_s3 == "3 3"
  ) %>%
  group_by(flagged) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),

    m_mullen_exp = map_dbl(data, ~mean(.x$exp, na.rm = TRUE)),
    m_mullen_rec = map_dbl(data, ~mean(.x$rec, na.rm = TRUE)),
    m_vl_comm    = map_dbl(data, ~mean(.x$comm_max, na.rm = TRUE)),

    sd_mullen_exp = map_dbl(data, ~sd(.x$exp, na.rm = TRUE)),
    sd_mullen_rec = map_dbl(data, ~sd(.x$rec, na.rm = TRUE)),
    sd_vl_comm    = map_dbl(data, ~sd(.x$comm_max, na.rm = TRUE)),
  )

ggplot(BCP_mullen_vl, aes(x = l3_s3, y = rec)) +
  geom_boxplot() +
  theme_bw()

ggplot(BCP_mullen_vl, aes(x = l3_s3, y = exp)) +
  geom_boxplot() +
  theme_bw()

d <- function(m, sd) {

  diff <- abs(m[1] - m[2])
  psd <- sqrt((sd[1]^2 + sd[2]^2) / 2)

  return(diff / psd)

}

# Mullen expressive t-test
t.test(BCP_mullen_vl2$data[[1]]$exp, BCP_mullen_vl2$data[[2]]$exp)
d(BCP_mullen_vl2$m_mullen_exp, BCP_mullen_vl2$sd_mullen_exp)

# Mullen receptive t-test
t.test(BCP_mullen_vl2$data[[1]]$rec, BCP_mullen_vl2$data[[2]]$rec)
d(BCP_mullen_vl2$m_mullen_rec, BCP_mullen_vl2$sd_mullen_rec)

# Vineland communicative t-test
t.test(BCP_mullen_vl2$data[[1]]$comm_mean, BCP_mullen_vl2$data[[2]]$comm_mean)
d(BCP_mullen_vl2$m_vl_comm, BCP_mullen_vl2$sd_vl_comm)

vl2 <- vl %>%
  left_join(select(BplusE3, data_id, l3_s3))

ggplot(vl2, aes(x = visit, y = comm_std, color = l3_s3 == "3 3")) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw()

##############################################################################

BplusErzd <- BplusE %>%
  umx_residualize(var = c("lex_total", "syn_total"),
                  cov = c("sex", "mother_ed", "proj"),
                  data = .)

cor(BplusErzd$lex_total, BplusErzd$syn_total, use = "c")

mult_g1 <- multlcmm(lex_total + syn_total ~ ageC,
                    data     = as.data.frame(BplusE),
                    subject  = "data_id_num",
                    random   = ~ ageC,
                    ng       = 1,
                    link     = "3-manual-splines",
                    intnodes = c(513, 139) / exp(1))

mult_g26 <- lapply(2:3, function(ng)
                          multlcmm(lex_total + syn_total ~ ageC,
                                   data     = as.data.frame(BplusE2),
                                   subject  = "data_id_num",
                                   random   = ~ ageC,
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   link     = "3-manual-splines",
                                   intnodes = c(513, 139) / exp(1)))

fitstats_mult <- get_fit_stats(mult_g1, mult_g26)
plot_fit_stats(fitstats_mult)

# Create new Wordbank model
WBWS <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(any_of(colnames(eirli)))

library(psych)

WBWS_FA <- fa(select(WBWS, -data_id, -age), 2)

BE_FAapplied <- BplusE %>%
  ungroup() %>%
  select(all_of(colnames(WBWS)), -data_id, -age) %>%
  factor.scores(f = WBWS_FA, method = "tenBerge")

BplusE_FA <- BplusE %>%
  bind_cols(as_tibble(BE_FAapplied$scores)) %>%
  select(-any_of(WS_1A), -any_of(WS_II))

BplusE_FArzd <- BplusE_FA %>%
  umx_residualize(var = c("MR1", "MR2"),
                  cov = c("sex", "mother_ed", "proj"),
                  data = .)

BplusE_FA %>%
  filter(
    proj == "BCP"
  ) %>%
  umx_residualize(var = c("MR1", "MR2"),
                  cov = c("sex", "mother_ed"),
                  data = .) %>%
  ungroup() %>%
  select(MR1, MR2) %>%
  cor(use = "c") %>%
  round(3)

BplusE_FA %>%
  filter(
    proj == "BCP"
  ) %>%
  umx_residualize(var = c("MR1", "MR2"),
                  cov = c("age", "sex", "mother_ed"),
                  data = .) %>%
  ungroup() %>%
  select(MR1, MR2) %>%
  cor(use = "c") %>%
  round(3)

cor(BplusE_FA$MR1, BplusE_FA$MR2)
cor(BplusE_FArzd$MR1, BplusE_FArzd$MR2, use = "c")

ggplot(BplusE_FA, aes(x = MR1, y = MR2, color = status == "lg_dx")) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth() +
  scale_color_manual(values = c("black", "red")) +
  facet_grid(cols = vars(status)) +
  theme_bw()

FAmult_g1 <- multlcmm(MR1 + MR2 ~ ageC,
                      data     = as.data.frame(BplusE_FA),
                      subject  = "data_id_num",
                      random   = ~ ageC,
                      ng       = 1,
                      link     = "3-equi-splines")

FAmult_g26 <- lapply(2:3, function(ng)
                      multlcmm(MR1 + MR2 ~ ageC,
                               data     = as.data.frame(BplusE_FA),
                               subject  = "data_id_num",
                               random   = ~ ageC,
                               ng       = ng,
                               mixture  = ~ ageC,
                               link     = "3-equi-splines"))

get_fit_stats(FAmult_g1, FAmult_g26) %>%
  plot_fit_stats()

ggplot(BplusE_FA, aes(x = age, y = syn_total / lex_total)) +
  geom_line(aes(group = data_id))

BplusE2 <- BplusE %>%
  mutate(
    syn_over_lex = syn_total / lex_total
  ) %>%
  filter(
    !is.na(syn_over_lex),
    !is.infinite(syn_over_lex)
  )

lca_ratio_g1 <- lcmm(syn_over_lex ~ ageC,
                     data     = BplusE2,
                     subject  = "data_id_num",
                     ng       = 1,
                     link     = "3-equi-splines"
                     )

lca_ratio_26gs <- lapply(2:6, function(ng)
                              lcmm(syn_over_lex ~ ageC,
                                   data     = BplusE2,
                                   subject  = "data_id_num",
                                   ng       = ng,
                                   mixture  = ~ ageC,
                                   link     = "3-equi-splines"))

get_fit_stats(lca_ratio_g1, lca_ratio_26gs) %>%
  plot_fit_stats()

### Sens/spec

eirli_SS <- left_join(eirli_d, BplusE3) %>%
  select(data_id, status, l3_s3) %>%
  distinct() %>%
  filter(
    status != "no_follow_up"
  ) %>%
  mutate(
    cell = case_when(
      l3_s3 == "3 3" & status == "lg_dx" ~ "TP",
      l3_s3 == "3 3" & status == "no_lg_dx" ~ "FP",
      l3_s3 != "3 3" & status == "lg_dx" ~ "FN",
      l3_s3 != "3 3" & status == "no_lg_dx" ~ "TN",
    )
  ) %>%
  group_by(cell) %>%
  summarize(
    n = n()
  )

sens <- eirli_SS$n[eirli_SS$cell == "TP"] / (eirli_SS$n[eirli_SS$cell == "TP"] + eirli_SS$n[eirli_SS$cell == "FN"])
spec <- eirli_SS$n[eirli_SS$cell == "TN"] / (eirli_SS$n[eirli_SS$cell == "TN"] + eirli_SS$n[eirli_SS$cell == "FP"])

BplusE3 %>%
  filter(
    l3_s3 == "3 3"
  ) %>%
  select(data_id, status) %>%
  distinct() %>%
  pull(status) %>%
  table()

BplusE3 %>%
  filter(
    l3_s3 == "2 2"
  ) %>%
  select(data_id, status) %>%
  distinct() %>%
  pull(status) %>%
  table()
