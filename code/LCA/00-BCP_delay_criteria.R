path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)

source("../mcdi-setup.R")

# Load data ####

bcp_ws <- read_data("BCP/BCP_WS_scored-200609.rds")$n
bcp_wg <- read_data("BCP/BCP_WG_asWS-200609.rds")

# Define functions ####

# This function, given a table of ages, calculates the number of words at 24
# months if possibly by interpolation

words_at_24m <- function(df) {

  df1 <- mutate(df, age = round(age, 1))

  if (sum(!is.na(df$TOTAL)) > 1) {

    new_data <- tibble(age = round(seq(11, 40, by = 0.1), 1)) %>%
      left_join(df1, by = "age") %>%
      mutate(
        inv_pred = try(pracma::interp1(age, TOTAL), silent = TRUE, TRUE)
        # inv_pred = try(pracma::interp1(age, TOTAL), FALSE)
      ) %>%
      filter(
        !is.na(inv_pred)
      )

    m24 <- which(new_data$age == 24)
    words <- new_data$inv_pred[m24]

  } else {
    words <- NA
  }

  return(words)

}

# Analysis ####

bcp_combine <- read_data("BCP/bcp-UMNUNC-mcdi-200609.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label, contains("comb")) %>%
  separate(demographics.Visit_label, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    visit = as.numeric(str_remove(visit, "m")),
    across(contains("comb"), ~replace(.x, .x == ".", NA)),
    not_combining = mcdi_words_sentences.combining_score == "no"
  ) %>%
  na.omit() %>%
  rename(
    data_id = demographics.CandID
  ) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    not_combining_concern = map_lgl(data,
                                    ~any(.x$visit >= 24 & .x$not_combining))
  )


bcp <- bind_rows(bcp_wg, bcp_ws) %>%
  select(data_id, age, TOTAL) %>%
  filter(
    !is.na(TOTAL)
  ) %>%
  arrange(data_id, age) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    w50_by_24mo  = map_lgl(data, ~any(.x$age <= 24 & .x$TOTAL >= 50)),

    min_age      = map_dbl(data, ~min(.x$age)),
    max_age      = map_dbl(data, ~max(.x$age)),

    i_words_24mo = map(data, words_at_24m) %>%
                    as.numeric() %>%
                    round(),

    i_words_ge50 = i_words_24mo >= 50,

    # If the data contain the determination that the child had 50w by 24 months,
    #  assign true. If it doesn't, then check the interpolation. If the interp
    #  predicts 50 words, also flag as TRUE, only if both checks fail, then
    #  assign FALSE. If interpolation was NA, then use that (not enough info
    #  to determine)
    w50_by_24mo2 = if_else(w50_by_24mo, TRUE, i_words_ge50)

  )

table(bcp$w50_by_24mo2, useNA = "a")

cbcl <- read_data("BCP/bcp-CBCL-220209.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label, CBCL.Administration,
         CBCL.ear_infections, CBCL.worried_lang_development) %>%
  rename(
    data_id      = demographics.CandID,
    cohortXvisit = demographics.Visit_label,
    admin        = CBCL.Administration,
    ear_inf      = CBCL.ear_infections,
    lg_concerns  = CBCL.worried_lang_development
  ) %>%
  filter(
    admin == "All"
  ) %>%
  separate(cohortXvisit, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    visit       = as.numeric(str_remove(visit, "m")),
    ear_inf     = replace(ear_inf, ear_inf == "not_answered", NA),
    ear_inf_ge6 = ear_inf %in% c("6-8", "9_or_more"),
    lg_concerns = case_when(lg_concerns == "yes" ~ TRUE,
                            lg_concerns == "no"  ~ FALSE,
                            lg_concerns == "not_answered" ~ NA)
  )

delay3 <- left_join(cbcl, select(bcp, data_id, w50_by_24mo2)) %>%
  left_join(select(bcp_combine, -data)) %>%
  rename(
    w50       = w50_by_24mo2
  ) %>%
  select(-admin, -ear_inf, -visit) %>%
  distinct() %>%
  ungroup() %>%
  mutate(
    lt50w = !w50,
  ) %>%
  mutate(

    data_id = as.character(data_id),


    n_flags = select(., lt50w, not_combining_concern, lg_concerns,
                      ear_inf_ge6) %>%
                rowSums(na.rm = TRUE),

    delay = case_when(

              # FIRST ASSIGN COMPLETE STATUSES

              # If no flags, consider no delay
              w50 & !not_combining_concern & !lg_concerns & !ear_inf_ge6 ~ 0,

              # <50 words at 24mo, and language concerns
              !w50 & lg_concerns ~ 1,

              # <50 words at 24mo, and 6+ ear infections
              !w50 & ear_inf_ge6 ~ 2,

              w50 & not_combining_concern & lg_concerns ~ 3,
              w50 & not_combining_concern & ear_inf_ge6 ~ 4,

              # NOW START FILLING IN GUESSES

              # If there were no flags excluding NA,
              n_flags == 0 ~ 0.1,
              # If the only flag was parent-report lg concerns, assume OK
              n_flags == 1 & lg_concerns ~ 0.2,

              # If ear infection and words/not combining is ok
              ear_inf_ge6 & w50 & !not_combining_concern ~ 0.3,
              ear_inf_ge6 & w50                          ~ 0.31,
              ear_inf_ge6 & !not_combining_concern       ~ 0.32,
              ear_inf_ge6 & !lg_concerns                 ~ 0.33,

              ear_inf_ge6 & lg_concerns ~ 2.5,
              w50 & not_combining_concern ~ 2.9,

              !w50 ~ 2.1,


            )

  )

table(delay3$delay, useNA = "a")

write_csv(delay3, "BCP_delay3.csv")

##### Analyze those in category 3.3

# Delay status for all individuals
bcp_delay <- read_csv("delay3.csv") %>%
  mutate(
    data_id = as.character(data_id)
  )

# Information about those BCP participants in class 3.3
bcp_33 <- BE3 %>%
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
BCP_mullen_vl <- BE3 %>%
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
  left_join(select(BE3, data_id, l3_s3))

ggplot(vl2, aes(x = visit, y = comm_std, color = l3_s3 == "3 3")) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = data_id), alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw()

##############################################################################

BErzd <- BE %>%
  umx_residualize(var = c("lex_total", "syn_total"),
                  cov = c("sex", "mother_ed", "proj"),
                  data = .)

cor(BErzd$lex_total, BErzd$syn_total, use = "c")

mult_g1 <- multlcmm(lex_total + syn_total ~ ageC,
                    data     = as.data.frame(BE),
                    subject  = "data_id_num",
                    random   = ~ ageC,
                    ng       = 1,
                    link     = "3-manual-splines",
                    intnodes = c(513, 139) / exp(1))

mult_g26 <- lapply(2:3, function(ng)
  multlcmm(lex_total + syn_total ~ ageC,
           data     = as.data.frame(BE2),
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

BE_FAapplied <- BE %>%
  ungroup() %>%
  select(all_of(colnames(WBWS)), -data_id, -age) %>%
  factor.scores(f = WBWS_FA, method = "tenBerge")

BE_FA <- BE %>%
  bind_cols(as_tibble(BE_FAapplied$scores)) %>%
  select(-any_of(WS_1A), -any_of(WS_II))

BE_FArzd <- BE_FA %>%
  umx_residualize(var = c("MR1", "MR2"),
                  cov = c("sex", "mother_ed", "proj"),
                  data = .)

BE_FA %>%
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

BE_FA %>%
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

cor(BE_FA$MR1, BE_FA$MR2)
cor(BE_FArzd$MR1, BE_FArzd$MR2, use = "c")

ggplot(BE_FA, aes(x = MR1, y = MR2, color = status == "lg_dx")) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth() +
  scale_color_manual(values = c("black", "red")) +
  facet_grid(cols = vars(status)) +
  theme_bw()

FAmult_g1 <- multlcmm(MR1 + MR2 ~ ageC,
                      data     = as.data.frame(BE_FA),
                      subject  = "data_id_num",
                      random   = ~ ageC,
                      ng       = 1,
                      link     = "3-equi-splines")

FAmult_g26 <- lapply(2:3, function(ng)
  multlcmm(MR1 + MR2 ~ ageC,
           data     = as.data.frame(BE_FA),
           subject  = "data_id_num",
           random   = ~ ageC,
           ng       = ng,
           mixture  = ~ ageC,
           link     = "3-equi-splines"))

get_fit_stats(FAmult_g1, FAmult_g26) %>%
  plot_fit_stats()

ggplot(BE_FA, aes(x = age, y = syn_total / lex_total)) +
  geom_line(aes(group = data_id))

BE2 <- BE %>%
  mutate(
    syn_over_lex = syn_total / lex_total
  ) %>%
  filter(
    !is.na(syn_over_lex),
    !is.infinite(syn_over_lex)
  )

lca_ratio_g1 <- lcmm(syn_over_lex ~ ageC,
                     data     = BE2,
                     subject  = "data_id_num",
                     ng       = 1,
                     link     = "3-equi-splines"
)

lca_ratio_26gs <- lapply(2:6, function(ng)
  lcmm(syn_over_lex ~ ageC,
       data     = BE2,
       subject  = "data_id_num",
       ng       = ng,
       mixture  = ~ ageC,
       link     = "3-equi-splines"))

get_fit_stats(lca_ratio_g1, lca_ratio_26gs) %>%
  plot_fit_stats()

### Sens/spec

eirli_SS <- left_join(eirli_d, BE3) %>%
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

BE3 %>%
  filter(
    l3_s3 == "3 3"
  ) %>%
  select(data_id, status) %>%
  distinct() %>%
  pull(status) %>%
  table()

BE3 %>%
  filter(
    l3_s3 == "2 2"
  ) %>%
  select(data_id, status) %>%
  distinct() %>%
  pull(status) %>%
  table()

# Compare with other estimate ====

BplusE <- read_data("LCA/BplusE.rds") %>%
  filter(status == "BCP") %>%
  select(data_id, delay_status.x) %>%
  distinct() %>%
  ungroup() %>%
  left_join(delay3, .) %>%
  mutate(
    delay3_status = case_when(
      delay == 0 ~ "confirmed_no_delay",
      delay < 1 ~ "probable_no_delay",
      delay %in% 1:4 ~ "confirmed_delay",
      delay > 1 ~ "probable_delay"
    )
  )

table(BplusE$delay3_status, BplusE$delay_status.x, useNA = "a")
