####################################################
# Load libraries
####################################################

path <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
library(ppcor)
select <- dplyr::select
rename <- dplyr::rename

source("../mcdi-setup.R")

ws <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(data_id, age, LEXICAL, SYNTAX)

ws_demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(-ethnicity, -instrument) %>%
  mutate(
    birth_order_1 = birth_order == "First",
    birth_order_n = recode(birth_order,
                           "First" = 1, "Second" = 2, "Third" = 3,
                           "Fourth" = 4, "Fifth" = 5, "Sixth" = 6,
                           "Seventh" = 7, "Eigth" = 8),
    mom_ed_coll   = mom_ed %in% c("College", "Some Graduate", "Graduate"),
    mom_ed_n      = recode(mom_ed,
                           "Primary" = 6, "Some Secondary" = 10,
                           "Secondary" = 12, "Some College" = 14,
                           "College" = 16, "Some Graduate" = 18,
                           "Graduate" = 20)
  )

WS <- left_join(ws_demo, ws) %>%
  na.omit()

WS_long <- WS %>%
  pivot_longer(c(LEXICAL, SYNTAX), names_to = "score")

ggplot(WS_long, aes(x = age, y = value)) +
  geom_jitter() +
  facet_wrap(vars(score))

################################################################################
# Age models

# LEXICAL

lex_age <- lm(LEXICAL ~ age, data = WS)
lex_age2 <- lm(LEXICAL ~ age + I(age^2), data = WS)
lex_age3 <- lm(LEXICAL ~ age + I(age^2) + I(age^3), data = WS)

anova(lex_age, lex_age2, lex_age3)

ggplot(WS, aes(x = age, y = LEXICAL)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  theme_minimal()

# SYNTAX

syn_age <- lm(SYNTAX ~ age, data = WS)
syn_age2 <- lm(SYNTAX ~ age + I(age^2), data = WS)
syn_age3 <- lm(SYNTAX ~ age + I(age^2) + I(age^3), data = WS)

anova(syn_age, syn_age2, syn_age3)

ggplot(WS, aes(x = age, y = SYNTAX)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  theme_minimal()

################################################################################
# Sex models

lex_age2_sex <- lm(LEXICAL ~ age + I(age^2) + sex, data = WS)
lex_age2_sex_x <- lm(LEXICAL ~ age*sex + I(age^2)*sex, data = WS)
anova(lex_age2, lex_age2_sex, lex_age2_sex_x)

syn_age2_sex <- lm(SYNTAX ~ age + I(age^2) + sex, data = WS)
syn_age2_sex_x <- lm(SYNTAX ~ age*sex + I(age^2)*sex, data = WS)
anova(syn_age2, syn_age2_sex, syn_age2_sex_x)

ggplot(WS, aes(x = age, y = LEXICAL)) +
  geom_jitter(alpha = 0.1) +
  stat_function(fun = ~ -521.8277 + 41.6897 * .x - 0.2892 * .x^2,
                color = "red", size = 1) +
  stat_function(fun = ~ -695.1509 + 56.1703 * .x + 306.1482 - 0.5805 * .x^2 +
                          -29.0209 * .x + 0.5842 * .x^2,
                color = "blue", size = 1) +
  theme_minimal()

ggplot(WS, aes(x = age, y = SYNTAX)) +
  geom_jitter(alpha = 0.1) +
  stat_function(fun = ~ 7.6868 - 5.0943 * .x + 0.2975 * .x^2,
                color = "red", size = 1) +
  stat_function(fun = ~ 7.6868 - 5.0943 * .x + 0.2975 * .x^2 + 61.4327 +
                        -4.5933 * .x + 0.0566 * .x^2,
                color = "blue", size = 1) +
  theme_minimal()


# Sex for lex
# Sex*age for syn

###############################################################################
# Test birth order

lex_age2_sex_BO1 <- lm(LEXICAL ~ age*sex + I(age^2)*sex + birth_order_1,
                      data = WS)
lex_age2_sex_BO <- lm(LEXICAL ~ age*sex + I(age^2)*sex + birth_order_n,
                      data = WS)
lex_age2_sex_BOx <- lm(LEXICAL ~ age*sex*birth_order_1 +
                         I(age^2)*sex*birth_order_1,
                      data = WS)

anova(lex_age2_sex, lex_age2_sex_BO1, lex_age2_sex_BO, lex_age2_sex_BOx)
# Including 1st/later is significant, but numbers are

syn_age2_sex_BO1 <- lm(SYNTAX ~ age*sex + I(age^2)*sex + birth_order_1,
                       data = WS)
syn_age2_sex_BO <- lm(SYNTAX ~ age*sex + I(age^2)*sex + birth_order_n,
                      data = WS)
syn_age2_sex_BOx <- lm(SYNTAX ~ age*sex*birth_order_1 + I(age^2)*sex*birth_order_1,
                      data = WS)

anova(syn_age2_sex, syn_age2_sex_BO1, syn_age2_sex_BO, syn_age2_sex_BOx)
# Including 1st/later is significant, but numbers are not

################################################################################
# Test mom college

lex_age2_sex_BO1_momcoll <- lm(LEXICAL ~ age*sex + I(age^2)*sex + birth_order_1 +
                                 mom_ed_coll, data = WS)

lex_age2_sex_BO1_momed <- lm(LEXICAL ~ age*sex + I(age^2)*sex + birth_order_1 +
                              mom_ed_n, data = WS)

lex_age2_sex_BO1_momcollx <- lm(LEXICAL ~ age*sex*mom_ed_coll +
                                  I(age^2)*sex*mom_ed_coll +
                                  birth_order_1*mom_ed_coll,
                                data = WS)


anova(lex_age2_sex_BO1, lex_age2_sex_BO1_momcoll, lex_age2_sex_BO1_momed)
anova(lex_age2_sex_BO1_momcoll, lex_age2_sex_BO1_momcollx)
# Mom graduated college is good, but not years

syn_age2_sex_BO1_momcoll <- lm(SYNTAX ~ age*sex + I(age^2)*sex + birth_order_1 +
                                 mom_ed_coll, data = WS)

syn_age2_sex_BO1_momed <- lm(SYNTAX ~ age*sex + I(age^2)*sex + birth_order_1 +
                               mom_ed_n, data = WS)

syn_age2_sex_BO1_momcollx <- lm(SYNTAX ~ age*sex*mom_ed_coll +
                                I(age^2)*sex*mom_ed_coll +
                                birth_order_1*mom_ed_coll, data = WS)

anova(syn_age2_sex_BO1, syn_age2_sex_BO1_momcoll, syn_age2_sex_BO1_momed)
anova(syn_age2_sex_BO1_momcoll, syn_age2_sex_BO1_momcollx)
# Mom's education doesn't matter

################################################################################


lex_final <- lex_age2_sex_BO1_momcoll
syn_final <- syn_age2_sex_BO1_momcoll


WScc <- WS %>%
  mutate(
    male = if_else(sex == "Male", 1, -1),
    firstb = if_else(birth_order_1, 1, -1),
    mom_coll = if_else(mom_ed_coll, 1, -1)
  ) %>%
  select(data_id, age, male, firstb, mom_coll, LEXICAL, SYNTAX) %>%
  na.omit()

lex_final_cc <- lm(LEXICAL ~ age*male + I(age^2)*male + firstb + mom_coll,
                   data = WScc)

coef_tbl = tibble(lex = coef(lex_final), syn = coef(syn_final)) %>%
  mutate(
    term = c("intercept", "age", "male", "agesq", "first_born",
             "mom_college", "age_sex", "agesq_sex")
  ) %>%
  pivot_longer(-term, names_to = "score", values_to = "coef")

plot_me_lex <- expand.grid(age = 16:30,
                            sex = c("Male", "Female"),
                            mom_ed_coll = c(TRUE, FALSE),
                            birth_order_1 = c(TRUE, FALSE)) %>%
  mutate(
    score = "lex",
    y_hat = predict(lex_final, .)
  )

plot_me_syn <- expand.grid(age = 16:30,
                           sex = c("Male", "Female"),
                           mom_ed_coll = c(TRUE, FALSE),
                           birth_order_1 = c(TRUE, FALSE)) %>%
  mutate(
    score = "syn",
    y_hat = predict(syn_final, .)
  )

plot_me <- bind_rows(plot_me_syn, plot_me_lex)

ggplot(plot_me, aes(x = age, y = y_hat,
                    color = interaction(sex, birth_order_1))) +
  geom_line(aes(linetype = mom_ed_coll), size = 1) +
  scale_x_continuous(breaks = 16:30) +
  facet_wrap(vars(score), scales = "free_y") +
  theme_bw()

################################################################################

cor(WS$LEXICAL, WS$SYNTAX)

pWS <- WS %>%
  mutate(
    age2 = age^2,
    male = if_else(sex == "Male", 1, 0),
    BO1  = if_else(birth_order_1, 1, 0),
    momC = if_else(mom_ed_coll, 1, 0),
    age_male = age * male,
    age2_male = age2 * male,
  ) %>%
  select(starts_with("age"), male, BO1, momC, LEXICAL, SYNTAX)

partial <- pcor(pWS)

corrplot::corrplot(partial$estimate,
                   method = "number", type = "lower", diag = FALSE,
                   p.mat = partial$p.value, sig.level = 0.05 / 21)

###############################################################################

# Partial correlation for FA

FA <- read_data("Wordbank/WS-FA_scores.rds")$scores %>%
  as_tibble() %>%
  dplyr::rename(FA_LEXICAL = MR1, FA_SYNTAX = MR2)

wsFA <- bind_cols(ws_demo, FA)

ggplot(wsFA, aes(x = FA_LEXICAL, y = FA_SYNTAX, color = age)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_color_viridis() +
  theme_bw() +
  labs(x = "Inventory size", y = "Sentence complexity", color = "Age (mo.)")

pWSFA <- wsFA %>%
  mutate(
    age2 = age^2,
    male = if_else(sex == "Male", 1, 0),
    BO1  = if_else(birth_order_1, 1, 0),
    momC = if_else(mom_ed_coll, 1, 0),
    age_male = age * male,
    age2_male = age2 * male,
  ) %>%
  select(starts_with("age"), male, BO1, momC, starts_with("FA")) %>%
  na.omit()

partial_FA <- pcor(pWSFA)

# Partial correlation for MNLFA

MNLFA <- read_data("MNLFA/eta_results.csv") %>%
  filter(
    model == "3d"
  ) %>%
  mutate(
    ID = as.character(ID)
  ) %>%
  select(-model)

wsMNLFA <- right_join(ws_demo, MNLFA, by = c("data_id" = "ID", "age"))

p_pWSFA <- wsMNLFA %>%
  mutate(
    age2 = age^2,
    male = if_else(sex == "Male", 1, 0),
    BO1  = if_else(birth_order_1, 1, 0),
    momC = if_else(mom_ed_coll, 1, 0),
    age_male = age * male,
    age2_male = age2 * male,
  ) %>%
  select(starts_with("age"), male, BO1, momC, starts_with("ETA")) %>%
  na.omit()

partial_MNLFA <- pcor(p_pWSFA)

# No mean impact MNLFA models

lex_nmi <- readModels("models_nomeanimpact/lex_3d/scoring.out")$savedata %>%
  dplyr::rename(ETA_NMI_L = ETA) %>%
  mutate(
    age = D_AGE_C + 23
  ) %>%
  select(ID, age, ETA_NMI_L)

syn_nmi <- readModels("models_nomeanimpact/syn_3d/scoring.out")$savedata %>%
  dplyr::rename(ETA_NMI_S = ETA) %>%
  mutate(
    age = D_AGE_C + 23
  ) %>%
  select(ID, age, ETA_NMI_S)

nmi <- left_join(lex_nmi, syn_nmi) %>%
  mutate(
    ID = as.character(ID)
  )

wsNMI <- right_join(ws_demo, nmi, by = c("data_id" = "ID", "age"))

by_age <- WS %>%
  left_join(wsFA) %>%
  left_join(wsMNLFA) %>%
  left_join(wsNMI) %>%
  filter(
    data_id %in% WS$data_id
  ) %>%
  arrange(age)

ggplot(by_age, aes(x = ETA_L, y = (ETA_L - ETA_NMI_L) / ETA_L)) +
  geom_point()

by_age_n <- by_age %>%
  group_by(age) %>%
  nest() %>%
  mutate(
    size   = map_int(data, nrow),
    r_raw  = map_dbl(data, ~cor(.x$LEXICAL, .x$SYNTAX)),
    r_FA    = map_dbl(data, ~cor(.x$FA_LEXICAL, .x$FA_SYNTAX)),
    r_MNLFA = map_dbl(data, ~cor(.x$ETA_L, .x$ETA_S)),
    r_MNLFA_NMI = map_dbl(data, ~cor(.x$ETA_NMI_L, .x$ETA_NMI_S))
  ) %>%
  arrange(age)

by_age_long <- by_age_n %>%
  select(age, size, starts_with("r")) %>%
  pivot_longer(-c(age, size)) %>%
  mutate(
    name = str_remove(name, "^r_")
  )

score_table <- tibble(
    name = c("raw", "FA", "MNLFA", "MNLFA_NMI"),
    data  = list(
              rename(WS,      score_lex = LEXICAL,    score_syn = SYNTAX),
              rename(wsFA,    score_lex = FA_LEXICAL, score_syn = FA_SYNTAX) %>%
                na.omit(),
              rename(wsMNLFA, score_lex = ETA_L,     score_syn = ETA_S),
              rename(wsNMI,   score_lex = ETA_NMI_L, score_syn = ETA_NMI_S)
            ),
    dat_num = map(data, ~mutate(.x,
                          age2 = age^2,
                          male = if_else(sex == "Male", 1, 0),
                          BO1  = if_else(birth_order_1, 1, 0),
                          momC = if_else(mom_ed_coll, 1, 0),
                          age_male = age * male,
                          age2_male = age2 * male,
                        ) %>%
                        select(starts_with("age"), male, BO1, momC,
                               starts_with("score"))
                  ),
    cor  = map(dat_num, cor),
    r    = map_dbl(cor, ~.x[8, 9]),
    pcor = map(dat_num, pcor),
    part_r = map_dbl(pcor, ~.x$estimate[8, 9])
  )

score_table_long <- score_table %>%
  select(name, r, part_r) %>%
  pivot_longer(-name, names_to = "method")

ggplot(by_age_long, aes(x = age, y = value, color = name)) +
  geom_line(color = "black") +
  geom_point(aes(size = size), alpha = 0.75) +
  geom_hline(data = score_table_long,
             aes(yintercept = value, color = name, linetype = method)) +
  scale_y_continuous(limits = 0:1, breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = seq(16, 30, 2)) +
  viridis::scale_color_viridis(discrete = TRUE) +
  facet_grid(cols = vars(name)) +
  theme_bw()

wsFA2 <- wsFA %>%
  select(data_id, age, starts_with("FA_")) %>%
  mutate(
    age2 = age * age
  ) %>%
  umx::umx_residualize(c("FA_LEXICAL", "FA_SYNTAX"),
                       c("age", "age2"),
                       data = .)

cor(wsFA$FA_LEXICAL, wsFA$FA_SYNTAX)
cor(wsFA2$FA_LEXICAL, wsFA2$FA_SYNTAX)

ggplot(wsFA2, aes(x = FA_LEXICAL, y = FA_SYNTAX, color = age)) +
  geom_point(alpha = 0.25) +
  viridis::scale_color_viridis() +
  scale_x_continuous(sec.axis = dup_axis()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme_minimal() +
  labs(x = "Resid. lexical FA score", y = "Resid. syntax FA score",
       color = "Age (mo).") +
  theme(legend.position = "bottom")

lm(FA_SYNTAX ~ FA_LEXICAL, data = wsFA2) %>%
  summary()

################################################################################
