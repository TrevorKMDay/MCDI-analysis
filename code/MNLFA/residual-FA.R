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
library(broom)
library(umx)
select <- dplyr::select

source("../mcdi-setup.R")

ws <- read_data("Wordbank/WS-scored.rds")$n %>%
  select(data_id, age, LEXICAL, SYNTAX)

ws_demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(-ethnicity, -instrument) %>%
  mutate(
    age2        = age * age,
    sex_male    = as.numeric(sex == "Male"),
    age_male    = age * sex_male,
    BO1         = as.numeric(birth_order == "First"),
    mom_ed_coll = as.numeric(mom_ed %in% c("College", "Some Graduate",
                                             "Graduate")),
  ) %>%
  select(-sex, -mom_ed, -birth_order)

################################################################################
## Raw

# Raw scores with demographics
WS_raw <- left_join(ws_demo, ws) %>%
  na.omit()

################################################################################
## FA

FA <- read_data("Wordbank/WS-FA_scores.rds")$scores %>%
  as_tibble() %>%
  rename(LEXICAL = MR1, SYNTAX = MR2)

WS_FA <- bind_cols(ws_demo, FA) %>%
  na.omit()

################################################################################
## MNLFA

MNLFA <- read_data("MNLFA/eta_results.csv") %>%
  filter(
    model == "3d"
  ) %>%
  mutate(
    ID = as.character(ID)
  ) %>%
  select(-model) %>%
  rename(
    LEXICAL = ETA_L,
    SYNTAX  = ETA_S
  )

WS_MNLFA <- right_join(ws_demo, MNLFA, by = c("data_id" = "ID", "age"))

################################################################################

residualize <- function(numeric_data) {

  umx_residualize(var = c("LEXICAL", "SYNTAX"),
                  cov = c("age", "age2", "sex_male", "age_male", "BO1",
                          "mom_ed_coll"),
                  data = numeric_data)

}

all_models <- tibble(m = list(WS_raw, WS_FA, WS_MNLFA) %>%
                          lapply(function(.x) select(.x, -data_id)))

all_models <- all_models %>%
  mutate(
    cor    = map_dbl(m, ~cor(.x)[7, 8]),
    pcor   = map_dbl(m, ~pcor(.x)$estimate[7, 8]),
    rm     = map(m, residualize),
    r_cor  = map_dbl(rm, ~cor(.x)[7, 8]),
    r_pcor = map_dbl(rm, ~pcor(.x)$estimate[7, 8])
  )

ggplot(all_models$m[[2]], aes(x = age, y = LEXICAL)) +
  geom_jitter() +
  geom_smooth()

ggplot(all_models$rm[[2]], aes(x = age, y = LEXICAL)) +
  geom_jitter() +
  geom_smooth()

ggplot(all_models$rm[[2]], aes(x = age, y = SYNTAX)) +
  geom_jitter() +
  geom_smooth()



residFA <- all_models$rm[[2]]

ggplot(residFA, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  annotate("text", x = -2.5, y =  2.5,
           label = "Small vocabulary;\nbig sentences") +
  annotate("text", x =  2.5, y =  2.5,
           label = "Vocabulary on par\nwith sentences") +
  annotate("text", x =  2.5, y = -2.5,
           label = "Sentences short\nfor vocabulary") +
  annotate("text", x = -2.5, y = -2.5,
           label = "Small vocabulary;\nshort sentences") +
  viridis::scale_color_viridis() +
  theme_minimal()

lm1 <- lm(SYNTAX ~ LEXICAL, data = residFA)

lcuts = c(-3:-1, 1:3) * sd(residFA$LEXICAL)
scuts = c(-3:-1, 1:3) * sd(residFA$SYNTAX)

residFA <- residFA %>%
  mutate(
    lex_g = cut(LEXICAL, breaks = 5),
    syn_g = cut(SYNTAX, breaks = 5)
  )

table(residFA$lex_g, residFA$syn_g, useNA = "a")

rFAn <- residFA %>%
  group_by(sex_male, lex_g, syn_g) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    sex = if_else(as.logical(sex_male), "M", "F")
  ) %>%
  pivot_wider(c(lex_g, syn_g), names_from = sex, values_from = n) %>%
  mutate(
    across(c(M, F), ~replace_na(.x, 0)),
    total = M + F,
    label = paste0("M: ", M, " / F:", F),
    ratio = round(M / F, 2)
  )

ggplot(rFAn, aes(x = lex_g, y = syn_g)) +
  geom_tile(aes(fill = total)) +
  geom_text(aes(label = label), color = "black") +
  viridis::scale_fill_viridis() +
  theme_minimal()

ggplot(rFAn, aes(x = lex_g, y = syn_g)) +
  geom_tile(aes(fill = ratio)) +
  geom_text(aes(label = ratio), color = "black") +
  viridis::scale_fill_viridis() +
  theme_minimal()

###############################################################################


ws <- read_data("Wordbank/WS-scored.rds")$p %>%
  select(-LEXICAL, -SYNTAX) %>%
  select_all(~paste0("i_", .)) %>%
  rename(data_id = i_data_id, cov_age = i_age) %>%
  left_join(
    rename(ws_demo,
           cov_age2 = age2, cov_sex_male = sex_male, cov_age_male = age_male,
           cov_BO1 = BO1, cov_mom_ed_coll = mom_ed_coll)
  ) %>%
  na.omit()

resid_ws <- umx_residualize(
  var = str_subset(colnames(ws), "^i_"),
  cov = str_subset(colnames(ws), "^cov_"),
  data = ws
)

resid_ind <- resid_ws %>%
  select(starts_with("i_"))

FA2 <- psych::fa(resid_ind, nfactors = 2, rotate = "promax")

(FA2$scores %>%
  cor())[1, 2]

residFApcor <- ws_demo %>%
  na.omit() %>%
  bind_cols(as_tibble(FA2$scores)) %>%
  select(-data_id) %>%
  pcor()

residFApcor$estimate[7, 8]

##############################################################################

library(plotly)
library(ggplot2)
library(gapminder)

anim_FA <- bind_rows(
    WS_FA,
    bind_cols(select(WS_FA, data_id), select(residFA, -ends_with("_g")))
  ) %>%
  mutate(
    model = c(rep("2: FA", nrow(WS_FA)), rep("3: rsd. FA", nrow(residFA)))
  )

p <- ggplot(anim_FA, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(aes(frame = model), alpha = 0.75, shape = 16) +
  geom_smooth(aes(frame = model, linetype = sex_male == 1), method = "lm",
              se = FALSE) +
  geom_smooth(aes(frame = model, linetype = sex_male == 1), method = "lm",
              formula = y ~ poly(x, 2), se = FALSE,
              color = "red") +
  viridis::scale_color_viridis() +
  theme_bw()

ggplotly(p)

##############################################################################

residFA_syno <- residFA %>%
  umx_residualize(var = "SYNTAX", covs = "LEXICAL", data = .) %>%
  mutate(
    model = "4: SYN on LEX",
  )

WS_raw_scaled <- WS_raw %>%
  mutate(
    model = "1: Z(raw)",
    across(c(LEXICAL, SYNTAX), scale)
  )

anim_FA2 <- bind_rows(WS_raw_scaled, anim_FA,
                      bind_cols(select(WS_FA, data_id), residFA_syno))

p2 <- ggplot(anim_FA2, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(aes(frame = model), alpha = 0.5, shape = 16) +
  geom_smooth(aes(frame = model), method = "lm", se = FALSE) +
  geom_smooth(aes(frame = model), method = "lm", formula = y ~ poly(x, 2),
              se = FALSE, color = "red") +
  viridis::scale_color_viridis() +
  theme_bw()

ggplotly(p2)

anim_FA3 <- anim_FA2 %>%
  mutate(
    age_group = cut(age, 5, labels = c("0", "20th", "40th", "60th", "80th"))
  )

p3 <- ggplot(anim_FA3, aes(x = LEXICAL, y = SYNTAX, color = age_group)) +
  geom_point(aes(frame = model), alpha = 0.25, shape = 16) +
  geom_smooth(aes(frame = model), method = "lm", se = FALSE) +
  colorspace::scale_color_discrete_diverging(palette = "Red-Green") +
  theme_bw()

ggplotly(p3)

syn_over_lex <- bind_cols(select(WS_FA, data_id),
                          select(residFA_syno, SYNTAX)) %>%
  rename(syn_over_lex = SYNTAX)

anim_FA4 <- anim_FA3 %>%
  left_join(syn_over_lex)

p4 <- ggplot(anim_FA4, aes(x = LEXICAL, y = SYNTAX,
                           color = syn_over_lex)) +
  geom_point(aes(frame = model), alpha = 0.5, shape = 16) +
  geom_smooth(aes(frame = model), method = "lm", se = FALSE) +
  scale_color_viridis() +
  theme_bw()

ggplotly(p4)

###############################################################################

# Raw data

lm(SYNTAX ~ LEXICAL + I(LEXICAL ^ 2), data = WS_raw) %>%
  summary()

p1 <- ggplot(WS_raw, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.5, shape = 20) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  annotate("text", x = 0, y = 200, label = "R^2 == .8422",
           hjust = 0, parse = TRUE) +
  scale_color_viridis() +
  labs(x = "Lexical (w)", "Syntax (w)", title = "(a) Raw (quad.)") +
  theme_bw() +
  theme(legend.position = "none")

# FA

lm(SYNTAX ~ LEXICAL + I(LEXICAL ^ 2), data = WS_FA) %>%
  summary()

p2 <- ggplot(WS_FA, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.5, shape = 20) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  annotate("text", x = -1, y = 3.5, label = "R^2 == .7367",
           hjust = 0, parse = TRUE) +
  scale_color_viridis() +
  labs(x = "Lexical", "Syntax", title = "(b) FA (quad.)") +
  theme_bw() +
  theme(legend.position = "none")


# FA residualized on age/age2 only

WS_FA_rAge <- WS_FA %>%
  select(age, age2, LEXICAL, SYNTAX) %>%
  umx_residualize(c("LEXICAL", "SYNTAX"), c("age", "age2"), data = .)

lm(SYNTAX ~ LEXICAL, data = WS_FA_rAge) %>%
  summary()

p3 <- ggplot(WS_FA_rAge, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.5, shape = 20) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -2, y = 3, label = "R^2 == .4452",
           hjust = 0, parse = TRUE) +
  scale_color_viridis() +
  labs(x = "Lexical", "Syntax", title = "(c) Resid. FA (age;\nlinear)") +
  theme_bw() +
  theme(legend.position = "none")

# FA on all vars

lm(SYNTAX ~ LEXICAL, data = residFA) %>%
  summary()

p4 <- ggplot(residFA, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.5, shape = 20) +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = -2, y = 3, label = "R^2 == .4320",
           hjust = 0, parse = TRUE) +
  scale_color_viridis() +
  labs(x = "Lexical", "Syntax", title = "(d) Resid. FA (all vars.; linear)") +
  theme_bw()

library(patchwork)

png("four_R2_plots.png", width = 9.32, height = 3.74, units = "in", res = 300)

p1 | p2 | p3 | p4

dev.off()

