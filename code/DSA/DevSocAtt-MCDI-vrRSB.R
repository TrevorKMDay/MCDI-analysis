if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/DSA")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/DSA")
}

library(tidyverse)
library(scales)
library(MuMIn)

select <- dplyr::select
rename <- dplyr::rename

source("../mcdi-setup.R")
source("../BCP/format-BCP-funcs.R")
source("../Wordbank/wordbank-functions.R")

# DevSocAtt == DSA
DSA <- read_csv(.data("DSA/DevSocAtt-MCDI-vrRSB.csv")) %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(Identifiers, demographics.CandID, demographics.Sex,
         starts_with("mcdi."), starts_with("vrRSB.")) %>%
  filter(
    # Need both MCDI and vrRSB ...
    mcdi.Administration  == "All",
    vrRSB.Administration == "All",
    # ... collected with in six weeks (1.5 months)
    abs(mcdi.Candidate_Age - vrRSB.Candidate_Age) < 1.5
  ) %>%
  rename(
    CandID = demographics.CandID
  )

## Demographics

educ_lut = tibble(
  value = c("jr_high", "some_high", "high", "some_college", "college",
            "some_grad", "grad"),
  years = seq(8, 20, 2)
)

income_lut = tibble(
  value = c("75_100k", "over_200k", "50_75k", "100_150k", "35_50k", "150_200k",
            "25_35k", NA, "less_than_25k"),
  demo.income_1000  = c(87.5, 200, 62.5, 125, 42.5, 175, 29, NA, 12.5)
)

dsa_demo <- read_csv(.data("DSA/DevSocAtt-demo.csv")) %>%
  select_all(~str_replace(., "demographics_eligibility,", "demo.")) %>%
  purrr::discard(~all(is.na(.))) %>%
  select(Identifiers, demo.household_income, ends_with("education")) %>%
  mutate_all(~replace(.x, .x == "not_answered", NA)) %>%
  left_join(educ_lut, by = c("demo.parent1_education" = "value")) %>%
  rename(
    demo.parent1_educ_yr = years
  ) %>%
  left_join(educ_lut, by = c("demo.parent2_education" = "value")) %>%
  rename(
    demo.parent2_educ_yr = years
  ) %>%
  mutate(
    demo.household_educ = select(., ends_with("_educ_yr")) %>%
                          apply(1, mean, na.rm = TRUE)
  ) %>%
  left_join(income_lut, by = c("demo.household_income" = "value")) %>%
  left_join(select(DSA, Identifiers, CandID)) %>%
  select(CandID, starts_with("demo"))



## vrRSB #######################################################################

DSA %>%
  select(starts_with("vrRSB")) %>%
  select_if(is.numeric) %>%
  View()

dsa_vrrsb <- DSA %>%
  select(CandID, demographics.Sex,
         vrRSB.Candidate_Age, matches("vrRSB.*_score"), ends_with("_items")) %>%
  rename(
    age_vrrsb = vrRSB.Candidate_Age,
    RSB = vrRSB.RSB_total_score,
    VRS = vrRSB.video_reference_score,
    SCI = vrRSB.social_communicative_items,
    RRI = vrRSB.restricted_repetitive_items
  ) %>%
  mutate(
    RSBz = scale(RSB),
    VRSz = scale(VRS)
  )

dsa_vrrsb %>%
  select(age_vrrsb, matches("^...$")) %>%
  cor() %>%
  round(2)

ggplot(dsa_vrrsb, aes(x = RSB, y = VRS)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.25) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "red")

dsa_vrrsb %>%
  select(CandID, RSB, VRS) %>%
  pivot_longer(-CandID) %>%
  ggplot(aes(x = value, fill = name)) +
    geom_density(alpha = 0.5) +
    facet_wrap(. ~ name, ncol = 1, scales = "free_x")

dsa_vrrsb_flceil <- dsa_vrrsb %>%
  mutate(age_y = round(age_vrrsb)) %>%
  group_by(age_y) %>%
  dplyr::summarize(
    n = n(),
    n_lt10 = sum(VRS < 32 * .1),
    n_gt90 = sum(VRS > 32 * .9)
  )

################################################################################

## MCDI ########################################################################

dsa_mcdi <- DSA %>%
  select(CandID, demographics.Sex, starts_with("mcdi."))

sum(dsa_mcdi$mcdi.Candidate_Age <= 18)

dsa_mcdi_pctile <- dsa_mcdi %>%
  select(CandID, mcdi.Candidate_Age, mcdi.words_produced_percentile) %>%
  mutate(
    pctile = replace(mcdi.words_produced_percentile,
                     mcdi.words_produced_percentile == "Candidate too old to score",
                     NA) %>%
                as.numeric()
  )

# Convert to Wordbank format, renaming for function
dsa_mcdi_WB <- dsa_mcdi %>%
  select_all(~str_replace(., "mcdi.", "gest.")) %>%
  rename(
    data_id = CandID,
    age = gest.Candidate_Age,
    sex = demographics.Sex,
  ) %>%
  format.gestures(g_dict_file = .data("WG-example.csv"))

# Now score once formatted as WB
dsa_mcdi_scored <- score.WG(dsa_mcdi_WB)$n %>%
  mutate(
    TOTAL = LEXICAL + SYNTAX
  )

dsa_mcdi_scored %>%
  select(LEXICAL, SYNTAX, TOTAL) %>%
  cor()

################################################################################

all <- dsa_mcdi_scored %>%
  rename(
    CandID = data_id
  ) %>%
  select(CandID, age, TOTAL, LEXICAL, SYNTAX) %>%
  left_join(dsa_vrrsb) %>%
  mutate(
    avg_age = (age + age_vrrsb) / 2,
    age_quantile = cut(avg_age, breaks = 5, labels = 1:5),
    mcdi_quantile = cut(TOTAL, breaks = 5, labels = 1:5),
    log_VRS = log(VRS + 1),
    log_RSB = log(RSB + 1)
  ) %>%
  rename(
    sex = demographics.Sex
  ) %>%
  left_join(dsa_demo) %>%
  left_join(dsa_mcdi_pctile) %>%
  mutate(
    # Dummy code
    male = ifelse(sex == "Male", 1, 0)
  ) %>%
  mutate(across(starts_with("demo"),
                # Only two subjects with missing data, use median
                # (for ANOVA)
                ~replace(.x, is.na(.x), median(.x, na.rm = TRUE))))

all_summary <- all %>%
  mutate(age_y = round(age)) %>%
  group_by(age_y) %>%
  dplyr::summarize(
    n = n(),
    mcdi_sd = sd(TOTAL) / 396,
    vrs_sd = sd(VRS)    / 32
  ) %>%
  pivot_longer(-c(age_y, n))

ggplot(all_summary, aes(x = age_y, y = value, color = name)) +
  geom_line() +
  geom_smooth(method = "lm")

ggplot(all, aes(x = age, y = VRS)) +
  geom_point() +
  scale_x_continuous(limits = c(16, 30), breaks = 16:30) +
  theme_bw()

# Check for missing data
# all %>%
#   select(-ends_with("ile")) %>%
#   filter(!complete.cases(.)) %>%
#   View()

dsa_demo %>%
  select_if(is.numeric) %>%
  select(-CandID) %>%
  cor(use = "complete.obs") %>%
  round(2)

# FIND BEST MODEL FOR VRRSB

best_predictor_VRS <- lm(TOTAL ~ 1 + avg_age + male + VRS, data = all)
best_predictor_RSB <- lm(TOTAL ~ 1 + avg_age + male + RSB, data = all)

AICc(best_predictor_RSB) # 19038.47
AICc(best_predictor_VRS) # 18978.01

# RSB is a linear combination of social/communicative and restricted/repetitive
# try an ANOVA to see if RR improves, since it's a lg outcome
lm_SC <- lm(TOTAL ~ 1 + avg_age + male + SCI, data = all)
lm_SC_RR <- lm(TOTAL ~ 1 + avg_age + male + SCI + RRI, data = all)
anova(lm_SC, lm_SC_RR)

# Try all variables
model1 <- lm(TOTAL ~ 1 + avg_age + male + demo.household_educ + demo.income_1000 + VRS + I(VRS^2),
             data = all)
summary(model1)

# Drop education and income (non-sig)
model2 <- lm(TOTAL ~ 1 + avg_age + male + VRS, data = all)
summary(model2)

# Now test theoretical interactions
model3 <- lm(TOTAL ~ 1 + avg_age*male + avg_age*VRS + avg_age*I(VRS^2),
             data = all)
summary(model3)

# The only sig interaction is age/male

# We know age and sex is there based on previous work
model4 <-lm(TOTAL ~  1 + avg_age*male + VRS + I(VRS^2), data = all)
summary(model4)

# Model 4 is the best one offered
anova(model1, model2, model3, model4)

ggplot(augment(model4), aes(x = .fitted, y = .resid, color = as.factor(male))) +
  geom_point() +
  geom_smooth()

ggplot(augment(model4), aes(x = .resid)) +
  geom_density()

## FIND BEST MODEL FOR RSB

model1_rsb <- lm(TOTAL ~ 1 + avg_age + male + demo.household_educ + demo.income_1000 + RSB + I(RSB^2),
             data = all)
summary(model1_rsb)

# Drop education and income (non-sig)
model2_rsb <- lm(TOTAL ~ 1 + avg_age + male + RSB + I(RSB^2), data = all)
summary(model2_rsb)

# Now test theoretical interactions
model3_rsb <- lm(TOTAL ~ 1 + avg_age*male + avg_age*RSB + avg_age*I(RSB^2),
             data = all)
summary(model3_rsb)

# The only sig interaction is age/RSB

# We know age and sex is there based on previous work
model4_rsb <-lm(TOTAL ~  1 + avg_age + male + avg_age*RSB + I(RSB^2), data = all)
summary(model4_rsb)

# model4_rsb is the best one offered
anova(model1_rsb, model2_rsb, model3_rsb, model4_rsb)

ggplot(augment(model4_rsb), aes(x = .fitted, y = .resid, color = as.factor(male))) +
  geom_point() +
  geom_smooth()

ggplot(augment(model4_rsb), aes(x = .resid)) +
  geom_density()

#################################################################################

age_quantile <- all %>%
  group_by(age_quantile) %>%
  summarize(
    avg_age = mean(avg_age)
  )

age_coefs <- age_quantile$avg_age * coef(model4)["avg_age"]
male_coefs <- c(0, coef(model4)["sexMale"])

lines <- tibble(
    quantile = 1:nrow(age_quantile),
    age = age_quantile$avg_age,
    female = FALSE,
    male   = TRUE
  ) %>%
  pivot_longer(-c(quantile, age), names_to = "x", values_to = "male") %>%
  select(-x)

plot_line <- function(model, male, age, color, predictor) {

  # Coefficients from the model
  C <- coef(model)

  # Intercept (ages fall out here)
  intercept <- C["(Intercept)"] + (C["male"] * male) + (C["avg_age"] * age) +
    (C["avg_age:male"] * male * age)

  # Slope is pred and pred2
  m_1 <- C[predictor]
  m_2 <- C[paste0("I(", predictor, "^2)")]

  lt <- ifelse(male, 1, 2)

  line <- stat_function(fun = function(x) { intercept + (m_1 * x) + (m_2 * x^2)},
                          color = hue_pal()(5)[color],
                          size = 1,
                          linetype = lt
                        )

  return(line)

}

coeflines <- lines %>%
  apply(1, function(x) plot_line(model4, male = x[3], age = x[2], color = x[1],
                                 predictor = "VRS"))

coeflines_rsb <- lines %>%
  apply(1, function(x) plot_line(model4_RSB,
                                 male = x[3], age = x[2], color = x[1],
                                 predictor = "RSB"))

ggplot(all, aes(x = VRS,  y = TOTAL)) +
  geom_jitter(width = 0.3, alpha = 0.15, aes(color = age_quantile)) +
  coeflines +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(-1, NA))

ggplot(all, aes(x = RSB,  y = TOTAL)) +
  geom_jitter(width = 0.3, alpha = 0.15, aes(color = age_quantile)) +
  coeflines_rsb +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(-1, NA))

(table(select(all, ends_with("_quantile"))) / nrow(all)) %>%
  round(3)

ggplot(all, aes(x = VRS, y = pctile)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(-1, NA))

model_pctile.1 <- lm(pctile ~ 1 + VRS, data = all)
summary(model_pctile.1)

model_pctile.2 <- lm(pctile ~ 1 + RSB, data = all)
summary(model_pctile.2)

## LEX v SYN

all <- all %>%
  mutate(
    lex_p = LEXICAL / max(LEXICAL),
    syn_p = SYNTAX  / max(SYNTAX),
    syn_m_lex = syn_p - lex_p
  )

mean(all$avg_age) # 19.76

u_l_VRS <- quantile(all$VRS)

lm(SYNTAX ~ LEXICAL + VRS, data = all) %>%
  summary()

ggplot(all, aes(x = LEXICAL, y = SYNTAX)) +
  geom_point()+
  geom_smooth()

ggplot(all, aes(x = avg_age, y = syn_m_lex, color = VRS)) +
  geom_point()  +
  scale_color_viridis_c() +
  stat_function(
    fun = function(x) { -0.145 + (0.00665 * x) - (0.00163 * u_l_VRS[2]) },
    size = 1,
    color = "red"
  ) +
  stat_function(
    fun = function(x) { -0.145 + (0.00665 * x) - (0.00163 * u_l_VRS[4]) },
    size = 1,
    color = "blue"
  )


################################################################################

all <- all %>%
  mutate(
    VRS_quantile = cut(VRS, 5, labels = 1:5) %>%
                    as.numeric(),
    high_VRS = VRS_quantile > 3
  )

ggplot(all, aes(x = VRS, y = TOTAL, color = high_VRS)) +
  geom_point(alpha = .1) +
  stat_smooth(method = "lm", fullrange = TRUE) +
  facet_grid(rows = vars(age_quantile), cols = vars(VRS_quantile),
             scale = "free") +
  scale_y_continuous(limits = c(-1, 500))

hiloVRS <- all %>%
  group_by(high_VRS) %>%
  nest()
