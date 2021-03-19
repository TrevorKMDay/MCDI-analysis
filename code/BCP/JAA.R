if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/BCP")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/BCP")

}

# Libraries

library(tidyverse)
library(nlme)

# Helper functions

mutate <- dplyr::mutate

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")
source("../growth_curving/growth-curve-functions.R")

# Joint attention assessment ###################################################

fix_series_col <- function(vector) {

    revalue(vector, c("05points" = 0.5, "1point" = 1, "2points" = 2,
                      "3points" = 3, "4points" = 4, "no_response" = 0,
                      "not_applicable" = NA, "not_valid" = NA)) %>%
      as.numeric() %>%
      return()

}

jaa <- read_csv(.data("BCP/bcp-UMN-JAA.csv")) %>%
  select_all(~str_replace(., ",", ".")) %>%
  select_all(~str_replace(., "JointAttentionAssessment", "JAA")) %>%
  filter(
    JAA.Administration == "All"
  ) %>%
  select(demographics.CandID, Identifiers,
         JAA.Candidate_Age, JAA.mean_score, starts_with("JAA.series"),
         JAA.standard_deviation, JAA.total_rawscore) %>%
  separate(Identifiers, into = c(NA, "mo"), sep = "x") %>%
  dplyr::rename(
    CandID = demographics.CandID,
    JAA.age = JAA.Candidate_Age,
    JAA.sd = JAA.standard_deviation
  ) %>%
  mutate(
    across(c("JAA.age", "JAA.mean_score", "JAA.sd", "JAA.total_rawscore"),
           as.numeric),
    across(starts_with("JAA.series"), fix_series_col),
    mo = as.numeric(str_remove(mo, "m"))
  )


ggplot(jaa, aes(x = JAA.age, y = JAA.mean_score)) +
  geom_point() +
  geom_line(aes(group = CandID))

################################################################################

age_lut <- read_csv(.data("BCP/mcdi-age-lut.csv")) %>%
  mutate(
    mcdi_age = ifelse(!is.na(wg_age), wg_age, ws_age)
  ) %>%
  dplyr::rename(
    ideal_age = age
  )

subjs_gt_3 <- read_csv(.data("BCP/test-subjs-3.csv"))  %>%
  left_join(select(age_lut, data_id, ideal_age, mcdi_age),
            by = c("data_id" = "data_id", "age" = "mcdi_age"))

ggplot(subjs_gt_3, aes(x = age, y = TOTAL)) +
  geom_point() +
  geom_line(aes(group = data_id))

jaa2 <- jaa %>%
  filter(
    CandID %in% subjs_gt_3$data_id
  ) %>%
  group_by(CandID) %>%
  nest() %>%
  arrange(CandID) %>%
  mutate(
    model = map(data,  ~lm(JAA.mean_score ~ JAA.age, data = .x)),
    at_12 = unlist(map(model, ~unname(predict(.x, data.frame(JAA.age = 12)))))
  )

jaa3 <- jaa2 %>%
  unnest(data) %>%
  mutate(
    rgroup = round(runif(1, 1, 4))
  ) %>%
  select(-model) %>%
  ungroup()

ggplot(jaa3, aes(x = JAA.age, color = as.factor(CandID))) +
  geom_point(aes(y = JAA.mean_score), shape = 1) +
  geom_point(x = 12, aes(y = at_12), shape = 16) +
  geom_line(aes(y = JAA.mean_score, group = CandID), alpha = 0.5) +
  facet_wrap(vars(rgroup))

################################################################################
# Calculate curves


demo <- read_csv(.data("BCP/BCP-demographics-200609.csv"))

curves <- subjs_gt_3 %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    total_curve = map(data, ~gomp2.fit(.x, response_var = "TOTAL")),
    syntx_curve = map(data, ~gomp2.fit(.x, response_var = "SYNTAX", max = 180)),
    lexcl_curve = map(data, ~gomp2.fit(.x, response_var = "LEXICAL",
                                       max = 575)),
    total_kg = extract.kg(total_curve),
    syntx_kg = extract.kg(syntx_curve),
    lexcl_kg = extract.kg(lexcl_curve)
  ) %>%
  mutate(
    total_ku = 680 * total_kg / exp(1),
    syntx_ku = 180 * syntx_kg / exp(1),
    lexcl_ku = 575 * syntx_kg / exp(1),
    total_24 = map_dbl(total_curve, ~predict(.x, data.frame(age = 24))),
    syntx_24 = map_dbl(syntx_curve, ~predict(.x, data.frame(age = 24))),
    lexcl_24 = map_dbl(lexcl_curve, ~predict(.x, data.frame(age = 24)))
  )


jaa_curves <- jaa3 %>%
  select(CandID, at_12) %>%
  left_join(select(curves, -data, -ends_with("_curve")),
            by = c("CandID" = "data_id")) %>%
  left_join(demo)

jaa_curves_long <- jaa_curves %>%
  select(CandID, sex, at_12, ends_with("ku")) %>%
  pivot_longer(-c(CandID, sex, at_12), names_to = "curve", values_to = "ku")

jaa_24mo_long <- jaa_curves %>%
  select(CandID, sex, at_12, ends_with("24")) %>%
  pivot_longer(-c(CandID, sex, at_12), names_to = "curve", values_to = "n")


ggplot(jaa_curves_long, aes(x = at_12, y = ku, color = curve)) +
  geom_point(aes(shape = sex)) +
  scale_shape_manual(values = c(16, 3)) +
  geom_smooth(aes(linetype = sex), method = "lm") +
  scale_color_discrete(name = "Outcome", labels = c("Lexical", "Syntax",
                                                    "Total")) +
  labs(x = "DJAA at 12mo", y = "Words learned/mo (kU)") +
  facet_wrap("curve", scales = "free_y", ncol = 2)



lm_total <- lm(total_kg ~ 1 + income_inr + sex*at_12, data = jaa_curves)
lm_syntx <- lm(syntx_kg ~ 1 + income_inr + sex*at_12, data = jaa_curves)
lm_lexcl <- lm(lexcl_kg ~ 1 + income_inr + sex*at_12, data = jaa_curves)

summary(lm_total)
summary(lm_syntx)
summary(lm_lexcl)

ggplot(jaa_24mo_long, aes(x = at_12, y = n, color = curve)) +
  geom_point(aes(shape = sex)) +
  geom_smooth(aes(linetype = sex), method = "lm") +
  scale_color_discrete(name = "Outcome", labels = c("Lexical", "Syntax",
                                                    "Total")) +
  labs(x = "DJAA @ 12mo", y = "Fitted words @ 12mo") +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap("curve", scales = "free_y", ncol = 2)
