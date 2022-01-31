####################################################
# Load libraries
####################################################

setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/MNLFA")

library(tidyverse)
library(lavaan)

for (d in c("cfa1", "cfa2"))
  dir.create(d, showWarnings = FALSE)

source("../mcdi-setup.R")

wb_demo <- read_data("Wordbank/WS-demographics.rds") %>%
  select(data_id, age, sex, birth_order) %>%
  mutate(
    birth_order = birth_order %>%
      as.character() %>%
      replace(., . != "First", "Later")
  )

# table(wb_demo$sex, wb_demo$birth_order, useNA = "a")
# All individuals missing sex are missing birth order

wbws <- read_data("Wordbank/WS-scored.rds")$p %>%
  left_join(wb_demo) %>%
  mutate(
    age_bin = cut(age, breaks = c(16, 20, 24, 28, 36), include.lowest = TRUE)
  ) %>%
  select(data_id, age, age_bin, sex, birth_order, everything())

eirli <- read_data("EIRLI/EIRLI_clean.rds") %>%
  select(-hx_dx, -dx_count, -contains("_ed")) %>%
  mutate(
    # Collapse ages 33 (n=25) and 36 (n=339)
    age_bin = age %>%
      replace(., . > 32, "33-36"),
    dx = replace_na(dx, "Unknown")
  )

###############################################################################
# Functions

run_cfas <- function(nested_data) {

  result <- nested_data %>%
    mutate(
      # number of observations for cfa() call
      n = map_int(data, nrow),
      # CFAs
      lex_cfa = map2(data, n, ~cfa(model = formulae[1], data = .x,
                                   sample.nobs = .y) ),
      syn_cfa = map2(data, n, ~cfa(model = formulae[2], data = .x,
                                   sample.nobs = .y) )
    )

  return(result)

}

extract_stats <- function(cfa_data) {

  id_cols <- cfa_data %>%
    select(-data, -n, -ends_with("cfa")) %>%
    colnames()

  # message(id_cols)

  result <- cfa_data %>%
    mutate(
      lex_fit = map(lex_cfa, ~fitMeasures(.x, fitms)),
      syn_fit = map(syn_cfa, ~fitMeasures(.x, fitms))
    ) %>%
    select(-ends_with("cfa")) %>%
    unnest(ends_with("fit")) %>%
    ungroup() %>%
    mutate(
      measure = rep(fitms, length.out = nrow(.))
    ) %>%
    pivot_wider(id_cols = c(all_of(id_cols), n), values_from = ends_with("fit"),
                names_from = measure)

  if (length(id_cols) > 0)
    result <- arrange(result, across(id_cols))

  return(result)

}


###############################################################################

# Fit measures to extract
#           chisq
fitms <- c("pvalue", "cfi", "tli", "rmsea", "srmr")

s_formulae <- 'MR1 =~ action_words + animals + body_parts + clothing +
                        descriptive_words + food_drink + furniture_rooms +
                        games_routines + household + locations + people +
                        sounds + toys + vehicles'

s_formulae[2] <- 'MR2 =~ connecting_words + helping_verbs + pronouns +
                          quantifiers + question_words + COMPLEXITY'

# Allowing cross-correlations

c_formulae <- 'MR1 =~ animals + body_parts + clothing + food_drink +
                         furniture_rooms + games_routines + household + sounds +
                          toys + vehicles

                furniture_rooms ~~ household
                animals ~~ sounds
                animals ~~ vehicles
                games_routines ~~ sounds
                body_parts ~~ games_routines
                sounds ~~ vehicles
                body_parts ~~ sounds
                body_parts ~~ furniture_rooms
                furniture_rooms ~~ games_routines
                clothing ~~ furniture_rooms
                body_parts ~~ household'

c_formulae[2] <- 'MR2 =~ connecting_words + helping_verbs + pronouns +
                          quantifiers + question_words + COMPLEXITY

                  connecting_words ~~ helping_verbs'

###############################################################################

formulae <- s_formulae
output_dir <- "cfa1"
source("aMNLFA_CFAs_run.R")

formulae <- c_formulae
output_dir <- "cfa2"
source("aMNLFA_CFAs_run.R")
