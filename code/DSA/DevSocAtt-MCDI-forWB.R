# Setup ====

if (.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/DSA")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/DSA")
}

library(tidyverse)
library(lubridate)

select <- dplyr::select
rename <- dplyr::rename

source("../mcdi-setup.R")
source("../BCP/format-BCP-funcs.R")
source("../Wordbank/wordbank-functions.R")

## Load data ====

# DevSocAtt == DSA
DSA <- read_csv(.data("DSA/DevSocAtt-MCDI-vrRSB.csv")) %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(Identifiers, demographics.CandID, demographics.Sex,
         starts_with("mcdi.")) %>%
  mutate(
    across(ends_with(".Candidate_Age"), as.numeric)
  ) %>%
  rename(
    CandID = demographics.CandID
  )

# CDI ====

# Extract CDI first because then we can filter demographics down to just those
# that have MCDI for simplicity

dsa_mcdi <- DSA %>%
  select(CandID, demographics.Sex, starts_with("mcdi.")) %>%
  filter(
    mcdi.Administration == "All"
  )

# Convert to Wordbank format, renaming for function
dsa_mcdi_WB <- dsa_mcdi %>%
  select_all(~str_replace(., "mcdi.", "gest.")) %>%
  rename(
    data_id = CandID,
    # age = gest.Candidate_Age,
    sex = demographics.Sex,
  ) %>%
  format.gestures(g_dict_file = .data("WG-example.csv"))

## Demographics ====

dsa_GAB <- read_csv(.data("DSA/DevSocAtt-demo-230111.csv")) %>%
  separate(Identifiers, into = c("ID", "visit"), sep = ",") %>%
  select_all(~str_replace(., "demographics,", "")) %>% 
  filter(
    str_detect(visit, "babar")
  ) %>%
  select(CandID, Gestation_age_at_birth) %>%
  rename(
    gest_age_d = Gestation_age_at_birth
  ) %>%
  mutate(
    # Remove bad values
    gest_age_d = replace(gest_age_d, 
                         gest_age_d > 1000 | gest_age_d < 160, 
                         NA)
  )

ggplot(dsa_demo_a, aes(gest_age_d)) +
  geom_histogram()

educ_lut = tibble(
  value = c("jr_high", "some_high", "high", "some_college", "college",
            "some_grad", "grad"),
  years = seq(8, 20, 2)
)

# Want: Age, sex, mom years of education, birth order, other lgs, birth wt, 
#       gestational age (not available)
dsa_demo0 <- read_csv(.data("DSA/DevSocAtt-demoeligb-230111.csv")) %>%
  select_all(~str_replace(., "demographics_eligibility,", "")) %>%
  purrr::discard(~all(is.na(.))) %>%
  left_join(select(DSA, Identifiers, CandID)) %>%
  select(CandID, everything()) %>%
  separate(Identifiers, into = c("ID", "visit"), sep = ",") %>%
  filter(
    Administration == "All",
    # CandID %in% dsa_mcdi_WB$data_id
    str_detect(visit, "babar")
  )

dsa_birth_wt <- dsa_demo0 %>%
  select(CandID, birth_weight) %>%
  mutate(
    
    # Clean up birth weight
    birth_weight = str_replace(birth_weight, "(DK|N/A|Not Answered)",
                               NA_character_)
    
  ) %>%
  separate(birth_weight, sep = "lb.", 
           into = c("birth_wt_lbs", "birth_wt_oz")) %>%
  mutate(
    
    birth_wt_lbs = str_remove_all(birth_wt_lbs, "[^0-9.]") %>%
      as.numeric(),
    
    # Correct grams values
    birth_wt_lbs = if_else(birth_wt_lbs > 1000, birth_wt_lbs * 0.002205, 
                           birth_wt_lbs),
    
    # Remove oz/0z, spaces, and there's some external periods
    birth_wt_oz = str_remove_all(birth_wt_oz, "( |.z|^[.]|[.]$)") %>%
      as.numeric() %>%
      replace_na(0),
    
    # Sum
    birth_wt_lbs = round(birth_wt_lbs + birth_wt_oz / 16, 4)
    
  ) %>%
  select(-birth_wt_oz)
  
dsa_educ <- dsa_demo0  %>%
  select(CandID, parent1_education) %>%
  mutate_all(
    ~replace(.x, .x == "not_answered", NA)
  ) %>%
  left_join(educ_lut, by = c("parent1_education" = "value")) %>%
  rename(
    parent1_educ_yr = years
  ) 

## Siblings ====

dsa_sibs <- dsa_demo0 %>%
  select(CandID, Candidate_Age, Date_taken, matches("sibling._age")) %>%
  rowwise() 

dsa_nosibs <- dsa_sibs %>%
  filter(
    is.na(sibling1_age)
  ) %>%
  mutate(
    across(matches("sibling._age"), as.numeric)
  )

### Those with a datetime field (presumably DOB) ====

dsa_sibs_dt <- dsa_sibs %>%
  filter(
    str_detect(sibling1_age, "00:00:00")
  ) %>%
  mutate(
    across(matches("sibling._age"), as_date),
    across(matches("sibling._age"), ~interval(.x, Date_taken) / years(1))
  )

### Those with an excel date value ====

dsa_sibs_excel <- dsa_sibs %>%
  filter(
    str_detect(sibling1_age, "[0-9]{5}")
  ) %>%
  mutate(
    # Convert ages from Excel representation (presumably of DOB), 
    #   assuming origin of Jan. 1, 1900
    across(matches("sibling._age"), 
           ~as_date(as.numeric(.x), origin = ymd("1900-01-01"))),
    across(matches("sibling._age"), ~interval(.x, Date_taken) / years(1))
  )

### Other values

dsa_sibs_other <- dsa_sibs %>%
  filter(
    !str_detect(sibling1_age, "00:00:00"),
    !str_detect(sibling1_age, "[0-9]{5}")
  )

write_csv(dsa_sibs_other, "dsa_sibs_other_manualclean.csv")

dsa_sib_other_cleaned <- readxl::read_xlsx("dsa_sibs_other_manualclean.xlsx")

### SIBS COUNT ===

dsa_sibs1 <- bind_rows(dsa_nosibs, dsa_sibs_dt, dsa_sibs_excel,
                       dsa_sib_other_cleaned) %>%
  rowwise() %>%
  mutate(
    subject_age_y = Candidate_Age / 12,
    across(contains("age"), ~round(.x, 2))
  ) %>%
  select(-Candidate_Age, -Date_taken) %>%
  pivot_longer(-c(CandID, subject_age_y)) %>%
  group_by(CandID, subject_age_y) %>%
  mutate(
    sib_status = if_else(value > subject_age_y, "older", "younger")
  ) %>%
  mutate(
    n_older = sum(sib_status == "older", na.rm = TRUE),
    birth_order = n_older + 1
  ) %>%
  left_join(
    select(dsa_demo0, CandID, twin_child)
  )

dsa_sibs_final <- dsa_sibs1 %>%
  select(CandID, birth_order, twin_child) %>%
  distinct()

# Unsure what entry "ADB" for one sibling means for this person, removing their
#   birth order entry
dsa_sibs_final$birth_order[dsa_sibs_final$CandID == 860439] <- NA

## Language ====

# Despite the existence of other fields, these are the only ones that are filled
dsa_lg <- dsa_demo0 %>%
  select(CandID, primary_lang, other_home_lang1) %>%
  rename(
    lang_primary = primary_lang,
    lang_secondary = other_home_lang1
  ) %>%
  mutate(
    
    # Manual clean up of these fields
    
    lang_primary = lang_primary %>%
      tolower() %>%
      replace(.,
              . %in% c("emglish", "engliah", "englist", "englsish", "enlish",
                       "ennglish"),
              "english"),
    
    lang_secondary = lang_secondary %>%
      tolower() %>%
      replace(., . %in% c("no", "no secondary language", "non", "none", "nonw", 
                          "not applicable", "n/a", "n/s", "-", "---"),
              NA_character_) %>%
      str_replace("langauge", "language")
  )

# Final demo ====

dsa_demo1 <- dsa_mcdi %>%
  select(CandID, demographics.Sex, mcdi.Candidate_Age) %>%
  rename(
    sex = demographics.Sex,
    age_mo = mcdi.Candidate_Age
  ) %>%
  left_join(dsa_GAB) %>%
  left_join(dsa_birth_wt) %>%
  left_join(dsa_educ) %>%
  left_join(dsa_lg)

# Save data ====

write_csv(dsa_demo1, "dsa_demo-230111.csv")
write_csv(dsa_mcdi_WB, "dsa_CDI-230111.csv")
