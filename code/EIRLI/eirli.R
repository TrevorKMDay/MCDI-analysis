setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/EIRLI")

library(tidyverse)

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")

# wb <- readRDS(.data("Wordbank/Wordbank-WS-191105.RDS"))
# sort(as.character(unique(wb$category)))

################################################################################
# Look up table to convert cats to wb_cats

eirli_lut <- tibble(
    eirli_cat = c("Action Words", "Animals Names", "Body Parts",
                  "Sentence Complexity", "Clothing", "Connecting Words",
                  "Descriptive Words", "Food", "Furniture & Rooms",
                  "Games & Routines", "Helping Verbs", "People",
                  "Prepositions & Locations", "Pronouns",
                  "Quantifiers & Articles", "Question Words",
                  "Small Household Items", "Sound Effects", "Toys", "Vehicles"),
    wb_cat    = c("action_words", "animals", "body_parts", "COMPLEXITY",
                  "clothing", "connecting_words", "descriptive_words",
                  "food_drink", "furniture_rooms", "games_routines",
                  "helping_verbs", "people", "locations", "pronouns",
                  "quantifiers", "question_words", "household", "sounds",
                  "toys", "vehicles")
  )

educ_lut <- tibble(
    ed = c("some_secondary", "secondary", "college", "some_college",
           "graduate"),
    ed_n = c(9, 12, 16, 14, 20)
  )

################################################################################

# Load data
eirli <- read_csv(.data("EIRLI/EIRLI_CDICategory_forUMN_20210319.csv"))

eirli_demo <- eirli[1:5] %>%
  rename(
    subject_id = `Subject Identification`,
    follow_up  = `SchoolAgeFollowUp?`,
    father_ed  = `FATHER'S EDUCATION`,
    gender     = Gender,
    mother_ed  = `MOTHER'S EDUCATION`
  ) %>%
  mutate(
    follow_up = if_else(follow_up == "AnyOutcome", "any", "none"),
    father_ed = recode(father_ed,
                       "College Graduate" = "college",
                       "Graduate or Professional School" = "graduate",
                       "High School Graduate" = "secondary",
                       "Junior High School" = "some_secondary",
                       "Partial College" = "some_college",
                       "Partial High SChool" = "some_secondary"),
    mother_ed = recode(mother_ed,
                       "College Graduate" = "college",
                       "Graduate or Professional" = "graduate",
                       "High School Graduate" = "secondary",
                       "Junior High School" = "some_secondary",
                       "Partial College" = "some_college",
                       "Partial High SChool" = "some_secondary")
  ) %>%
  left_join(educ_lut, by = c("father_ed" = "ed")) %>%
  rename(father_ed_n = ed_n) %>%
  left_join(educ_lut, by = c("mother_ed" = "ed")) %>%
  rename(mother_ed_n = ed_n) %>%
  select(subject_id, gender, follow_up, starts_with("father_ed"),
         starts_with("mother_ed")) %>%
  mutate(
    parent_ed_n = (mother_ed_n + father_ed_n) / 2
  )

eirli_data <- eirli %>%
  select(`Subject Identification`, contains("W&S")) %>%
  rename(
    subject_id = `Subject Identification`
  ) %>%
  pivot_longer(-c(subject_id, contains("Exact Age")),
               names_to = "eirli_cat") %>%
  select_all(~str_replace(., "Exact Age at ", "exact_age_") %>%
                str_remove(" month CDI-W&S")) %>%
  mutate(
    age = str_extract(eirli_cat, "[0-9][0-9] mo") %>%
            str_remove(" mo") %>%
            as.numeric(),
    # Remove "W&S" (already included), and month (extracted), with optional
    # spaces
    eirli_cat = str_remove(eirli_cat, "[0-9][0-9] mo") %>%
            str_remove("(CDI:)?-? ?W&S ?-? ?") %>%
            trimws()
  ) %>%
  left_join(eirli_lut)

eirli_exact_age <- eirli_data %>%
  select(subject_id, starts_with("exact_age_")) %>%
  distinct() %>%
  pivot_longer(-subject_id, names_to = c(NA, NA, "age"),
               values_to = "exact_age", names_sep = "_",
               names_transform = list(age = as.integer)) %>%
  na.omit()

eirli_data_wide <- eirli_data %>%
  select(subject_id, age, wb_cat, value) %>%
  pivot_wider(names_from = wb_cat, values_from = value) %>%
  filter(!is.na(action_words)) %>%
  left_join(eirli_exact_age) %>%
  left_join(eirli_demo) %>%
  select(subject_id, gender, follow_up, father_ed_n, mother_ed_n, parent_ed_n,
         age, exact_age, everything()) %>%
  mutate(
    COMPLEXITY = replace_na(COMPLEXITY, 0),
    hi_ed = parent_ed_n > median(parent_ed_n, na.rm = TRUE)
  )

ggplot(eirli_data_wide, aes(x = subject_id, y = exact_age, color = hi_ed)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.25) +
  coord_flip() +
  scale_x_discrete(labels = NULL) +
  facet_grid(rows = vars(gender), cols = vars(follow_up))

totals <- eirli_data_wide %>%
  group_by(subject_id) %>%
  summarize(
    ntpt = n()
  ) %>%
  group_by(ntpt >= 3) %>%
  summarise(
    n = n()
  )
