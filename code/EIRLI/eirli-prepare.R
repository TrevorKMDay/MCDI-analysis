setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/EIRLI")

library(tidyverse)
library(psych)

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")

wb <- readRDS(.data("Wordbank/Wordbank-WS-191105.RDS"))
wb_cats <- sort(as.character(unique(wb$category)))

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

wb_cats[!(wb_cats %in% eirli_lut$wb_cat)]

# Convert education values to numbers
educ_lut <- tibble(
    ed = c("some_secondary", "secondary", "college", "some_college",
           "graduate"),
    ed_n = c(9, 12, 16, 14, 20)
  )

################################################################################

# Load data
eirli <- read_csv(.data("EIRLI/EIRLI_CDICategory_forUMN_20210319.csv"))

# dx info (from Wes T)
dx <- read_csv(.data("EIRLI/EIRLI_dx.csv")) %>%
  select(sid, contains("dx")) %>%
  rename(
    data_id = sid,
    hx_dx = `Family history of speech or lang. dx`,
    dx    = `Any lang dx?`,
    dx_count = `Count of lang dx's`
  ) %>%
  mutate(
    across(c("hx_dx", "dx"), ~(.x == 1))
  )

################################################################################

eirli_demo <- eirli[1:5] %>%
  rename(
    # Fix names
    data_id   = `Subject Identification`,
    follow_up = `SchoolAgeFollowUp?`,
    father_ed = `FATHER'S EDUCATION`,
    gender    = Gender,
    mother_ed = `MOTHER'S EDUCATION`
  ) %>%
  mutate(
    # Change follow up to TRUE/FALSE (TRUE = any), no NAs
    follow_up = follow_up == "AnyOutcome",
    # Recode mom/dad ed to something shorter/more readable
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
  # Add dad ed in years and rename
  left_join(educ_lut, by = c("father_ed" = "ed")) %>%
  rename(father_ed_n = ed_n) %>%
  # Mom ed in years
  left_join(educ_lut, by = c("mother_ed" = "ed")) %>%
  rename(mother_ed_n = ed_n) %>%
  left_join(dx) %>%
  mutate(
    # Average parent ed (if only one avail, use it)
    parent_ed_n = select(., ends_with("_ed_n")) %>%
                    rowMeans(na.rm = TRUE),
    # If follow up is FALSE, replace dx categories with NA indicating missing,
    # not no diagnosis (dx), count == 0 (dx_count), or no family history (hx_dx)
    across(contains("dx"), ~replace(.x, !follow_up, NA))
  ) %>%
  select(
    # Reorder to something nice
    data_id, gender, follow_up, contains("dx"), starts_with("father_ed"),
    starts_with("mother_ed"), parent_ed_n
  )

# eirli_demo %>%
#   filter(is.na(parent_ed_n))

# Extract data
eirli_data <- eirli %>%
  select(`Subject Identification`, contains("W&S")) %>%
  rename(
    data_id = `Subject Identification`
  ) %>%
  pivot_longer(-c(data_id, contains("Exact Age")),
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

# Extract exact ages
eirli_exact_age <- eirli_data %>%
  select(data_id, starts_with("exact_age_")) %>%
  distinct() %>%
  pivot_longer(-data_id, names_to = c(NA, NA, "age"),
               values_to = "exact_age", names_sep = "_",
               names_transform = list(age = as.integer)) %>%
  na.omit()

# Join everything together
eirli_data_wide <- eirli_data %>%
  select(data_id, age, wb_cat, value) %>%
  pivot_wider(names_from = wb_cat, values_from = value) %>%
  filter(
    # If action words are missing, that means this is a non-existing (i.e.
    # missed session)
    !is.na(action_words)
  ) %>%
  left_join(eirli_exact_age) %>%
  left_join(eirli_demo) %>%
  select(
    # Better order
    data_id, gender, follow_up, contains("dx"), father_ed, father_ed_n,
    mother_ed, mother_ed_n, parent_ed_n, age, exact_age, everything()) %>%
  mutate(
    # Missing complexity is 0
    COMPLEXITY = replace_na(COMPLEXITY, 0)
  )


write_csv(eirli_data_wide, .data("EIRLI/EIRLI_clean.csv"))
saveRDS(eirli_data_wide, .data("EIRLI/EIRLI_clean.rds"))

################################################################################
# Some plots and such

# Some addt'l columns not for save-out
eirli_data_wide2 <- eirli_data_wide %>%
  mutate(
    # Median split
    hi_ed = parent_ed_n > median(parent_ed_n, na.rm = TRUE),
    age_diff = exact_age - age
  )

# Average 6.4 day difference between visit tpt and true age
# mean(abs(eirli_data_wide2$age_diff)) * 30

ggplot(eirli_data_wide2, aes(x = data_id, y = exact_age, color = hi_ed)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.25) +
  coord_flip() +
  scale_x_discrete(labels = NULL) +
  facet_grid(rows = vars(gender), cols = vars(follow_up))

totals <- eirli_data_wide %>%
  group_by(data_id, follow_up, dx) %>%
  summarize(
    ntpt = n()
  ) %>%
  group_by(follow_up, dx) %>%
  summarise(
    n = n()
  )

################################################################################
# Score

eirli_data_wide3 <- eirli_data_wide %>%
  rename(
    complexity = COMPLEXITY
  ) %>%
  select(-(1:13))

cat_maxes <- apply(eirli_data_wide3, 2, max)

eirli_data_wide_p <- eirli_data_wide3 %>%
  as.matrix() %>%
  apply(1, function(x) x / cat_maxes) %>%
  t() %>%
  as_tibble()

eirli_fa <- fa(eirli_data_wide_p, nfactors = 2)

fa.diagram(eirli_fa)

