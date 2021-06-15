setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/EIRLI")

library(tidyverse)
library(patchwork)

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")

select <- dplyr::select
rename <- dplyr::rename
summarize <- dplyr::summarize

wb <- read_data("Wordbank/Wordbank-WS-191105.RDS")
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
                  "toys", "vehicles"),
    lexical   = c(T, T, T, F, T, F, T, T, T, T, F, T, F, F, F, F, T, T, T, T)
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
                       "College Graduate"                = "college",
                       "Graduate or Professional School" = "graduate",
                       "High School Graduate"            = "secondary",
                       "Junior High School"              = "some_secondary",
                       "Partial College"                 = "some_college",
                       "Partial High School"             = "some_secondary"),
    mother_ed = recode(mother_ed,
                       "College Graduate"         = "college",
                       "Graduate or Professional" = "graduate",
                       "High School Graduate"     = "secondary",
                       "Junior High School"       = "some_secondary",
                       "Partial College"          = "some_college",
                       "Partial High School"      = "some_secondary"),
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

eirli_demo %>%
  group_by(follow_up, dx) %>%
  summarize(
    n = n(),
    n_f = sum(gender == "Female"),
    n_dad_ed = sum(!is.na(father_ed_n)),
    m_dad_ed = mean(father_ed_n, na.rm = TRUE),
    sd_dad_ed = sd(father_ed_n, na.rm = TRUE),
    n_mom_ed = sum(!is.na(mother_ed_n)),
    m_mom_ed = mean(mother_ed_n, na.rm = TRUE),
    sd_mom_ed = sd(mother_ed_n, na.rm = TRUE)
  ) %>%
  mutate(
    p_f = n_f / n
  ) %>%
  View()

eirli_demo %>%
  summarize(
    n = n(),
    n_f = sum(gender == "Female"),
    n_dad_ed = sum(!is.na(father_ed_n)),
    m_dad_ed = mean(father_ed_n, na.rm = TRUE),
    sd_dad_ed = sd(father_ed_n, na.rm = TRUE),
    n_mom_ed = sum(!is.na(mother_ed_n)),
    m_mom_ed = mean(mother_ed_n, na.rm = TRUE),
    sd_mom_ed = sd(mother_ed_n, na.rm = TRUE)
  ) %>%
  mutate(
    p_f = n_f / n
  )

sum(is.na(eirli_demo$father_ed_n))

eirli_demo %>%
  filter(
    is.na(mother_ed_n)
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
  summarize(
    n = n()
  )

################################################################################

eirli_demo <- eirli_data_wide[, 1:13] %>%
  select(-contains("age")) %>%
  distinct()

table(eirli_demo$follow_up, eirli_demo$dx)

eirli_4 <- eirli_data_wide2 %>%
  rename(
    complexity = COMPLEXITY,
  ) %>%
  mutate(
    LEXICAL = select(., all_of(eirli_lex)) %>%
                rowSums(),
    SYNTAX  = select(., all_of(eirli_syn)) %>%
                rowSums()
  ) %>%
  filter(follow_up) %>%
  select(data_id, dx, gender, exact_age, LEXICAL, SYNTAX)

ggplot(eirli_4, aes(x = LEXICAL, y = SYNTAX)) +
  geom_point()

source("../growth_curving/growth-curve-functions.R")

long_by_id <- eirli_4 %>%
  group_by(data_id, gender, dx) %>%
  nest() %>%
  mutate(
    n_obs   = map_int(data, nrow),
    lex_fit = map(data, ~gomp2.fit(.x, response_var = "LEXICAL",
                                   t_var = "exact_age", max = 513)),
    lex_kg  = extract.kg(lex_fit),
    syn_fit = map(data, ~gomp2.fit(.x, response_var = "SYNTAX",
                                   t_var = "exact_age", max = 139)),
    syn_kg  = extract.kg(syn_fit),
  ) %>%
  filter(n_obs >= 3)

kgs <- long_by_id %>%
  select(data_id, dx, gender, ends_with("_kg"))

pos_lut <- tibble(
    dx = c(0, 0, 1, 1),
    name = rep(c("lex_kg", "syn_kg"), length.out = 4),
    position = 1:4
  )

kgs_long <- kgs%>%
  pivot_longer(-c(data_id, dx, gender), values_to = "kg") %>%
  left_join(pos_lut)

table(kgs$dx, kgs$gender) %>%
  chisq.test()

kgs_bydx <- kgs %>%
  group_by(dx) %>%
  nest() %>%
  mutate(
    n = map(data, nrow)
  )


# Hypothesize that no-dx is greater (steeper) than dx
lex_test <- t.test(kgs_bydx$data[[1]]$lex_kg, kgs_bydx$data[[2]]$lex_kg,
                   alternative = "greater")
syn_test <- t.test(kgs_bydx$data[[1]]$syn_kg, kgs_bydx$data[[2]]$syn_kg,
                   alternative = "greater")

library(scales)
colors <- hue_pal()(2)

ggplot(kgs_long, aes(x = as.factor(dx), y = kg, fill = name)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Dx", y = "kg", fill = "Measure") +
  scale_y_continuous(limits = c(NA, 0.425)) +
  scale_x_discrete(labels = c("No Dx", "Dx")) +
  geom_segment(aes(x = 0.8, y = 0.4,  xend = 1.8, yend = 0.4), size = 1.5,
               color = colors[1]) +
  geom_segment(aes(x = 1.2, y = 0.35, xend = 2.2, yend = 0.35), size = 1.5,
               color = colors[2]) +
  annotate("text", x = 1.3, y = 0.41, label = "***", color = colors[1]) +
  annotate("text", x = 1.7, y = 0.36, label = "***", color = colors[2]) +
  theme_bw()

################################################################################

eirli_data_wide %>%
  group_by(data_id) %>%
  summarize(
    n = n()
  ) %>%
  group_by(n) %>%
  summarize(
    nn = n()
  ) %>%
  mutate(
    x_or_fewer = cumsum(nn)
  ) %>%
  arrange(desc(n)) %>%
  mutate(
    x_or_more = cumsum(nn)
  ) %>%
  arrange(n)

eirli_data_wide %>%
  group_by(age) %>%
  summarize(
    n = n(),
    m_exact_age = mean(exact_age),
    sd_exact_age = sd(exact_age)
  ) %>%
  mutate(
    diff = m_exact_age - age,
    diff_days = round(diff * 30),
    sd_exact_age_days = round(sd_exact_age * 30, 1)
  )

eirli_dx_wide <- eirli_data_wide %>%
  filter(follow_up, dx) %>%
  select(data_id, gender, age, exact_age)


eirli_dx_count <- eirli_dx_wide %>%
  group_by(data_id) %>%
  summarize(
    timepoints = n()
  ) %>%
  group_by(timepoints) %>%
  summarize(
    n = n()
  ) %>%
  arrange(desc(timepoints)) %>%
  mutate(
    x_or_more = cumsum(n)
  ) %>%
  arrange(timepoints)

ggplot(eirli_dx_wide, aes(x = exact_age, y = data_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(. ~ gender) +
  scale_y_discrete(labels = NULL) +
  theme_bw

################################################################################

eirli_sum <- eirli_data_wide2 %>%
  select(data_id, gender, dx, age, exact_age, all_of(eirli_lut$wb_cat)) %>%
  mutate(
    LEXICAL = select(., all_of(eirli_lut$wb_cat[eirli_lut$lexical])) %>%
                rowSums(),
    SYNTAX  = select(., all_of(eirli_lut$wb_cat[!eirli_lut$lexical])) %>%
                rowSums(),
    SUM = select(., all_of(eirli_lut$wb_cat)) %>%
            rowSums()
  )

png("eirli_grid_plot.png", width = 6, height = 4, units = "in", res = 300)

ggplot(eirli_sum, aes(x = exact_age, y = SUM, color = dx)) +
  geom_point(alpha = 0.1) +
  facet_grid(rows = vars(gender), cols = vars(dx)) +
  geom_line(alpha = 0.1, aes(group = data_id)) +
  geom_smooth(size = 1.5, aes(linetype = gender)) +
  theme_bw() +
  labs(x = "Age (mo)", y = "Total words",
       title = "Total words spoken by Dx group and gender (EIRLI)")

dev.off()

png("eirli_smoothers.png", width = 5, height = 5, units = "in", res = 300)

ggplot(eirli_sum, aes(x = exact_age, y = SUM, color = dx)) +
  geom_smooth(aes(linetype = gender), se = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Age (mo)", y = "Total words",
       title = "Total words spoken by Dx group and gender (EIRLI)")

dev.off()

eirli_lexsyn <- eirli_sum %>%
  select(exact_age, gender, dx, LEXICAL, SYNTAX) %>%
  pivot_longer(-c(exact_age, gender, dx))


png("eirli_lex_syn.png", width = 6, height = 4, units = "in", res = 300)

ggplot(eirli_lexsyn, aes(x = exact_age, y = value, color = dx)) +
  facet_wrap(facets = vars(name), scales = "free_y") +
  geom_smooth(aes(linetype = gender), se = FALSE) +
  theme_bw() +
  labs(x = "Age (mo)", y = "Total words",
       title = "MB-CDI subscores by Dx group and gender (EIRLI)") +
  theme(legend.position = "bottom")

dev.off()
