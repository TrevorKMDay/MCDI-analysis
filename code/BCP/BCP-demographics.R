if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Format demographics, including matching mother's/father's education
# appropriately

na_omit <- function(x) {

  # My own NA omit function that returns NA instead of 'logical(0)' when used
  # on an all-NA list

  y <- na.omit(x)

  if (length(y) == 0)
    y <- NA

  return(y)

}

get_education <- function(x) {

  result <- c(NA, NA)

  # Loop through in this order;
  for ( i in c("biological_mother", "nonbio_mother", "biological_father",
                "non_specified", "not_answered") ) {

    if (!is.na(x[i])) {
      result <- c(x[i], i)
      break
    }

  }

  return(unlist(unname(result)))

}

# Libraries

library(tidyverse)
rename <- dplyr::rename
library(readxl)

date <- "200609"

source("G:/My Drive/Research/MCDI/MCDI-analysis/code/mcdi-setup.R")

# National poverty threshold - could find one for Hennepin Co. or TC metro
poverty = "G:/My Drive/Research/MCDI/MCDI-analysis/data/other/poverty_threshold2019.xlsx"
thresholds <- read_xlsx(poverty, sheet = 2) %>%
  pivot_longer(-family_size, names_to = "num_kids", values_to = "pthresh") %>%
  mutate(
    num_kids = gsub("kids.", "", num_kids) %>%
                as.numeric()
  ) %>%
  filter(
    num_kids > 0
  ) %>%
  na_omit()

# Get demographic/demographic-eligibility data, just the education for now
DE <- read_csv(.data(paste0("BCP/bcp-UMNUNC-demoeligb-", date, ".csv"))) %>%
  select_all(~gsub("demographics,", "demo.", .)) %>%
  select_all(~gsub("demographics_eligibility,", "de.", .)) %>%
  filter(de.Administration == "All") %>%
  distinct()

N <- length(unique(DE$demo.CandID))
ids <- table(DE$demo.CandID)
multiple.ids <- names(which(ids > 1))

# DE <- filter(DE, demo.CandID %in% multiple.ids)

#
# Education
#

demo.eligb <- DE  %>%
  select(demo.CandID, de.Candidate_Age, demo.Sex, ends_with("relationship"),
         ends_with("education")) %>%
  distinct()

# Reshape a few times so that one column is the parent, and the second column
## is their education level.
demo.eligb.long <- demo.eligb %>%
  pivot_longer(-c(demo.CandID, de.Candidate_Age)) %>%
  mutate(
    name = gsub("de.", "", name)
  ) %>%
  separate(name, into = c("parent", "x"))

# Separate into two dfs
del.educ <- filter(demo.eligb.long, x == "education") %>%
  rename(education = value) %>%
  select(-x)
del.rel  <- filter(demo.eligb.long, x == "relationship")%>%
  rename(relationship = value) %>%
  select(-x)

# Merge again, 'del' is demo.eligb.long
del <- left_join(del.rel, del.educ) %>%
  select(-parent) %>%
  distinct()

##

# Pivot wider ...
del.wide <- del %>%
  pivot_wider(
    id_cols = c(demo.CandID, de.Candidate_Age),
    names_from = relationship,
    values_from = education,
    values_fn = list(education = length)
  )

# Except there's a few individuals with multiples, one person with
## 'non_specified' twice (this is ok), and two with two biomom entries.
## Presumably, those are errors
multiple <- del.wide %>%
  filter(biological_mother > 1 | non_specified > 1 | biological_father > 1 | nonbio_mother > 1 | not_answered > 1 | nonbio_father > 1)

# Take the higher education for these duplicates (so happens for all 3 to be
## 'grad'), will have to be fixed later
del2 <- del %>%
  filter(
    paste(demo.CandID, de.Candidate_Age) %in% paste(multiple$demo.CandID, multiple$de.Candidate_Age),
    education == "grad"
  )

# Remove original entries
del3 <- del %>%
  filter(!(paste(demo.CandID, de.Candidate_Age) %in% paste(multiple$demo.CandID, multiple$de.Candidate_Age)))

# Create new, cleaned del
del4 <- rbind(del3, del2)

remove(del, del2, del3, del.wide, del.educ, del.rel, demo.eligb.long, multiple)

##

# Pivot wider, this time without duplicates
del.wide2 <- del4 %>%
  pivot_wider(
    id_cols = c(demo.CandID, de.Candidate_Age),
    names_from = relationship,
    values_from = education,
    values_fn = list(education = na_omit)
  )

# Lookup table to easily convert buckets into numbers
educ_lut <- tibble(
    mom_ed = c("jr_high", "some_high", "high", "some_college", "college",
               "some_grad", "grad", "not_answered"),
    mom_ed_n = c(8, 10, 12, 14, 16, 18, 20, NA)
  )

# Get education levels
educ <- apply(del.wide2, 1, get_education) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  rename(mom_ed = ...1, parent_ed = ...2)

# Join with ID#s
educ <- left_join(educ, educ_lut) %>%
  cbind(del.wide2[, 1:2], .) %>%
  mutate(mom_ed = as_factor(mom_ed), parent_ed = as_factor(parent_ed))

#
# Gender
#

sex <- demo.eligb %>%
  select(demo.CandID, demo.Sex) %>%
  distinct()

#
# SES
#

SES <- DE %>%
  filter(
    !is.na(de.Candidate_Age)
  ) %>%
  select(demo.CandID, de.Candidate_Age, demo.Sex, de.household_income,
         de.num_siblings_all, matches("de.sibling._home"),
         matches("de.parent._relationship")) %>%
  mutate(
    de.household_income = replace(de.household_income,
                                   de.household_income %in% c(".", "not_answered"),
                                   NA) %>%
                            replace(de.household_income == "less_than_25k",
                                    "0_25k"),
    income_n = gsub(".*_", "", de.household_income) %>%
                gsub("k", "", .) %>%
                as.numeric(),
    de.num_siblings_all = as.numeric(de.num_siblings_all)
  ) %>%
  mutate_at(
    c("demo.CandID", "demo.Sex", "de.household_income"), as.factor
  ) %>%
  distinct()

# Calculate number of siblings that live at home
siblings <- SES %>%
  select(starts_with("de.sibling")) %>%
  apply(., 1, function(x) sum(x %in% c("sometimes", "yes")))

# Count parents by assuming `not_answered` is no parent, and replace any 0s with
# 1s because none of these babies live by themselves
parents <- SES %>%
  select(starts_with("de.parent")) %>%
  apply(., 1, function(x) sum(!is.na(x))) %>%
  replace(. == 0, 1)

# Calculate family size (don't forget to include the baby)
SES$n_siblings  <- siblings
SES$n_kids    <- siblings + 1
SES$n_parents   <- parents
SES$family_size <- SES$n_parents + SES$n_kids

ggplot(na_omit(SES), aes(as.factor(income_n))) +
  geom_histogram(stat = "count") +
  scale_x_discrete(labels = c("<25k", "25-35k", "35-50k", "50-75k", "75-100k",
                              "100-150k", ">200k"))

mean(SES$income_n, na.rm = TRUE)

ggplot(SES, aes(family_size)) +
  geom_histogram(stat = "count") +
  scale_x_continuous(breaks = 2:9)

income_lut <- tibble(
    de.household_income = levels(SES$de.household_income),
    income_mean = c(12.5, 125, 175, 30, 42.5, 62.5, 87.5, 250) * 1000
  )

# Add in mean income for bin, in dollars
SES <- left_join(SES, income_lut) %>%
  mutate_at("income_mean", as.numeric) %>%
  left_join(thresholds,
            by = c("family_size", "n_kids" = "num_kids")) %>%
  mutate(inr = income_mean / pthresh)

ggplot(SES, aes(inr)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 0:14) +
  labs(x = "Income-to-needs ratio", y = "Count") +
  theme_bw() +
  geom_vline(xintercept = c(mean(SES$inr, na.rm = TRUE),
                            median(SES$inr, na.rm = TRUE)),
             linetype = c("solid", "dashed"))

# Round INR to same number of sig figs as input income (3)
SES.short <- SES %>%
  select(demo.CandID, de.Candidate_Age,
         de.household_income, n_siblings, n_parents, family_size,
         income_mean, inr) %>%
  mutate(
    demo.CandID = as.numeric(as.character(demo.CandID)),
    inr = signif(inr, 3)
  ) %>%
  rename(income_bin = de.household_income) %>%
  distinct()

#
# Ethnicity
#

de.ethnicity <- DE %>%
  select(demo.CandID, contains("ethnicity"), contains("race")) %>%
  rename(
    CandID = demo.CandID,
    ethnicity_biodad = de.biodad_ethnicity,
    ethnicity_biomom = de.biomom_ethnicity,
    ethnicity = de.subject_ethnicity,
    race_biomom = de.biomom_race,
    race_biodad = de.biodad_race,
    race = de.subject_race
  ) %>%
  mutate_at(vars(-CandID),
            function(x) as.factor(replace(x, x == "not_answered", NA)))



demographics <- left_join(educ, sex) %>%
  left_join(SES.short) %>%
  rename(
    CandID = demo.CandID,
    age = de.Candidate_Age,
    sex = demo.Sex,
    educ_momed = mom_ed,
    educ_parent = parent_ed,
    educ_momed_n = mom_ed_n,
    income_inr = inr,
    family_n_sibs = n_siblings,
    family_n_parents = n_parents
  ) %>%
  select(CandID, sex, starts_with("educ"), starts_with("family"),
         starts_with("income")) %>%
  mutate_at(c("sex", "income_bin"), as.factor)

apply(demographics, 2, function(x) sum(is.na(x)))

ggplot(demographics, aes(x = family_size, y = income_inr,
                         color = educ_momed_n)) +
  geom_jitter(height = 0.25, width = 0.25, alpha = 0.25) +
  scale_x_continuous(breaks = 2:9) +
  scale_y_continuous(breaks = 0:13) +
  scale_color_viridis_c() +
  labs(x = "Family Size", y = "INR", color = "Mom Ed")

# ids <- table(demographics$demo.CandID)
# multiple.ids <- names(which(ids > 1))
#
# filter(demographics, demo.CandID %in% multiple.ids)

write_csv(demographics,
          path = paste0("data/BCP/BCP-demographics-", date, ".csv"))

