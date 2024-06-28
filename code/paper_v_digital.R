setwd("~/Insync/day00096@umn.edu/Google Drive/Research/MCDI/MCDI-analysis/code/")

library(tidyverse)

ibis <- read_csv("../data/IBIS/agcc_ibis_mcdi.csv") %>%
  pivot_longer(-Identifiers) %>%
  separate(name, into = c("visit", "name"), sep = " ") %>%
  separate(name, into = c("instrument", "name"), sep = ",")

ibis_demo <- ibis %>%
  filter(
    instrument == "demographics"
  ) %>%
  pivot_wider(names_from = name) %>%
  filter(
    # Remove missing and Fragile X
    str_detect(Project, "IBIS[12]"),
    Risk == "LR"
  )

table(ibis_demo$Project, ibis_demo$Risk)

project <- ibis_demo %>%
  select(Identifiers, Project) %>%
  distinct()

ibis_cdi <- ibis %>%
  filter(
    instrument == "macarthur_words_gestures",
  ) %>%
  pivot_wider(names_from = name) %>%
  filter(
    Administration == "All",
    Identifiers %in% ibis_demo$Identifiers,
  ) %>%
  select(-ends_with("percentile")) %>%
  left_join(project) %>%
  mutate(
    across(ends_with("number"), as.numeric),
    age = as.numeric(Candidate_Age)
  ) %>%
  filter(
    age > 10
  ) %>%
  pivot_longer(ends_with("number"))

project_compare <- ibis_cdi %>%
  filter(
    visit %in% c("V12", "V24")
  )

ggplot(project_compare, aes(x = visit, y = value, fill = Project)) +
  geom_boxplot() +
  facet_grid(cols = vars(name), scales = "free_y")

# Test whether paper reports from 24-month visit had higher score than
# LORIS

t_test_df <- project_compare %>%
  pivot_wider(names_from = "name") %>%
  filter(
    visit == "V24"
  )

t.test(t_test_df$words_produced_number[t_test_df$Project == "IBIS1"],
       t_test_df$words_produced_number[t_test_df$Project == "IBIS2"],
       alternative = "g")
