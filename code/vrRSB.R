if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
}

library(tidyverse)
library(tidytext)
select <- dplyr::select


source("../mcdi-setup.R")

vrrsb <- read_data("BCP-PHE-vrRSB.csv") %>%
  select_all(~gsub(",", ".", .)) %>%
  filter(
    vrRSB.Administration == "All"
  ) %>%
  select(demographics.CandID, demographics.Sex,
         vrRSB.Candidate_Age, vrRSB.RSB_total_score,
         vrRSB.video_reference_score) %>%
  rename(
    CandID = demographics.CandID,
    gender = demographics.Sex,
    vrRSB.age = vrRSB.Candidate_Age,
    vrRSB.RSB = vrRSB.RSB_total_score,
    vrRSB.vrs = vrRSB.video_reference_score
  ) %>%
  mutate_at(vars(starts_with("vrRSB.")), as.numeric) %>%
  mutate(
    RSB_breaks = cut(vrRSB.RSB, breaks = 0:8 * 10, right = FALSE),
    vrs_breaks = cut(vrRSB.vrs, breaks = seq(0, 30, 5), right = FALSE)
  ) %>%
  na.omit()

vrrsb_rsb <- vrrsb %>%
  group_by(RSB_breaks, gender) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    pct = n / nrow(vrrsb)
  )

vrrsb_vrs <- vrrsb %>%
  group_by(vrs_breaks, gender) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    pct = n / nrow(vrrsb)
  )

ggplot(vrrsb_rsb, aes(x = RSB_breaks, y = pct * 100, fill = gender)) +
  geom_histogram(stat = "identity", position = "dodge")

ggplot(vrrsb_vrs, aes(x = vrs_breaks, y = pct * 100, fill = gender)) +
  geom_histogram(stat = "identity", position = "dodge")

