setwd("~/Insync/day00096@umn.edu/Google Drive/Research/MCDI/MCDI-analysis/")

library(tidyverse)
library(lubridate)

###### Try original data

orig_dat <- read_csv("data/BCP/bcp-UMNUNC-mcdi-200131.csv") %>%
  select_all(~str_replace(., ",", ".") %>%
                str_replace("mcdi_words_sentences", "ws") %>%
                str_replace("mcdi", "wg")) %>%
  select(Identifiers, demographics.CandID, ends_with("Candidate_Age"),
         ends_with("Date_taken"), ends_with("words_produced_percentile")) %>%
  mutate(
    across(c(ends_with("Age"), ends_with("percentile")), as.numeric),
    across(ends_with("Date_taken"), ymd)
  ) %>%
  rename(
    data_id = demographics.CandID
  )

orig_wg <- orig_dat %>%
  select(data_id, starts_with("wg")) %>%
  na.omit() %>%
  mutate(
    instrument = "WG"
  ) %>%
  select_all(~str_remove(., "wg."))

orig_ws <- orig_dat %>%
  select(data_id, starts_with("ws")) %>%
  na.omit() %>%
  mutate(
    instrument = "WS"
  ) %>%
  select_all(~str_remove(., "ws."))

orig_dat2 <- bind_rows(orig_wg, orig_ws) %>%
  mutate(
    age2    = round(Candidate_Age),
    est_dob = Date_taken %m-% months(age2),
    age_now = round(interval(est_dob, today()) / years(1), 2)
  )

ggplot(orig_dat2, aes(x = Candidate_Age, y = words_produced_percentile)) +
  geom_point() +
  geom_line(aes(group = data_id)) +
  geom_smooth() +
  theme_bw()

orig_byID <- orig_dat2 %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    mean_pctile = map_dbl(data, ~median(.x$words_produced_percentile, 
                                        na.rm = TRUE)),
    last_pctile = map_dbl(data, ~.x$words_produced_percentile[length(.x$words_produced_percentile)])
  )

ggplot(orig_byID, aes(x = mean_pctile, y = last_pctile)) +
  geom_jitter(alpha = 0.5, width = 1, height = 1) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = median(orig_byID$last_pctile, na.rm = TRUE)) +
  geom_vline(xintercept = median(orig_byID$mean_pctile, na.rm = TRUE)) +
  theme_bw()

orig_dat3 <- orig_byID %>%
  mutate(
    age_now = map_dbl(data, ~mean(.x$age_now))
  ) %>%
  select(-data)

ggplot(orig_dat3, aes(x = age_now, y = last_pctile)) + 
  geom_point() +
  theme_bw()
