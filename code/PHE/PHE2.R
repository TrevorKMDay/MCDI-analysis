if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
}

library(tidyverse)

source("../mcdi-setup.R")
source("../growth_curving/growth-curve-functions.R")

# Load data ====

wb_wg <- read_data("Wordbank/WGasWS-scored-230214.rds")
wb_ws <- read_data("Wordbank/WS-scored-230214.rds")

wb <- bind_rows(wb_wg, wb_ws) %>%
  select(data_id, age, form, comprehension)

wg <- read_data("ELab/mcdi_loris_data_WG_all_ages.csv") %>%
  select(-ID, -ProjectID, -Sex, -ends_with("_education"),
         -ends_with("_percentile")) %>%
  mutate(
    across(ends_with("_number"), as.numeric),
    inst = "WG"
  ) %>%
  distinct()

range(wg$words_produced_number, na.rm = TRUE)

ws <- read_data("ELab/mcdi_loris_data_WS_all_ages.csv") %>%
  select(-ID, -ProjectID, -Sex, -ends_with("_education"),
         -ends_with("_percentile"), -starts_with("I")) %>%
  mutate(
    across(ends_with("_number"), as.numeric),
    inst = "WS"
  ) %>%
  distinct()

range(ws$words_produced_number, na.rm = TRUE)

all_data <- bind_rows(wg, ws) %>%
  rename(
    age = Candidate_Age
  ) %>%
  filter(
    age < 40,
    age > 10,
    Name %in% c("BCP", "Phenoscreening1/BABAR", "Phenoscreening2")
  ) %>%
  mutate(
    project = if_else(Name == "BCP", "BCP", "Phe")
  )

range(all_data$age[all_data$inst == "WG"])
range(all_data$age[all_data$inst == "WS"])

table(all_data$project, all_data$inst)

ggplot(all_data, aes(x = age, y = words_produced_number, color = inst)) +
  geom_point(alpha = 0.1) +
  geom_smooth(aes(linetype = project), se = FALSE) +
  geom_vline(xintercept = c(16, 18, 30)) +
  scale_x_continuous(limits = c(10, 30)) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()

# Gompertz ====

all_data2 <- all_data %>%
  group_by(project, inst) %>%
  nest() %>%
  mutate(
    gc = map(data, ~gomp2.fit(.x, response_var = "words_produced_number")),
    gc_pred = map(gc, ~tibble(age = 10:40,
                              yhat = predict(.x, newdata = tibble(age = 10:40))))
  )

curves <- all_data2 %>%
  select(-data, -gc) %>%
  unnest(gc_pred)

ggplot(all_data, aes(x = age, y = words_produced_number)) +
  geom_point(alpha = 0.1, aes(color = inst)) +
  geom_line(data = curves,
            aes(y = yhat, linetype = project, color = inst), linewidth = 1) +
  geom_point(data = wb,
             aes(x = age, y = comprehension, color = form),
             alpha = 0.01) +
  geom_smooth(data = wb,
              aes(x = age, y = comprehension, color = form),
              method = "lm", formula = y ~ poly(x, 3), linetype = "longdash") +
  geom_vline(xintercept = c(16, 18, 30)) +
  scale_x_continuous(limits = c(10, 40)) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()

curves_diff <- curves %>%
  pivot_wider(names_from = inst, values_from = yhat) %>%
  mutate(
    diff = WG - WS
  )

ggplot(curves_diff, aes(x = age, y = diff)) +
  geom_line(aes(linetype = project)) +
  scale_x_continuous(limits = c(10, 37))

bcp_phe_wb <- all_data %>%
  select(CandID, age, inst, project, words_produced_number) %>%
  mutate(
    CandID = as.character(CandID)
  ) %>%
  bind_rows(
    wb %>%
      rename(CandID = data_id, inst = form,
             words_produced_number = comprehension) %>%
      mutate(
        project = "WB"
      )
  )

ggplot(bcp_phe_wb, aes(x = age, y = words_produced_number)) +
  geom_point(alpha = 0.05, aes(color = project), shape = 20) +
  geom_smooth(aes(color = project, linetype = inst), se = FALSE) +
  geom_smooth(aes(linetype = inst), se = FALSE, color = "black") +
  geom_vline(xintercept = 30.5, color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(18, 37), breaks = 16:37) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_light() +
  labs(x = "Age (mo.)", y = "Words produced (WG/WS)", color = "Project",
       linetype = "Instrument")

ggplot(bcp_phe_wb, aes(x = age, y = words_produced_number)) +
  geom_point(alpha = 0.1, aes(color = project), shape = 20) +
  geom_smooth(aes(color = project), se = FALSE) +
  geom_smooth(se = FALSE, color = "black") +
  geom_vline(xintercept = 30.5, color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(NA, 18), breaks = 0:18) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_light() +
  labs(x = "Age (mo.)", y = "Words produced (WG/WS)", color = "Project",
       linetype = "Instrument")
