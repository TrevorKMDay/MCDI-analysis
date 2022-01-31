####################################################
# Load libraries
####################################################

path <- "/Research/MCDI/MCDI-analysis/code/EIRLI"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))


library(tidyverse)
library(psych)
library(viridis)
library(umx)

select <- dplyr::select

source("../mcdi-setup.R")

##############################################################################

eirli <- read_data("EIRLI/EIRLI_clean_impute.rds")$p

eirli_nodx <- eirli %>%
  filter(
    is.na(dx)
  ) %>%
  mutate(
    mom_college = mother_ed_n >= 16
  ) %>%
  select(-follow_up, -hx_dx, -dx, -dx_count, -starts_with("father"),
         -starts_with("mother"), -parent_ed_n)

eirli_nodx_n <- eirli_nodx %>%
  group_by(data_id, gender, mom_college) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )

eirli_gt3tp <- eirli_nodx_n$data_id[eirli_nodx_n$n >= 3]

rm(eirli)

##############################################################################

wbwsfa <- read_data("Wordbank/WS-FA_scores.rds")

enodx_FA <- eirli_nodx %>%
  select(all_of(rownames(wbwsfa$weights))) %>%
  factor.scores(wbwsfa$weights, method = "Harman")

enodx_FA_scores <- bind_cols(
    select(eirli_nodx, -all_of(rownames(wbwsfa$weights))),
    as_tibble(enodx_FA$scores)
  ) %>%
  rename(
    LEXICAL = MR1,
    SYNTAX  = MR2
  ) %>%
  mutate(
    age2 = exact_age ^ 2,
    male = gender == "Male",
    age_male = exact_age * male
  )

enodx_FAscores_gt3 <- enodx_FA_scores %>%
  filter(
    data_id %in% eirli_gt3tp
  )

png("e_nodx_FA_scores.png", width = 5, height = 3.74, units = "in", res = 300)

ggplot(enodx_FA_scores, aes(x = LEXICAL, y = SYNTAX, color = exact_age)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "All factor-scored EIRLI data")

dev.off()

png("e_nodx_FA_scores_lines.png", width = 5, height = 3.74, units = "in",
    res = 300)

ggplot(enodx_FA_scores, aes(x = LEXICAL, y = SYNTAX, color = exact_age)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "All factor-scored EIRLI data")

dev.off()


png("e_nodx_FA_scores_lines_gt3tp.png", width = 5, height = 3.74, units = "in",
    res = 300)

ggplot(enodx_FAscores_gt3, aes(x = LEXICAL, y = SYNTAX, color = exact_age)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "All factor-scored EIRLI data w/ >=3 timepoints")

dev.off()

enodx_FAscores_r <- enodx_FA_scores %>%
  umx_residualize(c("LEXICAL", "SYNTAX"),
                  c("exact_age", "age2", "male", "mom_college"),
                  data = .) %>%
  na.omit()

png("e_nodx_FA_scores_resid.png", width = 5.5, height = 3.74, units = "in",
    res = 300)

ggplot(enodx_FAscores_r, aes(x = LEXICAL, y = SYNTAX, color = exact_age)) +
  geom_line(aes(group = data_id), alpha = 0.25,
            arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  scale_color_viridis() +
  theme_minimal()

dev.off()

enodx_FAsr_sol <- enodx_FAscores_r %>%
  umx_residualize("SYNTAX", "LEXICAL", data = .) %>%
  mutate(
    syn_over_lex = SYNTAX
  )

png("e_nodx_FA_scores_resid_sol_age.png", width = 5.5, height = 3.74,
    units = "in", res = 300)

ggplot(enodx_FAsr_sol, aes(x = exact_age, y = syn_over_lex)) +
  geom_line(aes(group = data_id), alpha = 0.2, size = 1) +
  theme_minimal()

dev.off()

###############################################################################

scatterplot3d(x = enodx_FA_scores$exact_age, y = enodx_FA_scores$LEXICAL,
              z = enodx_FA_scores$SYNTAX, type = "l", angle = 50)
