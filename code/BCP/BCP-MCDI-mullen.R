if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Libraries

library(tidyverse)
library(tidyselect)
library(readxl)
library(Hmisc)
library(corrplot)
library(viridis)
library(nlme)

# Helper functions

source("wordbank-functions.R")
source("format-BCP-funcs.R")

names <- read_csv("data/BCP_WG_asWS.rds")

# Load data

BCP_WG_scored <- readRDS("data/BCP_WG_scored.rds")
BCP_WS_scored <- readRDS("data/BCP_WS_scored.rds")

# Mullen

mullen <- read_csv("data/bcp-UMN-mullen-191030.csv") %>%
            select_all(~gsub("^.*,", "", .)) %>%
            separate(Visit_label, into = c("x", "age"), sep = "x") %>%
            mutate(age = as.numeric(gsub("m", "", age))) %>%
            select(-x, -Identifiers, -Cohort, -Current_stage, -Gender) %>%
            filter(!is.na(age)) %>%
            mutate_at(vars(contains("language")), as.numeric) %>%
            filter(!is.na(expressive_language_t)) %>%
            rename_all(
              funs(gsub("expressive", "e", .) %>%
                    gsub("receptive", "r", .) %>%
                    gsub("language", "lg", .) %>%
                    gsub("age_equivalent", "age_equiv", .) %>%
                    gsub("percentile", "perc", .))
            ) %>%
            rename(data_id = CandID)

#
# Correlelogram not very helpful
#

join <- merge(BCP_WG_scored[[2]], mullen, by = c("data_id", "age"))

join.corr <- cor(select(join, -data_id, -age, -ends_with("age_equiv"),
                        -ends_with("perc"), -ends_with("t")),
                 use = "complete.obs")

corrplot(join.corr)

#
#
#

BCP_scored.n <- bind_rows(BCP_WG_scored[[1]], BCP_WS_scored[[1]])

BCP_scored.n$total <- rowSums(select(BCP_scored.n, -data_id, -age),
                              na.rm = TRUE)

join.n <- merge(select(BCP_scored.n, data_id, age, total),
                select(mullen, data_id, age, ends_with("raw")),
                by = c("data_id", "age")) %>%
          pivot_longer(-c(data_id, age, total))

# Replace 0s with approximation for log transformation
join.n$total.log <- join.n$total
join.n$total.log[join.n$total.log == 0] <- NA
join.n$total.log <- log10(join.n$total.log)

ggplot(join.n, aes(x = total.log, y = value, color = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(linetype = name)) +
  labs(x = "MCDI inventory size (log)", y = "Mullen score (raw)") +
  scale_color_viridis()

ggplot(join.n, aes(x = total, y = value, color = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(linetype = name)) +
  labs(x = "MCDI inventory size", y = "Mullen score (raw)")+
  scale_color_viridis()

ggplot(join.n, aes(x = age, y = value, color = total)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(linetype = name)) +
  labs(x = "Age (mo)", y = "Mullen score (raw)")+
  scale_color_viridis()

ggplot(join.n, aes(x = age, y = value, color = total.log)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(linetype = name)) +
  labs(x = "Age (mo)", y = "Mullen score (raw)")+
  scale_color_viridis()

lm(mullen$e_lg_raw ~ mullen$r_lg_raw) %>% summary()
# Correlation between e and r lg raw is 0.88

lme(value ~ total.log + age,
    data = filter(join.n, name == "e_lg_raw"),
    random = ~ 1 | data_id,
    na.action = na.omit) %>% summary()
# B = .03273, R2 = .78, RSE 3.592 on 327 DoF

lme(value ~ total.log + age,
    data = filter(join.n, name == "r_lg_raw"),
    random = ~ 1 | data_id,
    na.action = na.omit) %>% summary()
