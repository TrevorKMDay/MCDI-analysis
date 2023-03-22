if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
}

library(tidyverse)
library(tidytext)
select <- dplyr::select

source("../mcdi-setup.R")
source("../Wordbank/wordbank-functions.R")
source("../BCP/format-BCP-funcs.R")
source("../growth_curving/growth-curve-functions.R")

PHE_data <- "PHE/PHE-MCDI-201103.rds"

if (file.exists(.data(PHE_data))) {

  PHE <- read_data(PHE_data)

} else {

  PHE <- read_data("PHE/BCPPHE-MCDI.csv") %>%
    select_all(~gsub(",", ".", .)) %>%
    select_all(~gsub("demographics.", "demo.", .)) %>%
    select_all(~gsub("mcdi[.]", "wg.", .)) %>%
    select_all(~gsub("mcdi_words_sentences.", "ws.", .)) %>%
    filter(
      demo.Site == "PHE"
    )

  save_data(PHE, "PHE/PHE-MCDI-201103.rds")

}

ages <- select(PHE, demo.CandID, wg.Candidate_Age) %>%
  arrange(demo.CandID) %>%
  mutate(
    age = round(as.numeric(wg.Candidate_Age), 0)
  ) %>%
  select(-wg.Candidate_Age)

# How many MLU entries? =====

MLU <- replace(PHE$ws.II_D_1, PHE$ws.II_D_1 == ".", NA)
sum(!is.na(MLU))

# 310 * 3 = 930 sentences

## WS ====

# Extract LORIS data for words and sentences
WS_orig <- PHE %>%
  select(starts_with("demo."), starts_with("ws.")) %>%
  filter(
    ws.Administration == "All"
  ) %>%
  mutate(
    ws.Candidate_Age = as.numeric(ws.Candidate_Age)
  )

range(WS_orig$ws.Candidate_Age)

# The Phenoscreening kids who got WS also got some WG supplemental section,
# these are those kids.
WS_WG_addtl <- PHE %>%
  select(starts_with("demo."), ws.Administration, starts_with("wg.")) %>%
  filter(
    ws.Administration == "All",
    wg.Administration == "All"
  ) %>%
  mutate(
    wg.Candidate_Age = as.numeric(wg.Candidate_Age)
  )

# Format in Wordbank format
WS_as_WB <- WS_orig %>%
  rename(
    data_id = demo.CandID,
    sex = demo.Sex
  ) %>%
  select(-starts_with("demo")) %>%
  select_all(~gsub("ws.", "sent.", .)) %>%
  format.sentences(., .data("Wordbank/WS-example.csv"))

WS <- score.WS(WS_as_WB, include.totals = TRUE)$n %>%
  mutate(
    inst = "WS"
  )

## WG ====

WG_orig <- PHE %>%
  select(starts_with("demo."), starts_with("wg."), ws.Administration) %>%
  filter(
    wg.Administration == "All" &
    ws.Administration != "All"
  ) %>%
  mutate(
    wg.Candidate_Age = as.numeric(wg.Candidate_Age)
  ) %>%
  arrange(demo.CandID) %>%
  select(-ws.Administration) %>%
  arrange(desc(wg.Candidate_Age))

WG_orig$demo.CandID %in% WS_orig$demo.CandID

WG_as_WB <- WG_orig %>%
  rename(
    data_id = demo.CandID,
    sex = demo.Sex,
  ) %>%
  select(-starts_with("demo")) %>%
  select_all(~gsub("wg.", "gest.", .)) %>%
  format.gestures(., .data("Wordbank/WG-example.csv"))

WG <- score.WG(WG_as_WB)

WG_as_WS <- score.GasS(WG_as_WB)$n %>%
  mutate(
    inst = "WGasWS"
  )

## Boxplot ===

all_data <- bind_rows(WS, WG_as_WS) %>%
  left_join(ages, by = c("data_id" = "demo.CandID")) %>%
  arrange(data_id, age) %>%
  select(data_id, age, inst, everything())

ggplot(all_data, aes(x = age, y = TOTAL, color = inst)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = data_id), alpha = 0.1, color = "black") +
  geom_smooth() +
  scale_x_continuous(limits = c(NA, 36)) +
  theme_minimal()

################################################################################


all.n <- bind_rows(WS$n, WG_as_WS$n) %>%
  mutate(
    INVENTORY = select(., -data_id, -age, -matches("^[WCLS]")) %>%
                  rowSums()
  )
all.p <- bind_rows(WS$p, WG_as_WS$p)

png("PHE_trends.png", width = 4, height = 3.64, units = "in", res = 300)

ggplot(all.n, aes(x = age, y = INVENTORY)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = data_id), alpha = 0.2) +
  scale_x_continuous(limits = c(5, NA))

dev.off()

ggplot(all.n, aes(x = LEXICAL, y = SYNTAX)) +
  geom_point(aes(color = age < 25)) +
  geom_line(aes(group = data_id), alpha = 0.2)

# Age groups

all.n <- all.n %>%
  mutate(
    age.bin = round(age),
  ) %>%
  arrange(data_id, age.bin)

age.groups <- all.n %>%
  group_by(data_id) %>%
  dplyr::summarize(
    n = n(),
    min = min(age.bin),
    max = max(age.bin)
  ) %>%
  filter(n > 1) %>%
  arrange(n, min, max)

filter(all.n, data_id %in% age.groups$data_id) %>%
ggplot(aes(x = age, y = LEXICAL)) +
  geom_point(aes(color = age < 25)) +
  geom_line(aes(group = data_id), alpha = 0.2)

subjects <- unique(age.groups$data_id)

################################################################################


maxes <- c(566, 114)

dat <- all.n %>%
  mutate(
    LEXICAL_perc = LEXICAL / maxes[[1]],
    SYNTAX_perc = SYNTAX / maxes[[2]]
  ) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate( ) %>%
  filter(
    n_visits > 1
  ) %>%
  mutate(
    # Calculate fits
    l.fits = map(data, ~ gomp2.fit(.x, response_var = "LEXICAL_perc", max = 1)),
    s.fits = map(data, ~ gomp2.fit(.x, response_var = "SYNTAX_perc", max = 1)),
    # Extract kg value from each model
    l.kg = extract.kg(l.fits),
    s.kg = extract.kg(s.fits),
    # Calculate gplot object for each kg
    l.curves = calculate.curves(l.kg, max = 1, color = "red"),
    s.curves = calculate.curves(s.kg, max = 1, color = "blue")
  )

# better with alpha < .2
# plot.curves(dat$l.curves, y_limits = c(0, maxes[1] * 1.1), x_limits = c(0, 45),
#             max_val = maxes[1])
#
# plot.curves(dat$s.curves, y_limits = c(0, maxes[2] * 1.1), x_limits = c(0, 45),
#             max_val = maxes[2])

dat <- dat %>%
  mutate(
    solo_l_plot = map(data,
                      ~ ggplot(.x, aes(x = age, y = LEXICAL_perc)) +
                        geom_point() +
                        scale_x_continuous(limits = c(0, 42)) +
                        scale_y_continuous(limits = c(0, 1)) +
                        l.curves ),
    solo_s_plot = map(data,
                      ~ ggplot(.x, aes(x = age, y = SYNTAX_perc)) +
                        geom_point() +
                        scale_x_continuous(limits = c(0, 42)) +
                        scale_y_continuous(limits = c(0, 1)) +
                        s.curves ),
    data_long = map(data,
                    ~ .x %>%
                      mutate(
                        lex_p = LEXICAL / maxes[1],
                        syn_p = SYNTAX / maxes[2]
                      ) %>%
                      select(age, lex_p, syn_p) %>%
                      pivot_longer(-age)),
    joint_plot = map(data_long,
                     ~ ggplot(.x, aes(x = age, y = value, color = name)) +
                       scale_x_continuous(limits = c(0, 42))+
                       scale_y_continuous(limits = 0:1) +
                       geom_point() +
                       labs(title = data_id) +
                       l.curves +
                       s.curves)
  )

library(patchwork)

for (i in dat$joint_plot) {
  print(i)
}

age.groups %>%
  select(-n) %>%
  pivot_longer(-data_id) %>%
  ggplot(aes(x = value, fill = name)) +
    geom_density(alpha = 0.5) +
    labs(x = "Age (mo)") +
    theme(legend.position = "none")
