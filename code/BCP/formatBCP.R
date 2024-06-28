# Setup ======

path <- "/Research/MCDI/MCDI-analysis/code/BCP"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

# Libraries ====

library(readxl)
library(Hmisc)
library(tidyverse)
library(ggExtra)

summarise <- dplyr::summarise
rename <- dplyr::rename

# Helper functions =====

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")

# Read data =====

# mcdi.date <- "200609"
mcdi.date <- "220802"

mcdi_all1_old <- read_csv(.data("BCP/bcp-UMNUNC-mcdi-200609.csv"))

sent_all1 <- read_data("BCP/bcp-UMNUNC-mcdiS-220802.csv")
gest_all1 <- read_data("BCP/bcp-UMNUNC-mcdiG-220802.csv")

mcdi_all1 <- left_join(sent_all1, gest_all1)

s_dict_file <- .data("other/s_dict.csv")
g_dict_file <- .data("other/g_dict.csv")

s_dict <- read_csv(s_dict_file)
WS_1A <- sort(unique(s_dict$category))

# Clean up mcdi_all
colnames(mcdi_all1) <- gsub("demographics,", "demo.", colnames(mcdi_all1))
colnames(mcdi_all1) <- gsub("mcdi,", "gest.", colnames(mcdi_all1))
colnames(mcdi_all1) <- gsub("mcdi_words_sentences,", "sent.",
                            colnames(mcdi_all1))

# Note that "Gender" was changed to "Sex" in spring 2020, may not work with
# older datasets

if ("demo.Gender" %in% colnames(mcdi_all1)) {
  .sex_column <- "demo.Gender"
} else if ("demo.Sex" %in% colnames(mcdi_all1)) {
  .sex_column <- "demo.Sex"
} else {
  stop("No sex/gender column in data")
}

mcdi_all <- mcdi_all1 %>%
  select(demo.CandID, demo.Visit_label, all_of(.sex_column),
         starts_with("gest."), starts_with("sent.")) %>%
  filter(
    grepl("^bcp[ABCDEG]", demo.Visit_label),
    !(demo.Visit_label %in% c("bcpCFP", "bcpGUESTxV1")),
    !grepl("Biomom", demo.Visit_label)
  ) %>%
  separate(demo.Visit_label,
           into = c(NA, "demo.ideal_age"),
           sep = "x") %>%
  mutate(
    demo.ideal_age = as.numeric(gsub("m", "", demo.ideal_age))
  ) %>%
  dplyr::rename(
    data_id = demo.CandID,
    ideal_age = demo.ideal_age,
    sex = .sex_column
  ) %>%
  mutate_at(vars(ends_with("morphemes")), as.numeric) %>%
  mutate_at(vars(ends_with("words")), as.numeric) %>%
  mutate(sent.Candidate_Age = as.numeric(sent.Candidate_Age))

age_lut <- mcdi_all %>%
  select(data_id, contains("age")) %>%
  dplyr::rename(
    ws_age = sent.Candidate_Age,
    wg_age = gest.Candidate_Age
  ) %>%
  mutate(
    across(c(wg_age, ws_age), ~replace(., . == ".", NA) %>%
                                  as.numeric())
  ) %>%
  filter(
    !(is.na(wg_age) & is.na(ws_age))
  )

# s879509 <- mcdi_all1 %>%
#   filter(
#     demo.CandID == 544404
#   ) %>%
#   select(demo.CandID, demo.Visit_label, ends_with("Candidate_Age"),
#          contains("Date_taken"))

write_csv(age_lut, .data("BCP/mcdi-age-lut.csv"))

# BCP analysis ====

## Words and Sentences ====

# Format BCP data as Wordbank
BCP_WS <- mcdi_all %>%
  select(-starts_with("gest"), -ends_with("morphemes"), -ends_with("words")) %>%
  format.sentences(s_dict_file)

# Score WS based on Wordbank
BCP_WS_scored <- score.WS(BCP_WS)

BCP_WS_scored[[1]] <- BCP_WS_scored[[1]] %>%
  mutate(
    TOTAL = select(., all_of(WS_1A)) %>%
              rowSums()
  ) %>%
  left_join(select(age_lut, data_id, ideal_age, ws_age),
            by = c("data_id", "age" = "ws_age")) %>%
  select(data_id, ideal_age, age, everything())

BCP_WS_scored[[2]] <- BCP_WS_scored[[2]] %>%
  left_join(select(age_lut, data_id, ideal_age, ws_age),
            by = c("data_id", "age" = "ws_age")) %>%
  select(data_id, ideal_age, age, everything())

# Calculate MLU3
MLU3 <- mcdi_all %>%
  select(data_id, sent.Candidate_Age, ends_with("morphemes"),
         ends_with("words")) %>%
  rename(age = sent.Candidate_Age) %>%
  na.omit() %>%
  mutate(
    age = as.numeric(age),
    MLU3m = rowMeans(select(., ends_with("morphemes")), na.rm = TRUE),
    MLU3w = rowMeans(select(., ends_with("words")), na.rm = TRUE)
  )

ggplot(MLU3, aes(x = MLU3w, y = MLU3m, color = age)) +
  geom_point() +
  geom_line(aes(group = data_id), alpha = 0.25) +
  scale_color_viridis_c()

cor(MLU3$MLU3m, MLU3$MLU3w) # 0.99

ggplot(BCP_WS_scored[[1]], aes(x = age, y = TOTAL)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess") +
  geom_line(aes(group = data_id), alpha = 0.25) +
  labs(x = "Age (mo.)", y = "Inventory size")

## Words and Gestures ====

BCP_WG <- format.gestures(mcdi_all, g_dict_file, inventory.only = TRUE)

BCP_WG_scored <- score.WG(BCP_WG, produces_value = "produces")

for (i in 1:2) {

  BCP_WG_scored[[i]] <- BCP_WG_scored[[i]] %>%
    left_join(select(age_lut, -ws_age),
              by = c("data_id", "age" = "wg_age")) %>%
    select(data_id, ideal_age, age, everything())

}

BCP_WG_asWS <- score.GasS(BCP_WG)[[1]] %>%
  mutate(
    age = as.numeric(age),
    TOTAL = select(., all_of(WS_1A)) %>%
      rowSums()
  ) %>%
  left_join(
    select(age_lut, -ws_age),
    by = c("data_id", "age" = "wg_age")
  ) %>%
  select(data_id, ideal_age, age, everything())

BCP_both <- bind_rows(BCP_WS_scored[[1]], BCP_WG_asWS)

ggplot(BCP_both, aes(x = age, y = TOTAL)) +
  geom_point() +
  geom_line(aes(group = as.factor(data_id))) +
  theme_bw()

BCP_bad <- BCP_both %>%
  arrange(data_id, age) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    max  = map_dbl(data, ~max(.x$TOTAL)),
    last = map_dbl(data, ~.x$TOTAL[nrow(.x)])
  ) %>%
  filter(
    last < max
  )

ggplot(unnest(BCP_bad, "data"), aes(x = age, y = TOTAL)) +
  geom_point() +
  geom_line(aes(color = as.factor(data_id))) +
  scale_y_continuous(limits = c(0, 680), breaks = seq(0, 680, by = 68)) +
  scale_x_continuous(limits = c(12, 36), breaks = seq(10, 40, 2)) +
  theme_bw()

# Write out ====

suffix <- paste0(mcdi.date, ".rds")
saveRDS(BCP_WG_scored, file = .data(paste0("BCP/BCP_WG_scored-", suffix)))
saveRDS(BCP_WG_asWS,   file = .data(paste0("BCP/BCP_WG_asWS-",   suffix)))
saveRDS(BCP_WS_scored, file = .data(paste0("BCP/BCP_WS_scored-", suffix)))
saveRDS(MLU3,          file = .data(paste0("BCP/BCP_WS_MLU3-",   suffix)))

# People who have four or more data points

multiple4 <- BCP_both %>%
  group_by(data_id) %>%
  summarise(
    n = n(),
    max = max(TOTAL)
  ) %>%
  filter(
    n >= 4,
    max >= 680 / exp(1) / 2
  )

test <- BCP_both %>%
  filter(data_id %in% multiple4$data_id) %>%
  select(data_id, age, LEXICAL, SYNTAX, TOTAL)

write_csv(test, .data("BCP/test-subjs-4.csv"))

multiple3 <- BCP_both %>%
  group_by(data_id) %>%
  summarise(
    n = n(),
    max = max(TOTAL)
  ) %>%
  filter(
    n >= 3,
    max >= 680 / exp(1) / 2
  )

test3 <- BCP_both %>%
  filter(
    data_id %in% multiple3$data_id,
    ) %>%
  select(data_id, age, LEXICAL, SYNTAX, TOTAL)

write_csv(test, .data("BCP/test-subjs-3.csv"))

################################################################################

mcdi_index <- mcdi_all %>%
  filter(
    gest.Administration == "All" | sent.Administration == "All"
  ) %>%
  select(data_id, age) %>%
  arrange(data_id) %>%
  group_by(data_id) %>%
  summarise(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  ) %>%
  arrange(
    min_age, max_age, data_id
  ) %>%
  mutate(
    index = row_number()
  )

spag_plot <- mcdi_all %>%
  filter(
    gest.Administration == "All" | sent.Administration == "All"
  ) %>%
  select(data_id, age, sex) %>%
  filter(
    !is.na(age)
  ) %>%
  left_join(mcdi_index) %>%
  arrange(index) %>%
  na.omit()

spaghetti <- ggplot(spag_plot, aes(x = age, y = index, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = index), alpha = 0.5) +
  scale_y_discrete(labels = NULL) +
  labs(x = "Age (mo)", y = "Individual") +
  theme_bw()

png("marginal_spaghetti_plot.png", width = 6, height = 3, units = "in",
    res = 300)

ggMarginal(spaghetti, type = "histogram", margins = "x")

dev.off()

################################################################################

# Number of forms

n_forms <- spag_plot %>%
  group_by(data_id) %>%
  summarise(
    n_forms = n()
  ) %>%
  group_by(n_forms) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n_forms)) %>%
  mutate(
    cumsum = cumsum(n)
  ) %>%
  arrange(n_forms)
