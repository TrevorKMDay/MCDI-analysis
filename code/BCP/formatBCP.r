if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/BCP")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/BCP")
}

# Libraries

library(readxl)
library(Hmisc)
library(tidyverse)
library(ggExtra)

summarise <- dplyr::summarise
rename <- dplyr::rename

# Helper functions

source("../Wordbank/wordbank-functions.R")
source("format-BCP-funcs.R")
source("../mcdi-setup.R")

# Read data

mcdi.date <- "200609"

mcdi_all1 <- read_csv(.data(paste0("BCP/bcp-UMNUNC-mcdi-", mcdi.date, ".csv")))
s_dict_file <- .data("other/s_dict.csv")
g_dict_file <- .data("other/g_dict.csv")

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
    age = demo.ideal_age,
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
  )

write_csv(age_lut, .data("BCP/mcdi-age-lut.csv"))

################################################################################
# BCP analysis
################################################################################

# Words and Sentences

# Format BCP data as Wordbank
BCP_WS <- mcdi_all %>%
  select(-starts_with("gest"), -ends_with("morphemes"), -ends_with("words")) %>%
  format.sentences(., s_dict_file)

# Score WS based on Wordbank
BCP_WS_scored <- score.WS(BCP_WS)

BCP_WS_scored[[1]] <- BCP_WS_scored[[1]] %>%
  mutate(
    TOTAL = select(., -data_id, -age, -matches("^[WCLS]")) %>%
              rowSums()
  )

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

# Words and Gestures
BCP_WG <- format.gestures(mcdi_all, g_dict_file, inventory.only = TRUE)

BCP_WG_scored <- score.WG(BCP_WG)

BCP_WG_asWS <- score.GasS(BCP_WG)[[1]] %>%
  mutate(
    TOTAL = select(., -data_id, -age, -matches("^[WCLS]")) %>%
      rowSums()
  )

BCP_both <- bind_rows(BCP_WS_scored[[1]], BCP_WG_asWS)

#
# Save data
#

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
