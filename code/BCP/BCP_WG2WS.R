path <- "/Research/MCDI/MCDI-analysis/code/BCP"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)
source("../mcdi-setup.R")
source("../Wordbank/wordbank-functions.R")

# Load data ====

s_dict <- read_data("other/s_dict.csv")

wg_data <- read_data("BCP/BCP_WG_scored-220802.rds")$n %>%
  mutate(
    data_id = str_pad(data_id, 6, "left", "0"),
    age = as.numeric(age),
    in_range = age <= 18,
    name = "WG",
    inst = "WG"
  ) %>%
  select(data_id, ideal_age, age, in_range, everything(), -LEXICAL, -SYNTAX)

wg_data <- wg_data %>%
  mutate(
    total = select(., -data_id, -ideal_age, -age, -in_range, -name, -inst) %>%
      rowSums()
  )

table(wg_data$in_range, wg_data$total > 250)

ws_data <- read_data("BCP/BCP_WS_scored-220802.rds")$n %>%
  select(data_id, ideal_age, age, TOTAL) %>%
  mutate(
    data_id = as.character(data_id),
    name = "WS",
    inst = "WS"
  )

models <- readRDS("../Wordbank/total_WG_to_WS_cubic.rds")

WB_WG <- read_data("Wordbank/WG-scored-230214.rds") %>%
  select(data_id, age, form, TOTAL)

WB_WS <- read_data("Wordbank/WS-scored-230214.rds") %>%
  select(data_id, age, form, TOTAL)

WB <- bind_rows(WB_WG, WB_WS) %>%
  group_by(age, form) %>%
  summarize(
    mean = mean(TOTAL)
  )

rm(WB_WG, WB_WS)

# Calculate out-of-range

bind_rows(wg_data, ws_data) %>%
  select(inst, age) %>%
  mutate(
    status = case_when(
      inst == "WG" & age <  8 ~ "too young",
      inst == "WG" & age > 18 ~ "too old",
      inst == "WS" & age < 16 ~ "too young",
      inst == "WS" & age > 30 ~ "too old",
      TRUE ~ "in range"
    )
  ) %>%
  group_by(inst, status) %>%
  summarize(
    max_age = max(age),
    n = n()
  )

# Adjust scores ====

wg_outofrange <- wg_data %>%
  filter(
    !in_range
  ) %>%
  mutate(
    total_WG = select(., any_of(s_dict$category)) %>%
      rowSums(),
  )

wg_outofrange$WS <- unname(predict(models, newdata = wg_outofrange))

wg_old_compare <- wg_outofrange %>%
  select(data_id, age, total_WG, WS) %>%
  pivot_longer(c(total_WG, WS), values_to = "TOTAL") %>%
  mutate(
    inst = "WG",
    name = replace(name, name == "WS", "WS_hat") %>%
      replace(., name == "total_WG", "WG")
  )

wg_inrange <- wg_data %>%
  filter(
    in_range
  ) %>%
  mutate(
    TOTAL = select(., any_of(s_dict$category)) %>%
      rowSums(),
    inst = "WG",
    name = "WG"
  ) %>%
  select(data_id, ideal_age, age, inst, name, TOTAL)

# Plot findings ====

all_data <- bind_rows(wg_inrange, wg_old_compare, ws_data)

bcp_noadj <- all_data %>%
  filter(
    name != "WS_hat"
  ) %>%
  mutate(
    age = round(age)
  ) %>%
  group_by(age) %>%
  summarize(
    mean = mean(TOTAL)
  )

ggplot(all_data, aes(x = age, y = TOTAL, color = name, linetype = inst)) +
  geom_line(data = WB, aes(x = age, y = mean, linetype = form),
            color = "black", size = 1) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.25) +
  geom_line(data = bcp_noadj, aes(x = age, y = mean), linetype = "solid",
            color = "firebrick", size = 1) +
  geom_vline(xintercept = c(16, 18), color = "red") +
  scale_linetype_manual(values = c("twodash", "solid")) +
  theme_minimal()

# ITEM LEVEL ====

## Load functions ====

cwd <- getwd()
source("../WG2WS/wordbank-WG2WS-funcs.R")
setwd(cwd)

## Adjust item level ====

shared_words <- read_data("other/sharedwords.csv") %>%
  rename(
    item_definition = definition
  )
shared_words %>%
  filter(
    g.cat != s.cat
  )

g_dict <- read_data("other/g_dict.csv")

wg_items <- read_delim(.data("BCP/mcdi_loris_data_WG_all_ages_item_level.txt"),
                       delim = "\t", show_col_types = FALSE) %>%
  select(CandID, Visit_label, Candidate_Age, starts_with("I_D_")) %>%
  filter(
    str_detect(Visit_label, "^bcp")
  ) %>%
  pivot_longer(starts_with("I_D_"), names_to = "label") %>%
  group_by(CandID, Visit_label) %>%
  mutate(
    CandID = str_pad(CandID, 6, "left", "0"),
    produces = value == "says_and_understands",
    item_id = paste0("item_", row_number() + 33)
  ) %>%
  left_join(g_dict, by = "item_id") %>%
  select(-label, -item_id, -item_kind) %>%
  full_join(shared_words) %>%
  filter(
    !is.na(s.cat)
  )

# Predict WS scores from WG data
wg_as_ws_scores <- wg_items %>%
  group_by(CandID, Visit_label, Candidate_Age, s.cat) %>%
  filter(
    Candidate_Age > 18
  ) %>%
  summarize(
    total_WG = sum(produces)
  ) %>%
  group_by(CandID, Visit_label, Candidate_Age) %>%
  mutate(
    WG_total_score = sum(total_WG)
  ) %>%
  rowwise() %>%
  mutate(
    total_WS = WG_to_WS(s.cat, total_WG, WG_total_score, Candidate_Age,
                         trimmed = TRUE)
  )

wg2ws <- wg_as_ws_scores %>%
  pivot_wider(id_cols = c(CandID, Visit_label, Candidate_Age, WG_total_score),
              names_from = s.cat, values_from = total_WS) %>%
  mutate(
    age_c = Candidate_Age - 18,
    inst = "WG2WS"
  ) %>%
  rename(
    WG_total = WG_total_score
  )

wg2ws$connecting_words <- estimate_cwords(wg2ws)

# Combine all data ====

wg_inrange_wg2ws <- wg_items %>%
  filter(
    Candidate_Age <= 18
  ) %>%
  mutate(
    data_id = paste(CandID, Visit_label, Candidate_Age, sep = "_"),
    item_kind = "word",
    value = if_else(value == "says_and_understands", "produces", value)
  ) %>%
  score.GasS()

wg_inrange_wg2ws_n <- wg_inrange_wg2ws$n %>%
  select(-matches("^[WCTLS]", ignore.case = FALSE)) %>%
  separate(data_id, into = c("data_id", NA, "visit_label", "age"),
           sep = "[_x]") %>%
  mutate(
    ideal_age = as.numeric(str_remove(visit_label, "m")),
    age = as.numeric(age),
    inst = "WGasWS"
  ) %>%
  select(-visit_label)

ws2 <- read_data("BCP/BCP_WS_scored-220802.rds")$n %>%
  select(-starts_with("word_"), -complexity, -TOTAL) %>%
  mutate(
    data_id = str_pad(data_id, 6, "left", "0"),
    inst = "WS"
  )

wg_outofrange2 <- wg2ws %>%
  separate(Visit_label, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    ideal_age = as.numeric(str_remove(visit, "m"))
  ) %>%
  rename(
    data_id = CandID,
    age = Candidate_Age
  ) %>%
  select(-visit, -WG_total, -age_c)

BCP_final <- bind_rows(wg_inrange_wg2ws_n, wg2ws2, ws2) %>%
  select(data_id, inst, ideal_age, age, all_of(unique(s_dict$category))) %>%
  mutate(
    TOTAL = select(., all_of(unique(s_dict$category))) %>%
      rowSums(na.rm = TRUE)
  ) %>%
  filter(
    # Bad data point
    !(data_id == "879509" & ideal_age == 30)
  )

save_data(BCP_final, "BCP/BCP_all_data_rescored.rds")

ggplot(BCP_final, aes(x = age, y = TOTAL, color = inst, fill = inst)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = data_id), alpha = 0.1) +
  geom_smooth()

## Check for drops =====

BCP_final_drops <- BCP_final %>%
  arrange(data_id, ideal_age) %>%
  group_by(data_id) %>%
  mutate(
    cummax = cummax(TOTAL),
    diff = TOTAL - cummax
  )

drops <- BCP_final_drops %>%
  filter(
    diff < 0
  ) %>%
  pull(data_id)

BCP_final_drops <- BCP_final_drops %>%
  filter(
    data_id %in% drops
  )

ggplot(BCP_final_drops, aes(x = age, y = TOTAL)) +
  geom_point(aes(color = inst)) +
  geom_line(aes(group = data_id))
