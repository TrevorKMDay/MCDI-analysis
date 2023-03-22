path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)

summarize <- dplyr::summarize

source("../mcdi-setup.R")
source("../growth_curving/growth-curve-functions.R")

##### Load data ####

eirli <- read_data("EIRLI/EIRLI_clean.csv")[, 14:33] %>%
  select(-COMPLEXITY)

# range(rowSums(eirli))

BE <- read_data("LCA/BplusE.rds") %>%
  filter(
    proj == "BCP"
  ) %>%
  ungroup()

range(BE$inventory_total)

WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

wb_wg <- read_data("Wordbank/WG-scored.rds")$n
wb_ws <- read_data("Wordbank/WS-scored.rds")$n

wb <- bind_rows(wb_wg, wb_ws) %>%
  select(data_id, age, all_of(colnames(eirli))) %>%
  mutate(
    inventory_total = select(., -data_id, -age) %>%
      rowSums(na.rm = TRUE),
    status = "WB"
  )

rm(wb_wg, wb_ws, WS_1A)

# Gompertz modeling ####

bcp_curve <- gomp2.fit(BE, response_var = "inventory_total")
wb_curve <- gomp2.fit(wb, response_var = "inventory_total")

max_value <- read_data("other/s_dict.csv") %>%
  group_by(category) %>%
  nest() %>%
  filter(
    category %in% colnames(BE)
  ) %>%
  mutate(
    n = map_dbl(data, nrow)
  )

## Check WB validity #####

max_inv_total = sum(max_value$n) + 1

kgs <- extract.kg(list(bcp_curve, wb_curve))
curves <- calculate.curves(kgs, max_inv_total)

png("plots/bcp_adjust/mean-trajectory.png", width = 8, height = 6,
    units = "in", res = 72)

plot.curves(curves, max_inv_total, x_limits = c(10, 60),
            y_limits = c(0, max_inv_total))

dev.off()

bcp_to_wb <- tibble(
    age = 11:38
  ) %>%
  mutate(
    bcp = solve.gomp2(x = c(11:38), k_g = kgs[1]),
    wb  = solve.gomp2(x = c(11:38), k_g = kgs[2]),
    diff = bcp - wb
  )

# Saves conversion to EIRLI norms (i.e. w/o missing categories)
save_data(bcp_to_wb, "LCA/BCP_to_EIRLI.rds")

# Trends #####

eirli_cat_avg <- read_data("EIRLI/EIRLI_clean.csv") %>%
  filter(
    # No DX group
    follow_up,
    !dx
  ) %>%
  select(-gender, -follow_up, -contains("dx"), -contains("_ed"),
         -COMPLEXITY) %>%
  pivot_longer(-c(data_id, age, exact_age), names_to = "category") %>%
  group_by(age, category) %>%
  summarize(
    eirli_nodx_mean = mean(value)
  )

bcp_cat_avg <- BE %>%
  select(data_id, exact_age, all_of(colnames(eirli))) %>%
  mutate(
    age = round(exact_age)
  ) %>%
  pivot_longer(-c(data_id, age, exact_age), names_to = "category") %>%
  group_by(age, category) %>%
  summarize(
    bcp_n = n(),
    bcp_mean = mean(value)
  )

wb_cat_avg <- wb %>%
  pivot_longer(-c(data_id, age, status), names_to = "category") %>%
  group_by(age, category) %>%
  dplyr::summarize(
    wb_mean = mean(value)
  )

cat_avg <- left_join(wb_cat_avg, eirli_cat_avg, by = c("age", "category"))  %>%
  left_join(bcp_cat_avg, by = c("age", "category")) %>%
  filter(
    !is.na(wb_mean)
  ) %>%
  pivot_longer(ends_with("mean"), names_to = "group") %>%
  filter(
    category != "inventory_total"
  ) %>%
  na.omit() %>%
  mutate(
    bcp_n = if_else(group == "bcp_mean", bcp_n, NA_integer_)
  )

dir.create("plots/bcp_adjust", showWarnings = FALSE, recursive = TRUE)
png("plots/bcp_adjust/comparison-facet.png", width = 8, height = 6,
    units = "in", res = 72)

ggplot(cat_avg, aes(x = age, value, color = group)) +
  geom_point(aes(size = bcp_n)) +
  geom_line() +
  facet_wrap(vars(category), scales = "free_y") +
  scale_x_continuous(limits = c(11, 30), breaks = seq(10, 30, by = 2)) +
  theme_bw() +
  labs(x = "Age (mo.)", y = "# words", color = "Group",
       size = "BCP sample size") +
  theme(legend.position = "bottom")

dev.off()
