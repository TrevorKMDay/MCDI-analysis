if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(readr)
library(dplyr)

source("../mcdi-setup.R")

#
# Convert actual data from CSV to RDS to load faster
#

sentences_file <- most_recent("Wordbank-WS-[0-9]*.csv", directory = data_dir)
gestures_file  <- most_recent("Wordbank-WG-[0-9]*.csv", directory = data_dir)

for (csv_file in c(gestures_file, sentences_file)) {

  rds_file <- gsub(".csv", ".rds", csv_file)

  if ( !file.exists(rds_file) ) {

    data <- read_csv(csv_file,
                     col_types = cols(.default = "f", age = "i"))
    write_rds(data, path = rds_file)

  }

}

#
# Extract demographics and save to RDS
#

WS <- readRDS(gsub("csv", "rds", sentences_file))
WG <- readRDS(gsub("csv", "rds", gestures_file))

# Get additional demographics
library(wordbankr)

ws.addtl <- get_administration_data("English (American)", "WS") %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  dplyr::select(data_id, age, sex, mom_ed, birth_order, ethnicity)

wg.addtl <- get_administration_data("English (American)", "WG")%>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  dplyr::select(data_id, age, sex, mom_ed, birth_order, ethnicity)

WS.demo <- WS %>%
  dplyr::select(data_id, age, sex, mom_ed) %>%
  distinct() %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  left_join(ws.addtl) %>%
  add_column(
    instrument = "WS"
  )

WG.demo <- WG %>%
  dplyr::select(data_id, age, sex, mom_ed) %>%
  distinct() %>%
  mutate(
    data_id = as.character(data_id)
  ) %>%
  left_join(wg.addtl) %>%
  add_column(
    instrument = "WG"
  )

save_data(WS.demo, "Wordbank/WS-demographics.rds")
save_data(WG.demo, "Wordbank/WG-demographics.rds")

s_dict <- WS %>%
  filter(type == "word") %>%
  select(category, definition) %>%
  distinct()

g_dict <- WG %>%
  filter(type == "word") %>%
  select(category, definition) %>%
  distinct()

write_csv(s_dict, .data("other/s_dict.csv"))
write_csv(g_dict, .data("other/g_dict.csv"))


# wg.mom_ed <- table(WG.demo$mom_ed, useNA = "always")
# ws.mom_ed <- table(WS.demo$mom_ed, useNA = "always")
#
# wg.me <- as.list(wg.mom_ed, sorted = TRUE)
# names(wg.me) <- gsub(" ", "", names(wg.me))
# names(wg.me)[length(names(wg.me))] <- ".NA"
#
# ws.me <- as.list(ws.mom_ed, sorted = TRUE)
# names(ws.me) <- gsub(" ", "", names(ws.me))
# names(ws.me)[length(names(ws.me))] <- ".NA"
#
# # Table, sorted by education level
# me.tbl <- wg.me %>%
#   as_tibble() %>%
#   rbind(ws.me) %>%
#   add_column(form = c("WG", "WS"), .before = 1) %>%
#   select(form, Primary, SomeSecondary, Secondary, SomeCollege,
#          College, SomeGraduate, Graduate, .NA)
#
# mom_ed.test <- select(me.tbl, -form, -.NA) %>% t() %>%
#   chisq.test(simulate.p.value = TRUE)
#
# sum.wg.me <- sum(me.tbl[1,-1])
# sum.ws.me <- sum(me.tbl[2,-1])
#
# mom_ed <- me.tbl %>%
#   mutate_if(is.integer, as.numeric)
#
# mom_ed[1, 2:9] <- mom_ed[1, -1] / sum.wg.me
# mom_ed[2, 2:9] <- mom_ed[2, -1] / sum.ws.me
#
# mom_ed <- mom_ed %>%
#   select(-.NA) %>%
#   rename(a = Primary, b = SomeSecondary, c = Secondary, d = SomeCollege,
#          e = College, f = SomeGraduate, g = Graduate) %>%
#   pivot_longer(cols = -form)
#
# population <- cbind(letters[1:7], NA) %>%
#   as_tibble() %>%
#   rename(level = V1, percent = V2) %>%
#   mutate(percent = c(NA, NA, 28.52, 16.12, 45.16 - 13.04, NA, 13.04))
#
# ggplot(data = NULL, aes(x = name, y = value * 100)) +
#   geom_histogram(data = mom_ed, stat = "identity", aes(fill = form),
#                  position = "dodge") +
#   scale_x_discrete(labels = c("Primary", "SomeSecondary", "Secondary",
#                               "SomeCollege", "College", "SomeGraduate",
#                               "Graduate")) +
#   labs(x = "Education Level", y = "% of sample", fill = "Form") +
#   geom_point(data = population, aes(x = level, y = percent))
#
# png("plots/mom_ed_presentation.png", width = 5.5, height = 5, units = "in",
#     res = 96) ; print(mom_ed.plot) ; dev.off()

