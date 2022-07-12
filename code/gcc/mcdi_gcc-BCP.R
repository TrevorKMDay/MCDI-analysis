if (.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(igraph)


##### Load data

ws_items <- read_csv("data/other/s_dict.csv")
cat_order <- unique(ws_items$category)

ws_items2 <- ws_items %>%
  select(-type) %>%
  mutate(
    # This gets the location of the category in the order vector,
    category_index = match(category, cat_order)
  ) %>%
  group_by(category) %>%
  mutate(
    # Group by cat to get order in cat
    item_index     = row_number()
  )

bcp <- read_csv("data/BCP/bcp-UMNUNC-mcdi-200609.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, mcdi_words_sentences.Administration,
         mcdi_words_sentences.Candidate_Age,
         starts_with("mcdi_words_sentences.I_A_")) %>%
  filter(
    mcdi_words_sentences.Administration == "All"
  ) %>%
  select(-mcdi_words_sentences.Administration) %>%
  rename(
    data_id = demographics.CandID,
    age = mcdi_words_sentences.Candidate_Age
  )

bcp2 <- bcp %>%
  pivot_longer(-c(data_id, age)) %>%
  separate(name, into = c(NA, NA, NA, NA, NA, "category_index", "item_index"),
           sep = "[._]") %>%
  mutate(
    age = as.numeric(age),
    across(c(category_index, item_index), as.integer)
  ) %>%
  arrange(data_id, category_index, item_index) %>%
  left_join(ws_items2, by = c("category_index", "item_index")) %>%
  mutate(
    says = if_else(value == "says", TRUE, FALSE, FALSE)
  ) %>%
  select(data_id, age, item_id, category, definition, says)

rm(bcp, ws_items)

##### Calculate graphs

egrid <- function(v) {

  mat <- t(combn(as.character(v), 2))

  colnames(mat) <- c("w1", "w2")

  return(as_tibble(mat))

}

conc_overlap <- readRDS("data/gcc/McRae-overlap.rds")

bcpN <- bcp2 %>%
  group_by(data_id, age) %>%
  filter(
    says
  ) %>%
  nest() %>%
  mutate(
    egrid   = map(data, ~egrid(.x$definition) %>%
                    left_join(conc_overlap, by = c("w1", "w2")) %>%
                    filter(overlap > 0)),
  )

bcp_graph <- bcpN %>%
  mutate(
    n_words = map_int(data, nrow),
    graph = map(egrid, graph_from_data_frame, directed = FALSE),
    gcc   = map_dbl(graph, transitivity)
  )

ggplot(bcp_graph, aes(x = age, y = gcc)) +
  geom_point() +
  geom_line(aes(group = data_id)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(18, 40), breaks = 18:40) +
  theme_bw()

ggplot(bcp_graph, aes(x = n_words, y = gcc)) +
  geom_point() +
  geom_line(aes(group = data_id)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw()

ggplot(bcp_graph, aes(x = n_))
