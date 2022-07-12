if (.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(wordbankr)
library(igraph)

## Get words from wordbank

words <- get_item_data("English (American)", form = "WS") %>%
  filter(
    type == "word"
  ) %>%
  select(item_id, category, definition)

ws_words <- str_remove(words$definition, " [(].*[)]$")

###### Get CONCS

concs <- read_delim("data/gcc/McRae-BRM-InPress/CONCS_FEATS_concstats_brm.txt",
                    delim = "\t")

sum(ws_words %in% concs$Concept)
ws_mcrae_words <- intersect(ws_words, concs$Concept)

concs_pf <- concs %>%
  filter(
    # Remove encyclopedic/taxonomic features (the rest are functional or a
    # subcategory of perceptual)
    !(BR_Label %in% c("encyclopaedic", "taxonomic")),
    Concept %in% ws_words
  )

concs_features <- concs_pf %>%
  select(Concept, BR_Label, Feature) %>%
  group_by(Concept) %>%
  nest()

conc_overlap <- expand_grid(w1 = concs_features$Concept,
                            w2 = concs_features$Concept) %>%
  left_join(concs_features, by = c("w1" = "Concept")) %>%
  left_join(concs_features, by = c("w2" = "Concept"),
            suffix = c(".w1", ".w2")) %>%
  mutate(
    overlap = map2_dbl(data.w1, data.w2, ~sum(.x$Feature %in% .y$Feature))
  ) %>%
  select(w1, w2, overlap)

saveRDS(conc_overlap, "data/gcc/McRae-overlap.rds")

# Create matrix to remove
conc_overlap_mat <- conc_overlap %>%
  select(w1, w2, overlap) %>%
  pivot_wider(names_from = w2, values_from = overlap) %>%
  column_to_rownames("w1") %>%
  as.matrix()

conc_overlap_mat[upper.tri(conc_overlap_mat, diag = TRUE)] <- NA

conc_overlap_4 <- conc_overlap_mat %>%
  as.data.frame() %>%
  rownames_to_column("w1") %>%
  pivot_longer(-w1, names_to = "w2", values_to = "overlap") %>%
  na.omit() %>%
  filter(
    overlap > 2
  )

g <- graph_from_data_frame(conc_overlap_4, directed = FALSE)

plot(g)
transitivity(g, "average")

##### Wordbank

wb <- readRDS("data/Wordbank/Wordbank-WS-191105.rds") %>%
  filter(
    type == "word",
    definition %in% ws_mcrae_words,
    !is.na(value)
  ) %>%
  select(data_id, age, item_id, category, definition, value) %>%
  group_by(data_id, age) %>%
  nest() %>%
  arrange(age)

part_adj_mat <- function(words) {

  adj_mat <- conc_overlap_mat
  missing_words <- colnames(adj_mat)[!(colnames(adj_mat) %in% words)]
  adj_mat[missing_words, ] <- 0
  adj_mat[, missing_words] <- 0

  return(adj_mat)

}

egrid <- function(v) {

  mat <- t(combn(as.character(v), 2))

  colnames(mat) <- c("w1", "w2")

  return(as_tibble(mat))

}

egrid(wb$data[[1]]$definition)

wb_adj_onlywords <- wb %>%
  mutate(
    n_words = map_int(data, ~sum(!is.na(.x$value)))
  ) %>%
  filter(
    n_words > 10
  ) %>%
  mutate(
    egrid   = map(data, ~egrid(.x$definition) %>%
                          left_join(conc_overlap, by = c("w1", "w2")) %>%
                          filter(overlap > 0)),
    egrid_size = map_int(egrid, nrow)
  )

wb_graph <- wb_adj_onlywords %>%
  mutate(
    graph = map(egrid, graph_from_data_frame, directed = FALSE)
  )

wb_gcc <- wb_graph %>%
  mutate(
    gcc = map_dbl(graph, transitivity, "global"),
    gcc_avg = map_dbl(graph, transitivity, "average")
  )

plot(wb_graph$graph[[5]])

ggplot(wb_gcc, aes(x = age, y = gcc)) +
  geom_jitter() +
  geom_smooth()

ggplot(wb_gcc, aes(x = age, y = gcc_avg)) +
  geom_jitter() +
  geom_smooth()
