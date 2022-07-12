setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/semnet")

library(tidyverse)
library(igraph)

source("../mcdi-setup.R")
load(.data("semnet/associations-child.Rdata"))

##### CDI

sdict <- read_data("other/s_dict.csv") %>%
  filter(

    # # Words duplicated in two places
    !str_detect(definition,
                "^(can|chicken|clean|drink|dry|fish|orange|slide|swing|watch|water|work)"),

    # No AoA in Cox & Haebig
    !(definition %in% c("about", "above", "an", "babysitter", "basement",
                        "before", "beside", "but", "camping", "child", "could",
                        "country", "does", "downtown", "each", "every", "hate",
                        "hers", "if", "into", "last", "much", "naughty",
                        "none", "nurse", "out", "person", "play", "pen",
                        "poor", "scarf", "snowsuit", "so", "their", "them",
                        "then", "tights", "tray", "us", "vagina", "vanilla",
                        "walker", "was", "where", "when", "which", "wish",
                        "woods", "would", "yesterday", "yourself")),

    !(definition %in% c("babysitter's name", "child's own name",
                        "give me five!", "gonna get you!", "pet's name",
                        "so big!", "this little piggy"))
  ) %>%
  mutate(
    group = if_else(category %in% c("connecting_words", "helping_verbs",
                                    "locations", "pronouns", "quantifiers",
                                    "question_words", "time_words"),
                    "func", "content")
  )

##### Prepare data

assocs <- associations_child %>%
  select(-SOURCE, -COND, -ORDER, -LIST_ID, -RESP_ID) %>%
  mutate(
    across(c(CUE, RESPONSE), as.character)
  ) %>%
  filter(
    CUE %in% sdict$definition,
    RESPONSE %in% sdict$definition,
  )

assocs_tbl <- assocs %>%
  group_by(CUE, RESPONSE) %>%
  summarize(
    n = n()
  )

assocs_adj <- assocs_tbl %>%
  pivot_wider(names_from = RESPONSE, values_from = n)

ggplot(assocs_tbl, aes(x = n)) +
  geom_histogram(binwidth = 1)

##### Basic network graph

rsample <- assocs_tbl %>%
  ungroup() %>%
  slice_sample(n = 100)

edgelist_to_adjacency <- function(edge) {

  nodes <- c(edge$CUE, edge$RESPONSE) %>%
    unique() %>%
    sort()

  eg <- expand.grid(CUE = nodes, RESPONSE = nodes)

  values <- left_join(eg, edge, by = c("CUE", "RESPONSE"))

  mat <- values %>%
    mutate(
      n = replace_na(n, 0)
    ) %>%
    pivot_wider(names_from = RESPONSE, values_from = n) %>%
    column_to_rownames("CUE")

  return(list(matrix = mat, nodes = nodes))

}

x <- edgelist_to_adjacency(rsample)

qgraph(x$matrix, groups = sdict$group[match(x$nodes, sdict$definition)],
       minimum = 0, threshold = 2, details = TRUE, layout = "spring",
       repulsion = -.7, directed = TRUE)

# centralityPlot(as.data.frame(rsample))

centrality <- rsample %>%
  as.data.frame() %>%
  centralityTable() %>%
  group_by(measure) %>%
  nest()

assocs_tbl %>%
  ungroup() %>%
  slice_sample(n = 800) %>%
  select(CUE, RESPONSE) %>%
  as.matrix() %>%
  graph_from_edgelist() %>%
  transitivity(type = "global")

wb0 <- read_data("Wordbank/Wordbank-WS-191105.rds")

get_assoc <- function(assocs_tbl, word_list) {

  assocs_tbl %>%
    filter(
      CUE %in% word_list,
      RESPONSE %in% word_list
    ) %>%
    select(-n) %>%
    return()

}

wb <- wb0 %>%
  filter(
    type == "word"
  ) %>%
  group_by(data_id, age) %>%
  nest() %>%
  mutate(
    words_known = map(data, ~as.character(.x$definition[!is.na(.x$value)])),
  ) %>%
  ungroup()

wb$assocs <- lapply(wb$words_known, function(x) get_assoc(assocs_tbl, x))

wb2 <- wb %>%
  mutate(
    graph = map(assocs, ~graph_from_edgelist(as.matrix(.x))),
    gcc = map_dbl(graph, transitivity)
  )

wb3 <- wb2 %>%
  mutate(
    n = map_int(words_known, length)
  ) %>%
  select(data_id, age, n, gcc) %>%
  filter(
    n >= 20,
    n <= 600
  )

ggplot(wb3, aes(x = age, y = n)) +
  geom_jitter(alpha = 0.2, width = 0.25) +
  geom_smooth() +
  theme_minimal()


ggplot(wb3, aes(x = age, y = gcc)) +
  geom_jitter(alpha = 0.2, width = 0.25) +
  geom_smooth() +
  theme_minimal()

ggplot(wb3, aes(x = n, y = gcc)) +
  geom_jitter(alpha = 0.2, width = 0.25) +
  geom_smooth() +
  theme_minimal()

###### Random sampling

gcc_from_size <- function(sample_from, n) {

  # Random rows
  temp <- sample_from %>%
    ungroup() %>%
    slice_sample(n = n)

  # Create graph
  g <- temp %>%
    as.matrix() %>%
    graph_from_edgelist()

  gcc <- transitivity(g, type = "global")

  return(gcc)

}

gcc_null <- expand_grid(size = 20:600, rep = 1) %>%
  rowwise() %>%
  mutate(
    gcc = map_dbl(size, ~gcc_from_size(assocs_tbl[, 1:2], .x))
  )

ggplot(wb3, aes(x = n, y = gcc)) +
  geom_jitter(alpha = 0.2, width = 0.25) +
  geom_jitter(data = gcc_null, aes(x = size, y = gcc), alpha = 0.2,
              color = "red") +
  geom_smooth() +
  theme_minimal()
