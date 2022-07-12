setwd("~/Insync/day00096@umn.edu/Google Drive/Research/MCDI/MCDI-analysis/")

library(tidyverse)
library(viridis)
library(wordbankr)
library(igraph)

## Get words from wordbank

words <- get_item_data("English (American)", form = "WS") %>%
  filter(
    type == "word"
  ) %>%
  select(item_id, category, definition)

## Load in McRae data

dat1 <- read_table("data/gcc/McRae-BRM-InPress/cos_matrix_brm_IFR_1-200.txt")
dat2 <- read_table("data/gcc/McRae-BRM-InPress/cos_matrix_brm_IFR_201-400.txt")
dat3 <- read_table("data/gcc/McRae-BRM-InPress/cos_matrix_brm_IFR_401-541.txt")

mcrae <- left_join(dat1, dat2) %>%
  left_join(dat3)

# Remove values duplicated across diagonal to make df easier to work with
mcrae_long <- pivot_longer(mcrae, -CONCEPT, names_to = "CONCEPT2") %>%
  filter(
    !is.na(value)
  )

rm(dat1, dat2, dat3)

retrieve_mcrae_dist <- function(x1, x2) {

  for (i in c(x1, x2))
    if (!(i %in% mcrae_long$CONCEPT2)) {
      message(paste("Error:", i, "not in dataset"))
      return(NA)
    }

  value <- mcrae_long %>%
    filter(
      CONCEPT == x1 | CONCEPT == x2,
      CONCEPT == x2 | CONCE
    ) %>%
    pull(value)

  return(value)

}

##

mcdi_words_in_mcrae <- words %>%
  filter(
    definition %in% mcrae$CONCEPT
  ) %>%
  rename(
    word = definition
  ) %>%
  mutate(
    order = as.numeric(str_remove(item_id, "item_")),
    rel_order = row_number()
  )

mwim <- expand_grid(word1 = mcdi_words_in_mcrae$word,
                    word2 = mcdi_words_in_mcrae$word) %>%
  filter(
    word1 != word2
  ) %>%
  left_join(mcrae_long, by = c("word1" = "CONCEPT", "word2" = "CONCEPT2")) %>%
  left_join(mcdi_words_in_mcrae, by = c("word1" = "word")) %>%
  left_join(mcdi_words_in_mcrae, by = c("word2" = "word"),
            suffix = c(".1", ".2")) %>%
  mutate(
    # value = replace(value, value == 0, NA)
  )

ggplot(mwim, aes(x = rel_order.1, y = rel_order.2, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_bw()

## Test it on a participant

p1_words <- readRDS("data/Wordbank/Wordbank-WS-191105.rds") %>%
  filter(
    data_id == .$data_id[1],
    type == "word"
  )

p1_mwim <- p1_words %>%
  filter(
    definition %in% mcdi_words_in_mcrae$word
  ) %>%
  select(data_id, item_id, category, definition, value) %>%
  mutate(
    produces = replace_na(value == "produces", FALSE)
  )

p1_produces <- p1_mwim %>%
  filter(
    produces
  )

p1_expand_grid <- expand_grid(word1 = p1_produces$definition,
                              word2 = p1_produces$definition) %>%
  left_join(mwim)

p1_adjacency <- p1_expand_grid %>%
  select(starts_with("word"), value) %>%
  mutate(
    value = replace_na(value, 0) %>%
              replace(., . < .2, 0)
  ) %>%
  rename(
    from = word1,
    to = word2,
    weight = value
  ) %>%
  pivot_wider(names_from = to, values_from = weight) %>%
  column_to_rownames(var = "from") %>%
  as.matrix()

p1_graphBAM <- graphAM(adjMat = p1_adjacency)

nodes(p1_graphBAM)
edges(p1_graphBAM)

plot(p1_graphBAM)
