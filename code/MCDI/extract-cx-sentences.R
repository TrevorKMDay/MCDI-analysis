library(wordbankr)
library(tidyverse)

setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/MCDI")

ws <- get_item_data("English (American)", form = "WS")

cx <- ws %>%
  filter(type == "complexity") %>%
  select(definition) %>%
  separate(definition, into = c("simple", "complex"), sep = " / ")

write_csv(cx, "complexity-sentences.csv")
