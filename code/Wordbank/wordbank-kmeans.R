if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)

library(factoextra)
library(cluster)

# kmeans

source("../mcdi-setup.R")
source("wordbank-functions.R")

ws <- read_data("Wordbank/WS-scored.rds")$p %>%
  group_by(age) %>%
  nest() %>%
  arrange(age)

################################################################################

dir.create("kmeans_plots", showWarnings = FALSE)

ws <- ws %>%
  mutate(
    clus_gap = map(data, ~select(.x, -data_id, -LEXICAL, -SYNTAX) %>%
                            clusGap(kmeans, nstart = 25, K.max = 10, B = 50)),
  )

ws2 <- ws %>%
  add_column(
    # n_clusters = c(5, 6, 9, 7, 7, 6, 5, 8, 8, 9, 3, 9, 10, 9, 9)
    n_clusters = 4,
  ) %>%
  mutate(
    data2  = map(data, ~select(.x, -data_id, -LEXICAL, -SYNTAX)),
    kmeans = map2(data2, n_clusters, ~kmeans(.x, centers = .y, nstart = 25))
  )

for (i in 1:nrow(ws)) {

  age <- ws$age[i]

  png(paste0("kmeans_plots/fviz_nbclust_", age, "mo.png"),
      width = 4, height = 3, units = "in", res = 600)

  ws$data[[i]] %>%
    select(-data_id, -LEXICAL, -SYNTAX) %>%
    fviz_nbclust(kmeans, method = "wss") %>%
    print()

  dev.off()

  png(paste0("kmeans_plots/fviz_gapstat_", age, "mo.png"),
      width = 4, height = 3, units = "in", res = 600)

  print( fviz_gap_stat(ws$clus_gap[[i]]) )

  dev.off()

}

fviz_plots <- lapply(1:nrow(ws), function(i)
                     fviz_cluster(ws2$kmeans[[i]], data = ws2$data2[[i]]))

ws3 <- ws2 %>%
  mutate(
    means = map(kmeans, ~as_tibble(.x$centers) %>%
                          rownames_to_column("cluster"))
  ) %>%
  select(age, means) %>%
  unnest(means) %>%
  pivot_longer(-c(age, cluster))

bar_plots <- lapply(16:30, function(x)

    ggplot(filter(ws3, age == x), aes(x = name, y = value, fill = cluster)) +
      geom_histogram(position = "dodge", stat = "identity") +
      labs(title = x)

  )

lapply(bar_plots, print)
