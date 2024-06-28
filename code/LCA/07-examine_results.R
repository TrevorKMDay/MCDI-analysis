path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)

source("../mcdi-setup.R")
source("00-LCA_functions.R")

WS_1A <- read_data("other/s_dict.csv") %>%
  pull(category) %>%
  unique()

WS_II <- c("WORD_FORMS_NOUNS", "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
           "WORD_ENDINGS_VERBS", "COMPLEXITY")

func_words <- WS_1A[16:22]
verbs <- "action_words"
adjs <- "descriptive_words"
nouns <- WS_1A[!(WS_1A %in% c(func_words, verbs, adjs))]

# Load data ====

ng4 <- read_data("LCA/BN0_NG4_results.rds") %>%
  select(proj, data_id, class, prob)

BplusE <- read_data("LCA/BplusE.rds") %>%
  ungroup() %>%
  left_join(ng4, by = c("proj", "data_id")) %>%
  select(-data_id_num, 
         -sounds, -COMPLEXITY, -ends_with("total")) %>%
  pivot_longer(any_of(WS_1A), names_to = "category", values_to = "n") %>%
  mutate(
    lexical_class = case_when(
                      category == "action_words" ~ "verbs",
                      category == "descriptive_words" ~ "adjectives",
                      category %in% func_words ~ "function",
                      category %in% nouns ~ "nouns",
                      # TRUE ~ NA
                    ),
    class = recode(class, 
                    `1` = "low", `2` = "high", `3` = "very low", 
                    `4` = "average")
  ) %>%
  rename(
    latent_class = class
  )

BplusE_lexclass <- BplusE %>%
  group_by(proj, status, data_id, age, exact_age, latent_class, 
           lexical_class) %>%
  summarize(
    class_total = sum(n)
  ) %>%
  group_by(proj, status, data_id, age) %>%
  mutate(
    indiv_total = sum(class_total),
    class_pct = class_total / indiv_total,
    outlier = lexical_class == "nouns" & class_pct < 0.50 |
                lexical_class != "nouns" & class_pct > 0.45
  ) %>%
  filter(
    !is.na(latent_class),
    !outlier
  )

plots <- lapply(c("adjectives", "function", "nouns", "verbs"),
                function(x)
                  ggplot(filter(BplusE_lexclass, lexical_class == x),
                         aes(x = exact_age, y = class_pct * 100)) +
                    geom_point(size = 1, alpha = 0.01) +
                    geom_line(aes(group = interaction(data_id, lexical_class)),
                              alpha = 0.01) +
                    geom_smooth(aes(color = latent_class, 
                                    fill = latent_class)) +
                    theme_bw() +
                    labs(x = "Age (mo.)", y = "% of lexicon", color = "LC", 
                         fill = "LC", title = x) +
                    theme(legend.position = "none")
                )

library(patchwork)

(plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]])

