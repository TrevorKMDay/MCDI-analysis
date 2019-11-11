if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(tidyselect)
library(psych)
library(viridis)

# This is large, so it takes a while!
gestures <- read_csv("Wordbank-WG-191105.csv",
                      col_types = cols(.default = "f", age = "i"))

source("wordbank-functions.R")

################################################################################
# Extract demographics
################################################################################

g.demo <- gestures %>%
            select(data_id, age, sex, mom_ed) %>%
            distinct()

length(unique(g.demo$data_id)) == nrow(g.demo)

################################################################################
# Score per Part I.D subcategory
# For type = word, sum 'produce' over all
################################################################################

# Convert produces/NA to T/F (takes a while on 3.5M elements)
words <- gestures %>%
          filter(type == "word") %>%
          mutate(produces = score.produces(value))

# For each ID, count the number of responses in each category, then convert
# that to a proportion and spread to wide, keeping only ID and 22 columns
words.grouped <- words %>%
                  group_by(data_id, category) %>%
                  summarise(n = n(), sum = sum(produces)) %>%
                  mutate(score = sum / n) %>%
                  pivot_wider(c(data_id),
                              names_from = category, values_from = score)

################################################################################
# Factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- "FA1000-WG.RDS"
if (file.exists(FA1000)) {
  
  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)
  
} else {
  
  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(words.grouped, factors = x,
                                       rotate = "promax"))
  
  # and save it
  saveRDS(factor.analyses, file = FA1000)
  
}

# Display plots
for(i in 1:max.factors) {
  
  png(paste0("WG-factors", i, ".png"), width = 500, height = 500)
  
  print( fa.diagram(factor.analyses[[i]]) )
  
  dev.off()
  
}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel(words.grouped[, -1], fa = "fa")

# Extra plots

RMSEA <- sapply(factor.analyses, function(x) x$RMSEA) %>%
  t() %>%
  as_tibble() %>%
  select(-confidence) %>%
  add_column(n = 1:5, .before = 1)

ggplot(RMSEA, aes(x = n, y = RMSEA)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower,  ymax = upper), width = 0.1)

#
# Plot 3-factor solution 1+3 against 2
# 

values <- factor.analyses[[3]]$scores %>%
            as_tibble() %>%
            mutate(lexical = MR1 + MR3) 

values.demo <- cbind.data.frame(g.demo, values)

