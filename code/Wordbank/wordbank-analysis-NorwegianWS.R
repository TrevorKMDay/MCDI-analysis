setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")

library(tidyverse)
library(psych)
library(lavaan)
library(viridis)

source("../mcdi-setup.R")
source("wordbank-functions.R")

################################################################################

nob <- read_data("Wordbank/Wordbank-Norwegian-WS-210812.csv")
# eng <- read_data("Wordbank/WS-scored.rds")$n

nob_demo <- nob %>%
  select(data_id, age, sex, mom_ed) %>%
  distinct()

################################################################################

nob_scored <- score.WS(nob, include.totals = FALSE)
nob_p <- nob_scored$p

save_data(nob_scored, "Wordbank/Wordbank-Norwegian-WS-scored.rds")

################################################################################
# Split half
################################################################################

# Zip code for UMN
set.seed(55455)

nob_efa <- nob_demo %>%
  group_by(age, sex, mom_ed) %>%
  sample_frac(size = .5)

# Everyone in WS.merge1; everyone not
efa.half.ID <- as.character(nob_efa$data_id)
cfa.half.ID <- nob_p$data_id[!(nob_p$data_id %in% efa.half.ID)]

efa.half <- nob_p %>%
  filter(data_id %in% efa.half.ID)
cfa.half <- nob_p %>%
  filter(data_id %in% cfa.half.ID)

################################################################################
# Exploratory factor analysis
################################################################################

# Max.factors is the most I want to do, it's max_interpretable + 1
max.factors <- 5

FA1000 <- .data("norwegian-fa.rds")
if (file.exists(FA1000)) {

  # If the 1,000-iteration analysis has been run and saved, load it,
  # otherwise ...
  factor.analyses <- readRDS(FA1000)

} else {

  # ... actually run FAs with default method/iterations
  factor.analyses <- lapply(1:max.factors,
                            function(x)
                              apply.FA(select(efa.half, -data_id, -age),
                                       factors = x))

  # and save it
  saveRDS(factor.analyses, file = FA1000)

}

# Display plots
for(i in 1:max.factors) {

  png(paste0("plots/WS-Norwegian-factors-", i, ".png"), width = 5, height = 5,
      units = "in", res = 300)

  print( fa.diagram(factor.analyses[[i]]) )

  dev.off()

}

# This is psych's built-in method for estimating factors (and princ. comp.,
# but I left those out)
fa.parallel.plot <- efa.half %>%
  select(-data_id, -age) %>%
  fa.parallel(fa = "fa")

################################################################################
# Confirmatory factor analysis
################################################################################

# CFA doesn't seem to like the bootstrapped intervals, so redo it
# mod2 <- structure.diagram(factor.analyses[[2]], cut = .9, errors = TRUE)

fa_2fac <- fa(select(efa.half, -data_id, -age),
              2, rotate = "Promax",  weight = NULL)
model_2fac <- structure.diagram(fa_2fac, cut = 0.6, errors = TRUE)

nobs <- list(nrow(cfa.half), nrow(efa.half))

mcdi.cfa.1 <- cfa(model = model_2fac$lavaan,
                  data = select(cfa.half, -data_id, -age),
                  estimator = "MLR",
                  sample.nobs = nobs)

summary(mcdi.cfa.1, fit.measures = TRUE)

mix <- modificationIndices(mcdi.cfa.1, sort. = TRUE)

################################################################################

# Data must be in the same order as factor.analyses. Instead of redoing, do
# this li'l munge
FA_col_order <- colnames(factor.analyses[[2]]$residual)

nob_scores <- factor.scores(dplyr::select(nob_p, all_of(FA_col_order)),
                             factor.analyses[[2]], method = "Thurstone")

plot_scores <- bind_cols(nob_demo, as_tibble(nob_scores$scores))

ggplot(plot_scores, aes(x = MR1, y = MR2, color = age)) +
  scale_color_viridis() +
  geom_point(alpha = 0.25, size = 1) +
  labs(x = "Lexical", y = "Syntactic", color = "Age") +
  geom_abline(linetype = "longdash", color = "red", size = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black",
              size = 1)

ggplot(filter(plot_scores, !is.na(sex)),
       aes(x = MR1, y = MR2, color = sex)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)

ggplot(filter(plot_scores, !is.na(mom_ed)),
       aes(x = MR1, y = MR2, color = mom_ed)) +
  geom_point(alpha = 0.25, size = 1)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = FALSE)

# Group by age
plot_scores_summary <- plot_scores %>%
  rename(
    Lexical = MR1,
    Structural = MR2
  ) %>%
  pivot_longer(-c(data_id, age, sex, mom_ed,),
               names_to = "factor") %>%
  group_by(age, factor) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE)
  ) %>%
  mutate_at("factor", as_factor)

ggplot(plot_scores_summary,
       aes(x = age, y = mean, color = factor, fill = factor)) +
  geom_line(aes(y = mean), size = 2) +
  geom_ribbon(aes(ymin = mean - 0.5 * iqr, ymax = mean + 0.5 * iqr),
              alpha = 0.4) +
  geom_ribbon(aes(ymin = mean - 1.5 * iqr, ymax = mean + 1.5 * iqr),
              alpha = 0.3) +
  facet_grid(rows = vars(factor)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Age (mo.)", y = "Mean score [1 IQR, 3 IQR]")
