library(tidyverse)
library(lcmm)

source("../MCDI/MCDI-analysis/code/LCA/00-LCA_functions.R")

mullen <- readRDS("data/mullen_1.rds") %>%
  group_by(sub) %>%
  mutate(
    sub_num = cur_group_id()
  )

ggplot(mullen, aes(x = age, y = rl_percentile)) +
  geom_line(aes(group = sub), alpha = 0.1)

mullen_lca_ng1 <- lcmm(rl_percentile ~ age,
                         data     = mullen,
                         subject  = "sub_num",
                         ng       = 1,
                         link     = "2-quant-splines")

mullen_lca_ng2_6 <- lapply(2:6, function(ng)
                            lcmm(rl_percentile ~ age,
                                 data     = mullen,
                                 subject  = "sub_num",
                                 ng       = ng,
                                 link     = "2-quant-splines",
                                 mixture  = ~age,
                                 B        = mullen_lca_ng1))

fit_stats <- get_fit_stats(mullen_lca_ng1, mullen_lca_ng2_6)
plot_fit_stats(fit_stats)
