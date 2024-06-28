setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/WG2WS")

library(tidyverse)
library(strip)

# Original models ====

cat_models <- readRDS("WS_category_models.rds")
object.size(cat_models)

dir.create("rda_files", showWarnings = FALSE)

# Strip ====

## Total score models ====

total_WG_to_WS <- readRDS("total_WG_to_WS.rds")
total_WG_to_WS_noage <- readRDS("total_WG_to_WS_noage.rds")

total_WG_to_WS_stripped <- strip(total_WG_to_WS, keep = "predict")
total_WG_to_WS_noage_stripped  <- strip(total_WG_to_WS_noage, keep = "predict")

save(total_WG_to_WS_stripped, file = "rda_files/total_WG_to_WS_stripped.rda")
save(total_WG_to_WS_noage_stripped,
     file = "rda_files/total_WG_to_WS_noage_stripped.rda")

## Strip extra stuff from category models ====
cat_models_stripped <- cat_models %>%
  select(-n, -lm3_trim_AIC) %>%
  mutate(
    # Across operates on columns, and since strip operates on models, we have
    #   to lapply over the columns. It's silly
    across(starts_with("lm"),
           ~lapply(.x, function(x) strip(x, keep = "predict")))
  ) %>%
  select(category, lm1, lm4)

format(object.size(cat_models), standard = "SI", units = "KB")
format(object.size(cat_models_stripped), standard = "SI", units = "KB")

save(cat_models_stripped, file = "rda_files/cat_models_stripped.rda")

## Connecting words ====

cw <- readRDS("cwords_model.rds")
cw_stripped <- strip(cw, keep = "predict")
save(cw_stripped, file = "rda_files/cw_stripped.rda")
