path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

#
# models <- readRDS("WS_cats_lms.rds")
# m1 <- models$lm3[[1]]

mTrim <- function(model, pthresh = NA) {

  # Get coefficients and p values
  lm_table <- summary(model)$coefficients %>%
    as_tibble(rownames = "coef")

  if (is.na(pthresh)) pthresh <- 0.05 / nrow(lm_table)

  dropped_coefs <- vector()
  included_coefs <- vector()

  # Working backward through table to test interaction terms first
  for (i in nrow(lm_table):1) {

    coef <- lm_table$coef[i]
    p <- lm_table$`Pr(>|t|)`[i]

    # message(coef)
    # message(p)

    if (p < pthresh) {
      included_coefs <- c(included_coefs, coef)
    } else {

      # Separate interaction effects
      coef_terms <- str_split(coef, ":", simplify = TRUE)

      for (coef_term in coef_terms) {

        # Remove interaction I() text, as well as exponentiation,
        #   Remove "I(" and trailing ")", as well as leading "(", like in
        #   (Intercept)

        coef_term <- str_remove_all(coef_term, "(^I[(]|^[(]|[)]$|.[0-9]+)")

        if (any(str_detect(included_coefs, coef_term))) {
          message(paste0("Not excluding ", coef, " (", coef_term,
                         "), exists at higher-level"))
          # print(included_coefs)
        } else {
          dropped_coefs <- c(dropped_coefs, coef)
        }
      }
    }

    dropped_coefs <- str_replace(dropped_coefs, ".Intercept.", "1")

  }

  for (i in dropped_coefs)
    model <- update(model, as.formula(paste0("~.-", i)), data = model$model)

  return(model)

}

# lapply(models$lm3, mTrim)
