path <- "/Research/MCDI/MCDI-analysis/code/WG2WS"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

message("Loading models ...")

.models <- readRDS("WS_category_models.rds")
.cwords <- readRDS("cwords_model.rds")

message("Done!")

WG_to_WS <- function(category, wg, wg_total = NA, age = NA,
                     cubic = FALSE, trimmed = TRUE,
                     all_models = .models, verbose = FALSE) {

  if (category == "sounds")
    return(wg)

  max <- c(103, 43, 27, 28, 6, 63, 68, 33, 25, 21, 50, 26, 31, 29, 22, 25, 17,
           7, 12, 12, 18, 14)

  names(max) <- c("action_words", "animals", "body_parts", "clothing",
                  "connecting_words", "descriptive_words", "food_drink",
                  "furniture_rooms", "games_routines", "helping_verbs",
                  "household", "locations", "outside", "people", "places",
                  "pronouns", "quantifiers", "question_words", "sounds",
                  "time_words", "toys", "vehicles")

  max_value <- unname(max[names(max) == category])

  if (is.na(age)) {
    if (verbose)
      message("Choosing category-score only model")
    models <- all_models$lm1
  } else {
    if (is.na(wg_total)) {
      if (verbose)
        message("Choosing category score and age model")
      models <- all_models$lm2
    } else {
      if (verbose)
        message("Choosing category score, age, and WG total model")
      if (!cubic) {
        if (trimmed) {
          message("   ... trimmed")
          models <- all_models$lm3_trim
        } else {
          models <- all_models$lm3
        }
      } else {
        message("   ... cubic")
        models <- all_models$lm4
      }
    }
  }

  the_model <- models[all_models$category == category][[1]]

  new_data = tibble(age = age, cat_total_WG = wg, WG_total_score = wg_total) %>%
    mutate(
      age_c = age - 18
    )

  result <- predict(the_model, new_data) %>%
    unname() %>%
    round()

  # Don't return values below 0 or above category-specific max
  result <- if_else(result < 0, 0, result)
  result <- if_else(result > max_value, max_value, result)

  return(result)

}

estimate_cwords <- function(data, .model = .cwords) {

  cwords_hat <- predict(.model, data) %>%
    round() %>%
    unname()

  cwords_hat <- if_else(cwords_hat < 0, 0, cwords_hat)
  cwords_hat <- if_else(cwords_hat > 6, 6, cwords_hat)

  return(cwords_hat)

}
