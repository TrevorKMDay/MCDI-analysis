
library(tidyverse)

# path <- "/Research/MCDI/MCDI-analysis/code/MNLFA"
# locs <- c("G:/My Drive", "I:", "/Volumes")
# for (i in locs) {
#   if (dir.exists(paste0(i, path))) {
#     setwd(paste0(i, path))
#   }
# }

# source("../mcdi-setup.R")
# r3c_txt <- readLines(.data("MNLFA/xfer_from_LATIS_local/lex/round3calibration.out"))


charvec2tibble <- function(charvec) {

  x <- charvec %>%
    matrix(ncol = length(.) / 5) %>%
    t() %>%
    as_tibble() %>%
    type_convert(col_types = "cnnnn")

  colnames(x) <- c("var", "est", "se", "est_se", "p")

  return(x)
}

extract_intDIF <- function(r3c_txt, all.combos = TRUE) {

  # Get the index where the relevant parts start and end

  all_labs <- str_subset(r3c_txt, "  ETA BY ") %>%
    str_remove_all("(^  ETA BY |[*].*$)")

  message("Unique labs:")
  message(paste(all_labs, collapse = " "))

  # Start with intercept DIF
  eta_on <- which(r3c_txt == " ETA        ON")
  if (length(eta_on) == 0) {
    warning("No intercept DIF was detected")

    # But if no intercept DIF, find where ETA BY ends
    eta_by <- which(r3c_txt == " ETA      BY")
    n_dep <- r3c_txt[str_detect(r3c_txt, "^Number of dependent variables")] %>%
      str_extract("[0-9]*$") %>%
      as.numeric()

    eta_on <- eta_by + n_dep + 2
  }

  intercepts <- which(r3c_txt == " Intercepts")

  dif <- r3c_txt[(eta_on):(intercepts - 2)]

  # Extract the labels (length = n)
  labs <- dif[str_detect(dif, "[A-Z]* ON$")] %>%
    trimws() %>%
    str_remove(" *ON$")

  # Collapse all information into one line (length = n)
  # Not sure what the "Means" line is, but ditch it
  data <- (dif[!str_detect(dif, "[A-Z]* ON$")] %>%
             paste(collapse = "&") %>%
             str_split(., "&&"))[[1]] %>%
    str_subset("^ Means", negate = TRUE)

  # Split each into list of values
  split_data <- sapply(
    data,
    function(.x) {
      .x %>%
        trimws() %>%
        str_replace_all("&", " ") %>%
        str_split(" +")
    }
  )

  # Wrap each one into a data frame
  tibbles <- lapply(split_data, charvec2tibble) %>%
    unname()

  # Join labels with data
  full_DIF <- tibble(
    label = labs,
    result = tibbles
  ) %>%
    unnest(result)

  # Create missing rows for clarity
  all_combos <- expand_grid(
    label = unique(c("ETA", all_labs)),
    var = unique(full_DIF$var)
  ) %>%
    arrange(label) %>%
    left_join(full_DIF, by = c("label", "var"))

  if (all.combos) {
    return(all_combos)
  } else {
    return(full_DIF)
  }
}

extract_loadingDIF <- function(r3c_txt, all.combos = TRUE) {

  # Check for results
  new_addtl_param <- which(r3c_txt == "New/Additional Parameters")
  if (length(new_addtl_param) == 0) {
    message("No loading DIF found, returning NA")
    return(NA)
  }

  # Get L indexes
  eta_by <- tibble(str = str_subset(r3c_txt, "^  ETA BY ")) %>%
    separate(str, into = c(NA, NA, NA, "var", "L"), sep = "[ *]+") %>%
    mutate(
      L = str_remove_all(L, "[()_;]") %>%
        toupper()
    )

  # Get var indexes
  eta_on <- str_subset(r3c_txt, "^  ETA ON") %>%
    sapply(function(x) trimws(str_remove_all(x, "(ETA ON |;)"))) %>%
    str_split(" ")

  if (length(eta_on) > 0) {

  eta_onlut <- tibble(
      covariate = eta_on[[1]]
    ) %>%
    mutate(
      n = row_number()
    ) %>%
    add_row(covariate = "intercept", n = 0, .before = 1)

  } else {
    eta_onlut <- tibble(covariate = "intercept", n = 0)
  }

  # Get results
  qual_num_results <- which(r3c_txt == "QUALITY OF NUMERICAL RESULTS")

  loadDIF <- r3c_txt[(new_addtl_param + 1):(qual_num_results - 3)]

  loadDIF_tbl <- loadDIF %>%
    sapply(trimws) %>%
    sapply(function(x) unname(str_split(x, pattern = " +"))) %>%
    as.data.frame() %>%
    t() %>%
    as_tibble() %>%
    type_convert(col_types = "cnnnn") %>%
    rename(
      var = V1,
      est = V2,
      se = V3,
      est_se = V4,
      p = V5
    ) %>%
    separate(var, into = c("L", "n"), convert = TRUE) %>%
    left_join(eta_by, by = "L") %>%
    left_join(eta_onlut, by = "n") %>%
    select(-L, -n) %>%
    rename(
      label = var,
      var = covariate
    )

  all_combos <- expand_grid(
      label = eta_by$var,
      var = eta_onlut$covariate
    ) %>%
    left_join(loadDIF_tbl, by = c("label", "var"))

  if (all.combos) { return(all_combos) }
}

extract_loading <- function(scoring_txt) {

  n_dep_var <- str_subset(scoring_txt, "^Number of dependent variables") %>%
    str_remove("Number of dependent variables *") %>%
    as.numeric()

  eta_by <- which(str_detect(scoring_txt, "^ ETA *BY"))
  rows <- scoring_txt[(eta_by + 1):(eta_by + n_dep_var)]

  table <- rows %>%
    trimws() %>%
    lapply(., function(x) str_split(x, "[ ]+")) %>%
    unlist() %>%
    matrix(nrow = 5) %>%
    t() %>%
    as_tibble() %>%
    type_convert(col_types = "cnnnn") %>%
    rename(
      label  = V1,
      est    = V2,
      se     = V3,
      est_se = V4,
      p      = V5
    ) %>%
    mutate(
      across(where(is.numeric), ~replace(.x, .x == 999, NA))
    )

  return(table)

}

###############################################################################


package_aMNLFA_dir <- function(dir) {

  message(dir)

  r3c <- paste0(dir, "/round3calibration.out")
  if (!file.exists(r3c)) {
    warning(r3c, " does not exist")
    return(NA)
  }

  scoring <- paste0(dir, "/scoring.out")
  if (!file.exists(scoring)) {
    warning(scoring, " does not exist")
    return(NA)
  }

  r3c_lines <- readLines(r3c)
  scoring_lines <- readLines(scoring)

  result <- list(
    name         = basename(dir),
    loading      = extract_loading(r3c_lines),
    interceptDIF = extract_intDIF(r3c_lines),
    loadingDIF   = extract_loadingDIF(r3c_lines),
    summaries    = readModels(scoring)
  )

  return(result)

}

