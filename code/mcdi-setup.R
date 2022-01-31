# if(.Platform$OS.type == "unix") {
#   mcdi_root <- "/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis"
# } else {
#   mcdi_root <- "G:/My Drive/Research/MCDI/MCDI-analysis/"
# }

path <- "/Research/MCDI/MCDI-analysis/"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")
for (i in locs)
  if (dir.exists(paste0(i, path)))
    mcdi_root <- paste0(i, path)

code_dir <- paste0(mcdi_root, "/code")
data_dir <- paste0(mcdi_root, "/data")

most_recent <- function(regex, directory = ".") {

  result <- list.files(path = directory, pattern = regex, recursive = TRUE,
                       full.names = TRUE)

  if (length(result) == 0) {
    message(paste("No files matching pattern", regex, "found."))
    return(NA)
  } else {
    # If multiple files found, return the most recent (last) one
    return(result[length(result)])
  }

}

.data <- function(filename) {

  full_path <- list.files(path = data_dir,
                          pattern = filename,
                          recursive = TRUE,
                          full.names = TRUE)

  if (length(full_path) == 1) {
    return(full_path)
  } else if (length(full_path) > 1) {
    message("Multiple matching files found")
    return(NA)
  } else {
    return(paste0(data_dir, "/", filename))
  }

}

save_data <- function(data, filename) {

  dest <- paste0(data_dir, "/", filename)

  if (!file.exists(dest)) {
    saveRDS(data, file = dest)
  } else {
    message("File exists, not saving RDS")
  }

}

read_data <- function(filename) {

  src <- paste0(data_dir, "/", filename)

  if (grepl(".rds$", ignore.case = TRUE, x = src)) {
    x <- readRDS(src)
  } else if (grepl(".csv$", x = src)) {
    x <- read_csv(src)
  }

  return(x)

}
