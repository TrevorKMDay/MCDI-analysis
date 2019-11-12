#
# Functions
#

score.produces <- function(v, score.understands = FALSE) {

  # Apply identical() over character array to compare against "produces" and
  # not choke on `NA`
  if (!score.understands) {
    vals <- sapply(v, function(x) identical(as.character(x), "produces"))
  } else {
    vals <- ifelse(is.na(v), 0,
                 ifelse(v == "understands", 1,
                        ifelse(v == "produces", 2, NA)))
  }

  return(vals)

}

score.complexity <- function(v) {

  # Dummy code "complex" as 1 and "simple" as 0
  # Missing would also be 0 acc. to the manual 2e
  score <- ifelse(v == "complex", 1, 0)
  return(score)

}

# Score "sometimes"/"often"/"not yet"
score.SONy <- function(v) {

  # Score {often, 2}, {sometimes, 1}, {not yet, 0}
  score <- ifelse(v == "often", 2,
                  ifelse(v == "sometimes"), 1, 0)

  return(score)

}


# A function to apply the factor analysis over a given # of factors, so that
# they can be saved as a list
apply.FA <- function(data, factors, rotate = "oblimin", n.iter = 1000) {

  x <- data %>%
          ungroup() %>%
          select(-data_id) %>%
          fa(nfactors = factors, rotate = rotate, n.iter = n.iter)

  return(x)

}
