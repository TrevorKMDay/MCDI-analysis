#
# Functions
#

score.produces <- function(v, score.understands = FALSE,
                           produces = "produces") {

  # Apply identical() over character array to compare against "produces" and
  # not choke on `NA`
  if (!score.understands) {
    vals <- sapply(v, function(x) identical(as.character(x), produces))
  } else {
    vals <- ifelse(is.na(v), 0,
                 ifelse(v == "understands", 1,
                        ifelse(v == produces, 2, NA)))
  }

  return(vals)

}

score.complexity <- function(v, cx = "complex") {

  # Dummy code "complex" as 1 and "simple" as 0
  # Missing would also be 0 acc. to the manual 2e
  score <- ifelse(v == cx, 1, 0)
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

# This function takes a raw gestures table (long) and scores it as if they were
# responses to Words & Sentences
# TO DO: flexibly use in/inside or the AND/OR of in/inside
# Drop "in" because more kids use "inside"
score.GasS <- function(gestures, sc.understands = FALSE) {

  mapping <- read_csv("sharedwords.csv")

  scored <- gestures %>%
              filter(definition != "in") %>%
              mutate(s.category = mapping$s.cat[match(definition,
                                                      mapping$definition)],
                     says = score.produces(value,
                                           score.understands = sc.understands)) %>%
              group_by(data_id, age, s.category) %>%
              summarise(n = n(),
                        sum = sum(says),
                        perc = sum / n) %>%
              rename(category = s.category) %>%
              ungroup()

  return(scored)

}

score.WS <- function(sentences) {

  scored <- sentences %>%
              filter(type != "complexity") %>%
              mutate(says = score.produces(value),
                     category = as.character(category))

  scored2 <- scored %>%
                group_by(data_id, age, type, category) %>%
                summarise(n = n(),
                          sum = sum(says),
                          perc = sum / n) %>%
                ungroup() %>%
                select(-c(n, sum))

  scored3 <- sentences %>%
              filter(type == "complexity") %>%
              mutate(says = score.complexity(value)) %>%
              group_by(data_id, age, type) %>%
              summarise(n = n(),
                        sum = sum(says),
                        perc = sum / n) %>%
              ungroup() %>%
              select(-c(n, sum))

  scored4 <- bind_rows(scored2, scored3) %>%
              arrange(data_id, age, type, category)

  cat.NA = which(is.na(scored4$category))
  scored4$category[cat.NA] <- toupper(scored4$type[cat.NA])

  return(scored4)

}
