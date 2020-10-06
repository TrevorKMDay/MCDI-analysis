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

  mapping <- read_csv("data/sharedwords.csv")

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

  cw <- scored %>%
          select(data_id, age) %>%
          unique() %>%
          add_column(category = "connecting_words") %>%
          add_column(n = 5) %>%
          add_column(sum = 0) %>%
          add_column(perc = 0)

  scored2 <- bind_rows(scored, cw) %>%
              arrange(data_id, age, category)

  return(scored2)

}

score.WS <- function(sentences) {

  # Score non-complexity
  scored1 <- sentences %>%
              filter(type != "complexity") %>%
              mutate(says = score.produces(value),
                     category = as.character(category)) %>%
                group_by(data_id, age, type, category) %>%
                summarise(n = n(),
                          sum = sum(says),
                          perc = sum / n) %>%
                ungroup()

  # Score complexity
  # Replace NA n with 0, since that's what NA represents before scoring, for
  # parsimony
  scored.cx <- sentences %>%
    filter(type == "complexity") %>%
    mutate(says = score.complexity(value)) %>%
    group_by(data_id, age, type) %>%
    summarise(n = n(),
              sum = sum(says)) %>%
    mutate(sum = replace(sum, is.na(sum), 0),
            perc = sum / n) %>%
    ungroup()

  scored3 <- bind_rows(scored1, scored.cx) %>%
    arrange(data_id, age, type, category) %>%
    mutate(type = as.character(type))

  # Only words has subcategories, so copy categories over and use those
  cat.NA <- which(is.na(scored3$category))
  scored3$category[cat.NA] <- scored3$type[cat.NA]

  # Drop old label
  scored4 <- scored3 %>%
              dplyr::select(-type)

  # Spread raw numbers
  scored5.raw <- scored4 %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = sum) %>%
    dplyr::select(-how_use_words, -combine, -word_endings)
  colnames(scored5.raw)[25:29] <- toupper(colnames(scored5.raw)[25:29] )

  # Spread percent
  scored5.perc <- scored4 %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = perc) %>%
    dplyr::select(-how_use_words, -combine, -word_endings)
  colnames(scored5.perc)[25:29] <- toupper(colnames(scored5.perc)[25:29] )

  scored5 <- list(scored5.raw, scored5.perc)
  names(scored5) <- c("n", "p")

  # Return both raw numbers and percentages
  return(scored5)

}

score.WG <- function(gestures, inventory.only = TRUE, sc.understands = FALSE) {

  # inventory.only : only score words, not gestures
  # sc.understands : if FALSE, treat understands only as the same as doesn't
  #                   understand

  if (inventory.only){

    scored1 <- gestures %>%
                  mutate(says = score.produces(value,
                                               score.understands = sc.understands),
                         category = as.character(category)) %>%
                  group_by(data_id, age, type, category) %>%
                  summarise(n = n(),
                            sum = sum(says),
                            perc = sum / n) %>%
                  ungroup()

    scored2.raw <- scored1 %>%
                    pivot_wider(c(data_id, age), names_from = "category",
                                values_from = sum)

    # Spread percent
    scored2.perc <- scored1 %>%
                      pivot_wider(c(data_id, age), names_from = "category",
                                  values_from = perc)

    scored2 <- list(scored2.raw, scored2.perc)

    return(scored2)

  } else {

    return(NA)

  }

}
