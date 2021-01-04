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
    # Select columns explicitly in the correct order
    select(all_of(c("sounds", "animals", "vehicles", "toys", "food_drink",
                    "clothing", "body_parts", "household", "furniture_rooms",
                    "outside", "places", "people", "games_routines",
                    "action_words", "descriptive_words", "time_words",
                    "pronouns", "question_words", "locations", "quantifiers",
                    "helping_verbs", "connecting_words", "WORD_FORMS_NOUNS",
                    "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
                    "WORD_ENDINGS_VERBS", "COMPLEXITY"))) %>%
    fa(nfactors = factors, rotate = rotate, n.iter = n.iter)

  return(x)

}

# This function takes a raw gestures table (long) and scores it as if they were
# responses to Words & Sentences
# TO DO: flexibly use in/inside or the AND/OR of in/inside
# Drop "in" because more kids use "inside"
score.GasS <- function(gestures, sc.understands = FALSE,
                       mapping = .data("other/sharedwords.csv")) {

  sent_n <- read_csv(.data("other/s_dict.csv")) %>%
    filter(!is.na(category)) %>%
    group_by(category) %>%
    dplyr::summarize(
      n_WS = n()
    )

  mapping <- read_csv(mapping)

  syntactic.categories <- c("connecting_words", "helping_verbs",
                            "locations", "pronouns", "quantifiers",
                            "question_words", "time_words")

  # Score and calculate n, sum, perc
  scored <- gestures %>%
    filter(
      type == "word",
      definition != "in"
    ) %>%
    mutate(
      s.category = mapping$s.cat[match(definition, mapping$definition)],
      says = score.produces(value, score.understands = sc.understands)
    ) %>%
    group_by(data_id, age, s.category) %>%
    summarise(
      n = n(),
      sum = sum(says)
    ) %>%
    rename(
      category = s.category
    ) %>%
    left_join(sent_n) %>%
    mutate(
      perc = sum / n_WS,
      SYNTACTIC = category %in% syntactic.categories
    ) %>%
    ungroup()

  # Get lexical/syntax scores by group
  lex.syn <- scored %>%
    group_by(data_id, age, SYNTACTIC) %>%
    dplyr::summarize(
      N = sum(n_WS),
      SUM = sum(sum)
    ) %>%
    mutate(
      PERC = SUM / N,
      SYNTACTIC = if_else(SYNTACTIC, "SYNTAX", "LEXICAL")
    ) %>%
    dplyr::select(-N) %>%
    pivot_longer(-c("data_id", "age", "SYNTACTIC")) %>%
    pivot_wider(id_cols = c("data_id", "age"),
                names_from = c("name", "SYNTACTIC"))

  scored2.n <- scored %>%
    dplyr::select(-n, -SYNTACTIC) %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = "sum") %>%
    mutate(
      connecting_words = 0,
      WORD_ENDINGS_NOUNS = 0,
      WORD_ENDINGS_VERBS = 0,
      WORD_FORMS_NOUNS = 0,
      WORD_FORMS_VERBS = 0,
      COMPLEXITY = 0
    ) %>%
    left_join(dplyr::select(lex.syn, -starts_with("PERC_"))) %>%
    rename(
      LEXICAL = SUM_LEXICAL,
      SYNTAX = SUM_SYNTAX
    )

  scored2.perc <- scored %>%
    dplyr::select(-sum, -n, -SYNTACTIC) %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = "perc") %>%
      mutate(
        connecting_words = 0,
        WORD_ENDINGS_NOUNS = 0,
        WORD_ENDINGS_VERBS = 0,
        WORD_FORMS_NOUNS = 0,
        WORD_FORMS_VERBS = 0,
        COMPLEXITY = 0
      ) %>%
    left_join(dplyr::select(lex.syn, -starts_with("SUM_"))) %>%
    rename(
      LEXICAL = PERC_LEXICAL,
      SYNTAX = PERC_SYNTAX
    )

  scored3 <- list(scored2.n, scored2.perc)
  names(scored3) <- c("n", "p")

  return(scored3)

}

score.WS <- function(sentences) {

  categories <- c("sounds", "animals", "vehicles", "toys", "food_drink",
                  "clothing", "body_parts", "household", "furniture_rooms",
                  "outside", "places", "people", "games_routines",
                  "action_words", "descriptive_words", "time_words",
                  "pronouns", "question_words", "locations", "quantifiers",
                  "helping_verbs", "connecting_words", "WORD_FORMS_NOUNS",
                  "WORD_FORMS_VERBS", "WORD_ENDINGS_NOUNS",
                  "WORD_ENDINGS_VERBS", "COMPLEXITY")

  # Score non-complexity
  scored1 <- sentences %>%
    filter(type != "complexity") %>%
    mutate(
      says = score.produces(value),
      category = as.character(category)
    ) %>%
    group_by(data_id, age, type, category) %>%
    summarise(
      n = n(),
      sum = sum(says),
      perc = sum / n
    ) %>%
    ungroup()

  # Score complexity
  # Replace NA n with 0, since that's what NA represents before scoring, for
  # parsimony
  scored.cx <- sentences %>%
    filter(type == "complexity") %>%
    mutate(
      says = score.complexity(value)
    ) %>%
    group_by(data_id, age, type) %>%
    summarise(
      n = n(),
      sum = sum(says)
    ) %>%
    mutate(
      sum = replace(sum, is.na(sum), 0),
      perc = sum / n
    ) %>%
    ungroup()

  scored3 <- bind_rows(scored1, scored.cx) %>%
    arrange(data_id, age, type, category) %>%
    mutate(
      type = as.character(type)
    )

  # Only words has subcategories, so copy categories over and use those
  cat.NA <- which(is.na(scored3$category))
  scored3$category[cat.NA] <- scored3$type[cat.NA]

  # Drop old label
  scored4 <- scored3 %>%
    dplyr::select(-type)

  syntactic.categories <- c("complexity", "connecting_words", "helping_verbs",
                            "locations", "pronouns", "quantifiers",
                            "question_words", "time_words",
                            "word_endings_nouns", "word_endings_verbs",
                            "word_forms_nouns", "word_forms_verbs")

  scored4.lexsym <- scored4 %>%
    mutate(
      syntactic = category %in% syntactic.categories
    ) %>%
    group_by(data_id, age, syntactic) %>%
    summarise(
      N = sum(n),
      Sum = sum(sum),
      perc = Sum / N
    ) %>%
    pivot_wider(c(data_id, age), names_from = "syntactic",
                values_from = c("Sum", "perc")) %>%
    rename(
      N_SYNTAX = Sum_TRUE,
      N_LEXICAL = Sum_FALSE,
      P_SYNTAX = perc_TRUE,
      P_LEXICAL = perc_FALSE
    )

  # Spread raw numbers
  scored5.raw <- scored4 %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = sum) %>%
    dplyr::select(all_of(c("data_id", "age",
                           tolower(categories)))) %>%
    left_join(select(scored4.lexsym, data_id, age, starts_with("N"))) %>%
    rename(
      SYNTAX = N_SYNTAX,
      LEXICAL = N_LEXICAL
    )
  colnames(scored5.raw)[25:29] <- toupper(colnames(scored5.raw)[25:29])

  # Spread percent
  scored5.perc <- scored4 %>%
    pivot_wider(c(data_id, age), names_from = "category",
                values_from = perc) %>%
    dplyr::select(all_of(c("data_id", "age",
                           tolower(categories)))) %>%
    left_join(select(scored4.lexsym, data_id, age, starts_with("P"))) %>%
    rename(SYNTAX = P_SYNTAX,
           LEXICAL = P_LEXICAL)
  colnames(scored5.perc)[25:29] <- toupper(colnames(scored5.perc)[25:29])

  scored5 <- list(scored5.raw, scored5.perc)
  names(scored5) <- c("n", "p")

  # Return both raw numbers and percentages
  return(scored5)

}

score.WG <- function(gestures, inventory.only = TRUE, sc.understands = FALSE) {

  # inventory.only : only score words, not gestures
  # sc.understands : if FALSE, treat "understands only" as the same as "doesn't
  #                   understand"

  syntax.categories <- c("time_words", "descriptive_words", "pronouns",
                         "question_words", "locations", "quantifiers")

  if (inventory.only) {

    scored1 <- gestures %>%
      filter(type == "word") %>%
      mutate(says = score.produces(value,
                                   score.understands = sc.understands),
             category = as.character(category),
             syntactic = category %in% syntax.categories) %>%
      group_by(data_id, age, type, category, syntactic) %>%
      summarise(n = n(),
                sum = sum(says),
                perc = sum / n) %>%
      ungroup()

    scored2.lexsym <- scored1 %>%
      group_by(data_id, age, syntactic) %>%
      summarise(
        N = sum(n),
        Sum = sum(sum),
        perc = Sum / N
      ) %>%
      pivot_wider(c(data_id, age), names_from = "syntactic",
                  values_from = c("Sum", "perc")) %>%
      rename(N_SYNTAX = Sum_TRUE,
             N_LEXICAL = Sum_FALSE,
             P_SYNTAX = perc_TRUE,
             P_LEXICAL = perc_FALSE)

    scored2.raw <- scored1 %>%
      pivot_wider(c(data_id, age),
                  names_from = "category", values_from = sum) %>%
      left_join(select(scored2.lexsym, data_id, age, starts_with("N"))) %>%
      rename(SYNTAX = N_SYNTAX,
             LEXICAL = N_LEXICAL)

    # Spread percent
    scored2.perc <- scored1 %>%
      pivot_wider(c(data_id, age),
                  names_from = "category", values_from = perc) %>%
      left_join(select(scored2.lexsym, data_id, age, starts_with("P"))) %>%
      rename(SYNTAX = P_SYNTAX,
             LEXICAL = P_LEXICAL)

    scored2 <- list(scored2.raw, scored2.perc)
    names(scored2) <- c("n", "p")

    return(scored2)

  } else {

    # TO  DO: Score gestures
    return(NA)

  }

}
