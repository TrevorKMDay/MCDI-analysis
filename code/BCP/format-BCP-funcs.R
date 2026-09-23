format.sentences <- function(sentences, s_dict_file) {

  s_dict <- read_csv(s_dict_file, show_col_types = FALSE)

  sent <- sentences %>%
    select(data_id, sent.Candidate_Age, sex,
           sent.Administration,
           starts_with("sent.I")) %>%
    mutate(
      age = as.numeric(sent.Candidate_Age)
    ) %>%
    filter(
      sent.Administration == "All"
    ) %>%
    select(-sent.Administration, -sent.Candidate_Age) %>%
    pivot_longer(-c(data_id, age, sex)) %>%
    filter(
      !grepl("score", name)
    ) %>%
    filter(
      !grepl("status", name)
    ) %>%
    mutate(
      name = gsub("sent.", "", name)
    ) %>%
    separate(name, into = c("part", "section", "x"),
             extra = "merge") %>%
    separate(x, into = c("subsection", "question"), fill = "left",
             convert = TRUE)

  words <- filter(s_dict, item_kind == "word")
  categories <- words$category
  definitions <- words$item_definition
  item_ids <- words$item_id

  sent.IA <- filter(sent, part == "I", section == "A") %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(category = rep(categories, length.out = nrow(.))) %>%
    add_column(item_kind = "word") %>%
    add_column(item_definition = rep(definitions,
                                     length.out = nrow(.))) %>%
    add_column(item_id = rep(item_ids, length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  HUW <- filter(s_dict, item_kind == "how_use_words")

  sent.IB <- filter(sent, part == "I", section == "B") %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(HUW$item_id, length.out = nrow(.))) %>%
    add_column(item_kind = "how_use_words") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(HUW$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  WE <- filter(s_dict, item_kind == "word_endings")

  sent.IIA <- filter(sent, part == "II", section == "A") %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(WE$item_id, length.out = nrow(.))) %>%
    add_column(item_kind = "word_endings") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(WE$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  WFn <- filter(s_dict, item_kind == "word_forms_nouns")
  WFv <- filter(s_dict, item_kind == "word_forms_verbs")

  sent.IIBn <- filter(sent, part == "II", section == "B", question <= 5) %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(WFn$item_id,
                             length.out = nrow(.))) %>%
    add_column(item_kind = "word_forms_nouns") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(WFn$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  sent.IIBv <- filter(sent, part == "II", section == "B", question > 5) %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(WFv$item_id,
                             length.out = nrow(.))) %>%
    add_column(item_kind = "word_forms_verbs") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(WFv$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  WEn <- filter(s_dict, item_kind == "word_endings_nouns")
  WEv <- filter(s_dict, item_kind == "word_endings_verbs")

  # First 14 are nouns, last are verbs

  sent.IICn <- filter(sent, part == "II", section == "C", question <= 14) %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(WEn$item_id,
                             length.out = nrow(.))) %>%
    add_column(item_kind = "word_endings_nouns") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(WEn$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  sent.IICv <- filter(sent, part == "II", section == "C", question > 14) %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = if_else(is.na(.$value), "", "produces")) %>%
    add_column(item_id = rep(WEv$item_id,
                             length.out = nrow(.))) %>%
    add_column(item_kind = "word_endings_verbs") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(WEv$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  # No MLU in Wordbank
  #sent.IID <- filter(sent, part == "II", section == "D")

  complexity <- filter(s_dict, item_kind == "complexity")

  sent.IIE <- filter(sent, part == "II", section == "E") %>%
    arrange(data_id, age, subsection, question) %>%
    mutate(value = replace(value,
                           which(value == "more_complex"),
                           "complex") %>%
             replace(value != "more_complex",
                     "simple")) %>%
    add_column(item_id = rep(complexity$item_id,
                             length.out = nrow(.))) %>%
    add_column(item_kind = "complexity") %>%
    add_column(category = NA) %>%
    add_column(item_definition = rep(complexity$item_definition,
                                     length.out = nrow(.))) %>%
    select(data_id, age, sex, value, item_id, item_kind,
           category, item_definition)

  all <- bind_rows(sent.IA, sent.IB, sent.IIA, sent.IIBn, sent.IIBv, sent.IICn,
                   sent.IICv, sent.IIE) %>%
    arrange(data_id, age)

  return(all)

}

format.gestures <- function(gestures, g_dict_file, inventory.only = TRUE) {

  words <- read_csv(g_dict_file, show_col_types = FALSE) %>%
    filter(
      item_kind == "word"
    )

  gest <- gestures %>%
    select(data_id, gest.Candidate_Age, sex, gest.Administration,
           starts_with("gest.I")) %>%
    filter(
      gest.Administration == "All"
    ) %>%
    select(-gest.Administration) %>%
    pivot_longer(-c(data_id, gest.Candidate_Age, sex)) %>%
    filter(
      !grepl("score", name),
      !grepl("status", name),
      !grepl("F_replacement", name)
    ) %>%
    mutate(
      name = gsub("gest.", "", name)
    ) %>%
    separate(name, into = c("part", "section", "x"), extra = "merge") %>%
    separate(x, into = c("subsection", "question"), fill = "left",
             convert = TRUE)

  # words <- g_dict

  # Part I.D is the inventory
  gest.ID <- gest %>%
    filter(
      part == "I",
      section == "D"
    ) %>%
    mutate(
      item_kind = "word",
      item_id    = rep(words$item_id,    length.out = nrow(.)),
      category   = rep(words$category,   length.out = nrow(.)),
      definition = rep(words$item_definition, length.out = nrow(.))
    ) %>%
    select(data_id, gest.Candidate_Age, sex, value, item_id, item_kind,
           category, definition) %>%
    mutate(
      value = replace(value, value == "not_answered", NA),
      value = replace(value, value == "says_and_understands", "produces")
    ) %>%
    rename(
      age = gest.Candidate_Age
    )

  # If inventory only, return just section I.D,
  # haven't written full section yet
  if (inventory.only)
    return(gest.ID)
  else
    return(NA)

}
