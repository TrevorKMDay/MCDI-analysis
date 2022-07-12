# Exlusion criteria

bcp_words_24m <- bcp %>%
  select(data_id, age, inventory_total) %>%
  arrange(data_id, age) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    n_word_concern = map_chr(data, words_at_24m)
  )

combine_words <- read_data("BCP/bcp-UMN-mcdi-191030.csv") %>%
  select_all(~str_replace(., ",", "."))  %>%
  select(demographics.CandID, mcdi_words_sentences.Candidate_Age,
         mcdi_words_sentences.combining) %>%
  rename(
    data_id = demographics.CandID,
    exact_age = mcdi_words_sentences.Candidate_Age,
    combining = mcdi_words_sentences.combining
  ) %>%
  filter(
    !is.na(exact_age),
    combining != "."
  ) %>%
  mutate(
    data_id = as.character(data_id),
    exact_age = as.numeric(exact_age)
  ) %>%
  arrange(data_id, exact_age) %>%
  mutate(
    not_combining_concern = (exact_age >= 24) & (combining == "not_yet")
  )

bcp_delay_c <- read_data("BCP/bcp-delay_criteria.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label, CBCL.ear_infections,
         ace_subject_medical.devel_concern_language) %>%
  rename(
    data_id = demographics.CandID,
    visit   = demographics.Visit_label,
    ear_inf = CBCL.ear_infections,
    lg_concern = ace_subject_medical.devel_concern_language
  ) %>%
  separate(visit, into = c(NA, "visit"), sep = "x", remove = FALSE) %>%
  filter(
    !is.na(visit)
  ) %>%
  mutate(
    data_id = as.character(data_id),
    visit = as.numeric(str_remove(visit, "m")),
    across(c(ear_inf, lg_concern), ~replace(.x, .x == ".", NA)),

    ear_inf = replace(ear_inf, ear_inf == "not_answered", NA),
  ) %>%
  group_by(data_id) %>%
  summarize(
    ear_inf_concern = any(ear_inf %in% c("6-8", "9_or_more"))
  ) %>%
  arrange(desc(ear_inf_concern)) %>%
  left_join(select(combine_words, data_id, not_combining_concern)) %>%
  left_join(select(bcp_words_24m, data_id, n_word_concern)) %>%
  mutate(
    any_concern = case_when(
      str_detect(n_word_concern, "^<50w") ~ 1,
      str_detect(n_word_concern, "^<50w") & ear_inf_concern ~ 2,
      not_combining_concern ~ 3,
      not_combining_concern & ear_inf_concern ~ 4,
      TRUE ~ 0
    )
  )
