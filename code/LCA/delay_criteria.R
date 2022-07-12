path <- "/Research/MCDI/MCDI-analysis/code/LCA"
locs <- c("G:/My Drive", "I:", "/Volumes",
          "/home/tkmd/Insync/day00096@umn.edu/Google Drive")

for (i in locs)
  if (dir.exists(paste0(i, path)))
    setwd(paste0(i, path))

library(tidyverse)

source("../mcdi-setup.R")


bcp_ws <- read_data("BCP/BCP_WS_scored-200609.rds")$n
bcp_wg <- read_data("BCP/BCP_WG_asWS-200609.rds")

words_at_24m <- function(df) {

  df1 <- mutate(df, age = round(age, 1))

  if (sum(!is.na(df$TOTAL)) > 1) {

    new_data <- tibble(age = round(seq(11, 40, by = 0.1), 1)) %>%
      left_join(df1, by = "age") %>%
      mutate(
        inv_pred = try(pracma::interp1(age, TOTAL), silent = TRUE, TRUE)
        # inv_pred = try(pracma::interp1(age, TOTAL), FALSE)
      ) %>%
      filter(
        !is.na(inv_pred)
      )

    m24 <- which(new_data$age == 24)
    words <- new_data$inv_pred[m24]

  } else {
    words <- NA
  }

  return(words)

}

bcp_combine <- read_data("BCP/bcp-UMNUNC-mcdi-200609.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label, contains("comb")) %>%
  separate(demographics.Visit_label, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    visit = as.numeric(str_remove(visit, "m")),
    across(contains("comb"), ~replace(.x, .x == ".", NA)),
    not_combining = mcdi_words_sentences.combining_score == "no"
  ) %>%
  na.omit() %>%
  rename(
    data_id = demographics.CandID
  ) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    not_combining_concern = map_lgl(data,
                                    ~any(.x$visit >= 24 & .x$not_combining))
  )


bcp <- bind_rows(bcp_wg, bcp_ws) %>%
  select(data_id, age, TOTAL) %>%
  filter(
    !is.na(TOTAL)
  ) %>%
  arrange(data_id, age) %>%
  group_by(data_id) %>%
  nest() %>%
  mutate(
    w50_by_24mo  = map_lgl(data, ~any(.x$age <= 24 & .x$TOTAL >= 50)),

    min_age      = map_dbl(data, ~min(.x$age)),
    max_age      = map_dbl(data, ~max(.x$age)),

    i_words_24mo = map(data, words_at_24m) %>%
                    as.numeric() %>%
                    round(),

    i_words_ge50 = i_words_24mo >= 50,

    # If the data contain the determination that the child had 50w by 24 months,
    #  assign true. If it doesn't, then check the interpolation. If the interp
    #  predicts 50 words, also flag as TRUE, only if both checks fail, then
    #  assign FALSE. If interpolation was NA, then use that (not enough info
    #  to determine)
    w50_by_24mo2 = if_else(w50_by_24mo, TRUE, i_words_ge50)

  )

table(bcp$w50_by_24mo2, useNA = "a")

cbcl <- read_data("BCP/bcp-CBCL-220209.csv") %>%
  select_all(~str_replace(., ",", ".")) %>%
  select(demographics.CandID, demographics.Visit_label, CBCL.Administration,
         CBCL.ear_infections, CBCL.worried_lang_development) %>%
  rename(
    data_id      = demographics.CandID,
    cohortXvisit = demographics.Visit_label,
    admin        = CBCL.Administration,
    ear_inf      = CBCL.ear_infections,
    lg_concerns  = CBCL.worried_lang_development
  ) %>%
  filter(
    admin == "All"
  ) %>%
  separate(cohortXvisit, into = c(NA, "visit"), sep = "x") %>%
  mutate(
    visit       = as.numeric(str_remove(visit, "m")),
    ear_inf     = replace(ear_inf, ear_inf == "not_answered", NA),
    ear_inf_ge6 = ear_inf %in% c("6-8", "9_or_more"),
    lg_concerns = case_when(lg_concerns == "yes" ~ TRUE,
                            lg_concerns == "no"  ~ FALSE,
                            lg_concerns == "not_answered" ~ NA)
  )

delay3 <- left_join(cbcl, select(bcp, data_id, w50_by_24mo2)) %>%
  left_join(select(bcp_combine, -data)) %>%
  rename(
    w50       = w50_by_24mo2
  ) %>%
  select(-admin, -ear_inf, -visit) %>%
  distinct() %>%
  ungroup() %>%
  mutate(
    lt50w = !w50,
  ) %>%
  mutate(


    n_flags = select(., lt50w, not_combining_concern, lg_concerns,
                      ear_inf_ge6) %>%
                rowSums(na.rm = TRUE),

    delay = case_when(

              # FIRST ASSIGN COMPLETE STATUSES

              # If no flags, consider no delay
              w50 & !not_combining_concern & !lg_concerns & !ear_inf_ge6 ~ 0,

              # <50 words at 24mo, and language concerns
              !w50 & lg_concerns ~ 1,

              # <50 words at 24mo, and 6+ ear infections
              !w50 & ear_inf_ge6 ~ 2,

              w50 & not_combining_concern & lg_concerns ~ 3,
              w50 & not_combining_concern & ear_inf_ge6 ~ 4,

              # NOW START FILLING IN GUESSES

              # If there were no flags excluding NA,
              n_flags == 0 ~ 0.1,
              # If the only flag was parent-report lg concerns, assume OK
              n_flags == 1 & lg_concerns ~ 0.2,

              # If ear infection and words/not combining is ok
              ear_inf_ge6 & w50 & !not_combining_concern ~ 0.3,
              ear_inf_ge6 & w50                          ~ 0.31,
              ear_inf_ge6 & !not_combining_concern       ~ 0.32,
              ear_inf_ge6 & !lg_concerns                 ~ 0.33,

              ear_inf_ge6 & lg_concerns ~ 2.5,
              w50 & not_combining_concern ~ 2.9,

              !w50 ~ 2.1,


            )

  )

table(delay3$delay, useNA = "a")

write_csv(delay3, "delay3.csv")
