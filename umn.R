if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis")
}

library(tidyverse)

mcdi_all <- read_csv("bcp-mcdi-103019.csv")

#
# MCDI sentences
#

# Pare down and simplify labels
MS <- mcdi_all %>%
        select(starts_with("demographics"),
                starts_with("mcdi_words_sentences")) %>%
        select_all(~gsub(",", ".", .)) %>%
        select_all(~gsub("mcdi_words_sentences", "sent", .)) %>%
        select_all(~gsub("demographics", "demo", .)) %>%
        filter(sent.Administration == "All") %>%
        separate(demo.Visit_label,
                 into = c(NA, "demo.age_bin"),
                 sep = "x") %>%
        mutate(demo.age_bin = suppressWarnings(as.numeric(gsub("m", "", .$demo.age_bin)))) %>%
        add_column(demo.age = suppressWarnings(as.numeric(.$sent.Candidate_Age)),
                  .after = "demo.age_bin") %>%
        select(-demo.Cohort, -demo.Current_stage) %>%
        select(starts_with("demo"), starts_with("sent.I")) %>%
        select(-ends_with("_score"))

# Now rotate into long format, where each row is a single question
# We only want to look at IA and II BCE
MS.long <- MS %>%
              pivot_longer(starts_with("sent"),
                           names_to = "question", values_to = "value") %>%
              separate(question, into = c("part", "section", "subsection",
                                          "question"),
                       sep = "_") %>%
              mutate(part = gsub("sent.", "", part)) %>%
              arrange(demo.CandID, part, section, question)

# Separate into tables by part

# Options for value are "says" and "NA" for doesn't say, so turn to T/F by
# inverting NA
MS.I <- MS.long %>%
          filter(part == "I", section == "A") %>%
          mutate(says = !is.na(value))

# Part II doesn't have subsections, so rename subsections to questions,
# but we need to replace subsections with nouns/verbs for B/C.
MS.II <- MS.long %>%
            filter(part == "II", section %in% c("B", "C", "E")) %>%
            mutate(question = subsection,
                   subsection = NA)

#
# Score part I
#

word.labels <- unlist(read_csv("word_labels.csv")[, 1])

MS.I.sc <- MS.I %>%
            group_by(demo.CandID, demo.age_bin, subsection) %>%
            summarise(n = n(), sum = sum(says)) %>%
            mutate(perc = sum / n)

MS.I.scw <- MS.I.sc %>%
              select(-n, -sum) %>%
              pivot_wider(names_from = subsection, values_from = perc)

# Copy labels directly from Wordbank
colnames(MS.I.scw)[-(1:2)] <- word.labels

#
# Score part II
#

# Score B/C, just separated because of the difference in Ns in noun/verb
# categories
#
# B: nouns 1-5;  verbs 6-25
# C: nouns 1-14; verbs 15-31

MS.II.B.sc <- MS.II %>%
                filter(section == "B") %>%
                mutate(subsection = ifelse(question %in% 1:5, "nouns",
                                           "verbs"),
                       says = !is.na(value)) %>%
                group_by(demo.CandID, demo.age_bin, subsection) %>%
                summarise(n = n(), sum = sum(says)) %>%
                mutate(perc = sum / n)

# Spread _w_ide
MS.II.B.scw <- MS.II.B.sc %>%
                  select(-n, -sum) %>%
                  pivot_wider(names_from = subsection, values_from = perc) %>%
                  rename(WORD_FORMS_NOUNS = nouns,
                         WORD_FORMS_VERBS = verbs)

MS.II.C.sc <- MS.II %>%
                filter(section == "C") %>%
                mutate(subsection = ifelse(question %in% 1:14, "nouns",
                                           "verbs"),
                       says = !is.na(value)) %>%
                group_by(demo.CandID, demo.age_bin, subsection) %>%
                summarise(n = n(), sum = sum(says)) %>%
                mutate(perc = sum / n)

# Spread _w_ide
MS.II.C.scw <- MS.II.C.sc %>%
                select(-n, -sum) %>%
                pivot_wider(names_from = subsection, values_from = perc) %>%
                rename(WORD_ENDINGS_NOUNS = nouns,
                       WORD_ENDINGS_VERBS = verbs)

# Score II E

# According to the manual (2E), count only "more_complex" as 1, and missing
# or "less_complex" as 0.

MS.II.E.sc <- MS.II %>%
                filter(section == "E") %>%
                select(-subsection) %>%
                mutate(complexity = value == "more_complex") %>%
                group_by(demo.CandID, demo.age_bin) %>%
                summarise(n = n(),
                          sum = sum(complexity, na.rm = TRUE)) %>%
                mutate(COMPLEXITY = sum / n)

# There's only one score, so no wideness, but conform
MS.II.E.scw <- MS.II.E.sc %>%
                select(-n, -sum, -n.na, -n.ans)

# Merge them all together
MS.II.scw <- merge(MS.II.B.scw, MS.II.C.scw) %>%
              merge(MS.II.E.scw)

MS.scw <- merge(MS.I.scw, MS.II.scw)

today <- format(Sys.Date(), "%y%m%d")
write_csv(MS.scw, paste0("BCP-scores-", today, ".csv"))

