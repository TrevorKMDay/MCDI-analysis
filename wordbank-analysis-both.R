if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

library(tidyverse)
library(tidyselect)
library(corrplot)
library(psych)

source("wordbank-functions.R")

# Are all words in Gestures a subset of the words in Sentences?

# This is large, so it takes a while!
gestures <- read_csv("data/Wordbank-WG-191105.csv",
                     col_types = cols(.default = "f", age = "i"))

# This is large, so it takes a while!
sentences <- read_csv("data/Wordbank-WS-191105.csv",
                col_types = cols(.default = "f", age = "i"))

gestures.words <- gestures %>%
                    filter(type == "word")

sentences.words <- sentences %>%
                    filter(type == "word")

all.g.words <- gestures.words$definition %>%
                as.character() %>%
                unique()

all.s.words <- sentences.words$definition %>%
                as.character() %>%
                unique()

# 394/296 are ...

g.in.s <- all.g.words %in% all.s.words
sum(g.in.s)

# ... gestures adds "in" and "inside" that are represented by "in/inside" in
#     sentences
all.g.words[which(!g.in.s)]

# S words missing from G
s.in.g <- all.s.words %in% all.g.words
sg.words <- c(all.s.words[s.in.g], "inside")

g.cat <- gestures.words %>%
            filter(data_id == .$data_id[1]) %>%
            select(definition, category) %>%
            rename(g.cat = category)

s.cat <- sentences.words %>%
          filter(data_id == .$data_id[1]) %>%
          select(definition, category) %>%
          rename(s.cat = category) %>%
          mutate(definition = fct_recode(definition, "inside" = "inside/in"))


cats <- merge(g.cat, s.cat, by = "definition") %>%
          filter(definition %in% sg.words)

as.character(cats$g.cat) == as.character(cats$s.cat)

write_csv(cats, path = "sharedwords.csv")

gest.ord <- gestures %>%
              filter(data_id == data_id[1]) %>%
              select(type, category, definition)

write_csv(gest.ord, path = "g_dict.csv")


sent.ord <- sentences %>%
              filter(data_id == data_id[1]) %>%
              select(type, category, definition)

write_csv(gest.ord, path = "s_dict.csv")

################################################################################

# Plot category sizes

g.sizes <- gestures.words %>%
              filter(data_id == .$data_id[1]) %>%
              group_by(category) %>%
              summarise(n = n()) %>%
              ungroup()

s.sizes <- sentences.words %>%
              filter(data_id == .$data_id[1]) %>%
              group_by(category) %>%
              summarise(n = n()) %>%
              ungroup()

sizes <- merge(g.sizes, s.sizes, by = "category", suffixes = c(".g", ".s"),
               all = TRUE) %>%
          rename(gestures = n.g,
                 sentences = n.s) %>%
          pivot_longer(-category)

ggplot(sizes, aes(x = category, y = value, fill = name)) +
  geom_histogram(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Count", fill = "Instrument") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sizes$category)))

################################################################################

# Fake gestures score for sentences

acceptable.words <- read_csv("sharedwords.csv")$definition

# Use gestures subset and score
s.words.shd <- sentences.words %>%
                filter(definition %in% acceptable.words) %>%
                mutate(says = score.produces(value)) %>%
                group_by(data_id, age, category) %>%
                summarise(n = n(),
                          sum = sum(says),
                          perc = sum / n) %>%
                ungroup()

s.words.shd.n <- s.words.shd %>%
                  pivot_wider(c(data_id, age), values_from = sum,
                              names_from = category)

sentences.words.sc <- sentences.words %>%
                        mutate(says = score.produces(value)) %>%
                        group_by(data_id, age, category) %>%
                        summarise(n = n(),
                                  sum = sum(says),
                                  perc = sum / n)

sen.merge <- merge(sentences.words.sc, s.words.shd,
                   by = c("data_id", "age", "category"),
                   all = TRUE,
                   suffixes = c(".complete", ".gestures")) %>%
                mutate(perc.gestures = replace(perc.gestures,
                                               is.na(perc.gestures), 0))

ggplot(sen.merge, aes(x = perc.gestures, y = perc.complete)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(category)) +
  labs(x = "Gestures set", "Sentences set") +
  scale_y_continuous(breaks = c(0, 0.5, 1))

lm(perc.gestures ~ perc.complete, data = sen.merge)

################################################################################

# Score gestures as if it was sentences

gw.as.sent <- score.GasS(gestures.words, sc.understands = FALSE)
gw.as.sent.wide <- gw.as.sent %>%
                    pivot_wider(c(data_id, age),
                                names_from = "category", values_from = perc) %>%
                    mutate(connecting_words = 0,
                            WORD_FORMS_NOUNS = 0,
                            WORD_FORMS_VERBS = 0,
                            WORD_ENDINGS_NOUNS = 0,
                            WORD_ENDINGS_VERBS = 0,
                            COMPLEXITY = 0) %>%
                    add_column(instrument = "WG", .before = 1)

gw.as.sent.count <- gw.as.sent %>%
                      pivot_wider(c(data_id, age),
                                  names_from = "category", values_from = sum) %>%
                      mutate(connecting_words = 0,
                             WORD_FORMS_NOUNS = 0,
                             WORD_FORMS_VERBS = 0,
                             WORD_ENDINGS_NOUNS = 0,
                             WORD_ENDINGS_VERBS = 0,
                             COMPLEXITY = 0) %>%
                      add_column(instrument = "WG", .before = 1)

sentences.scored <- readRDS("WS-scored.rds") %>%
                      add_column(instrument = "WS", .before = 1)

# Bind together. Use sentences first to coerce into correct order w/ bind_rows
all.together <- bind_rows(sentences.scored, gw.as.sent.wide) %>%
                  select(instrument, data_id, age, everything())

# Load in analysis trained on WS only
factor.analyses <- readRDS("FA1000.RDS")
two.factor <- factor.analyses[[2]]

# Estimate scores
all.scores <- factor.scores(select(all.together, -data_id, -age, -instrument),
                            two.factor,
                            method = "Bartlett")

all.scores.summary <- all.scores$scores %>%
                        as_tibble() %>%
                        add_column(data_id = all.together$data_id,
                                   .before = 1) %>%
                        add_column(age = all.together$age,
                                   .after = "data_id") %>%
                        add_column(instrument = as.factor(all.together$instrument),
                                   .after = "age")

ass.WG <- filter(all.scores.summary, instrument == "WG")
ass.WS <- filter(all.scores.summary, instrument == "WS")

# smear <- 
  
ggplot(all.scores.summary, aes(x = MR1, y = MR2)) +
  scale_shape_manual(values = c(16, 4)) +
  geom_point(alpha = 0.5, aes(color = instrument, shape = instrument)) +
  labs(x = "Lexical", y = "Syntactic",
       color = "Instrument", shape = "Instrument") +
  geom_abline() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

joint.poly.model <- lm(MR2 ~ 1 + MR1 + I(MR1^2), data = all.scores.summary)

ggplot(all.scores.summary, aes(x = MR1, y = MR2)) +
  scale_shape_manual(values = c(16, 4)) +
  geom_point(alpha = 0.5, aes(color = instrument, shape = instrument)) +
  labs(x = "Lexical", y = "Syntactic",
       color = "Instrument", shape = "Instrument") +
  geom_abline() +
  theme(legend.position = "bottom") +
  stat_function(fun = function(x) { joint.poly.model$coefficients[1] +
                                    joint.poly.model$coefficients[2] * x + 
                                    joint.poly.model$coefficients[3] * x^2
                                  })

png("plots/both-smear.png", width = 10, height = 5, units = "in", res = 300)
smear
dev.off()

##

# One-weight
sent.count <- readRDS("WS-count.rds") %>%
                add_column(instrument = "WS", .before = 1)

lexical <- colnames(sent.count)[4:18]
syntactic <- colnames(sent.count)[19:30]
count <- bind_rows(sent.count, gw.as.sent.count) %>%
          mutate(lex = select(., one_of(lexical)) %>% rowSums(),
                 syn = select(., one_of(syntactic)) %>% rowSums()) %>%
          select(data_id, age, instrument, lex, syn)

ggplot(count, aes(x = lex, y = syn)) +
  scale_shape_manual(values = c(16, 4)) +
  geom_point(alpha = 0.3, aes(color = instrument, shape = instrument)) +
  labs(x = "Lexical", y = "Syntactic",
       color = "Instrument", shape = "Instrument") +
  geom_abline(slope = 221 / 566, color = "red", linetype = "longdash",
              size = 1) +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) 

both.1w.poly <- lm(syn ~ poly(lex, 2), data = count)

#
# Demographics
#

# Raw model
lex.age <- lm(lex ~ 1 + age, data = count)
syn.age <- lm(syn ~ 1 + age, data = count)

g.demo <- gestures %>%
            select(data_id, age, sex, mom_ed) %>%
            unique()

s.demo <- sentences %>%
          select(data_id, age, sex, mom_ed) %>%
          unique()

gs.demo <- bind_rows(g.demo, s.demo)

mom_ed_levels <- c("Primary", "Some Secondary", "Secondary", "Some College",
                   "College", "Some Graduate", "Graduate")

mom_ed_levels_n <- c(5, 8, 12, 14, 16, 18, 20)

count.demo <- merge(count, gs.demo, by = c("data_id", "age")) %>%
                mutate_at("mom_ed", as.character) %>%
                add_column(mom_ed_n = mom_ed_levels_n[match(.$mom_ed, 
                                                            mom_ed_levels)])


lex.control <- lm(lex ~ 1 + age + sex + mom_ed_n, data = count.demo)
syn.control <- lm(syn ~ 1 + age + sex + mom_ed_n, data = count.demo)
