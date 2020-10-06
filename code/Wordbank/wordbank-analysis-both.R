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
max_size <- c(566, 221)

# Loading RDS is faster, so do that if available

wg_rds <- "data/Wordbank-WG-191105.RDS"
if (file.exists(wg_rds)) {
  
  gestures <- readRDS(wg_rds)
  
} else {
  
  # This is large, so it takes a while!
  gestures <- read_csv("data/Wordbank-WG-191105.csv",
                        col_types = cols(.default = "f", age = "i"))
  
  saveRDS(gestures, wg_rds)
  
}


ws_rds <- "data/Wordbank-WS-191105.RDS"
if (file.exists(ws_rds)) {
  
  sentences <- readRDS(ws_rds)
  
} else {

  # This is large, so it takes a while!
  sentences <- read_csv("data/Wordbank-WS-191105.csv",
                  col_types = cols(.default = "f", age = "i"))
  
  saveRDS(sentences, ws_rds)

}

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
                  select(instrument, data_id, age, everything()) %>%
                  as_tibble

# Load in analysis trained on WS only
factor.analyses <- readRDS("data/FA1000-WS-100.RDS")
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

# png("plots/both-smear.png", width = 10, height = 5, units = "in", res = 300)
# smear
# dev.off()

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

png("plots/bothsmear-presentation.png", width = 7.5, height = 5, res = 96, 
    units = "in")

ggplot(count, aes(x = lex, y = syn)) +
  scale_shape_manual(values = c(16, 4)) +
  geom_point(alpha = 0.3, aes(color = instrument, shape = instrument)) +
  labs(x = "Lexical", y = "Syntactic",
       color = "Instrument", shape = "Instrument") +
  geom_abline(slope = max_size[2] / max_size[1], 
              color = "red", linetype = "longdash", size = 1) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black")

dev.off()

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

# Approximations in yearss
mom_ed_levels_n <- c(5, 8, 12, 14, 16, 18, 20)

# Whether mom finished college (HS v. not is very low: 200 vs 2000)
mom_bachelors <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)

count.demo <- merge(count, gs.demo, by = c("data_id", "age")) %>%
                mutate_at("mom_ed", as.character) %>%
                add_column(mom_ed_n = mom_ed_levels_n[match(.$mom_ed,
                                                            mom_ed_levels)],
                           mom_bachelors = mom_HS[match(.$mom_bachelors, 
                                                        mom_ed_levels)])


lex.control <- lm(lex ~ 1 + age + sex + mom_ed_n, data = count.demo)
syn.control <- lm(syn ~ 1 + age + sex + mom_ed_n, data = count.demo)

#
# How much does WGasWS underestimate WS?
#

# Score as if all words not in gestures were 0
sent.words_WG <- sentences.words %>%
                  mutate(value = replace(value,
                                         !(definition %in% all.g.words),
                                         NA))

# Perc
sent.words_WG.sc <- score.WS(sent.words_WG)[[2]]

sen.merge.plot <- sen.merge %>%
                    select(data_id, age, category, starts_with("perc.")) %>%
                    pivot_longer(-c(data_id, age, category))

ggplot(sen.merge.plot, aes(x = age, y = value, color = name)) +
  stat_smooth(method = "lm", fullrange = TRUE) +
  facet_wrap(vars(category)) +
  scale_x_continuous(limits = c(10, 30)) +
  theme(legend.position = "bottom")

ggplot(sen.merge, aes(x = age, y = perc.gestures - perc.complete)) +
  geom_point(alpha = 0.01) +
  geom_abline() +
  stat_smooth(fullrange = TRUE, method = "lm") +
  scale_x_continuous(limits = c(8, NA)) +
  facet_wrap(vars(category))



lm.cat <- function(estimates, cat) {

  x <- estimates %>%
        filter(category == as.character(cat)) %>%
        mutate(diff = perc.gestures - perc.complete)

  print(x[1,])

  lm.1 <- lm(diff ~ age, data = x)

  return(lm.1)

}

lm.result <- function(lm, x) {

  b <- unname(lm$coefficients[1])
  m <- unname(lm$coefficients[2])

  y <- m * x + b

  return(y)

}

relationships <- sapply(levels(sen.merge$category),
                        function(x) coef(lm.cat(sen.merge, x))) %>%
                  t() %>%
                  data.frame() %>%
                  add_column(name = levels(sen.merge$category),
                             .before = 1)

png("plots/error-presentation.png", width = 7.5, height = 5, res = 96, 
    units = "in")

ggplot(sen.merge, aes(x = age, y = perc.gestures - perc.complete)) +
  geom_abline(data = relationships,
              aes(intercept = X.Intercept., slope = age, color = name,
                  linetype = name),
              size = 1, alpha = 0.5) +
  scale_x_continuous(limits = c(8, 16)) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = rep(hue_pal()(5), each = 5)[1:22]) +
  scale_linetype_manual(values = rep(1:5, length.out = 22)) +
  theme(legend.position = "right") +
  labs(x = "Age (mo.)", y = "%WG - %WS")

dev.off()

# For each category, estimate the difference between scores using linear
# regression at 10 and 16 months
est.10_16 <- sapply(relationships,
                    function(x) lm.result(x, c(10, 16)))

################################################################################

median.ed <- median(count.demo$mom_ed_n, na.rm = TRUE)
mom.hi <- count.demo$mom_ed_n >= median.ed

all <- bind_rows(sent.count, gw.as.sent.count) %>%
        select(data_id, age, instrument,
               everything(),
               -starts_with("W"), -COMPLEXITY) %>%
        as_tibble() %>%
        mutate(SUM = select(., -data_id, -age, -instrument) %>% rowSums()) %>%
        inner_join(count.demo) %>%
        select(data_id, age, instrument, sex, mom_ed, mom_ed_n,
               everything()) %>%
        add_column(mom_ed_hi = mom.hi, .after = "mom_ed_n")

png("plots/Wordbank-trends-presentation.png", width = 5.5, height = 5, 
    res = 300,
    units = "in")

ggplot(drop_na(all), aes(x = age, y = SUM, color = sex)) +
  geom_jitter(width = 0.25, alpha = 0.1) +
  geom_smooth(aes(linetype = mom_ed_hi), na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 680)) +
  scale_x_continuous(limits = c(5, 40)) +
  labs(x = "Age (mo.)", y = "Words known", color = "Sex", 
       linetype = "Mom ed (high)") +
  theme(legend.position = "bottom")

dev.off()

#
#
#

sent.count <- read_rds("data/WS-scored.rds")[[1]]

all.n <- bind_rows(gw.as.sent.count, sent.count) %>%
          select(-COMPLEXITY, -starts_with("W")) %>%
          mutate(SUM = rowSums(.[, -(1:3)]))

png("plots/words-presentation.png", width = 6, height = 5, units = "in",
    res = 300)

ggplot(all.n, aes(x = age, y = SUM)) +
  geom_boxplot(aes(group = age), outlier.alpha = 0) +
  geom_jitter(width = .3, alpha = 0.1) +
  labs(x = "Age (mo)", y = "Words") +
  geom_smooth()

dev.off()
