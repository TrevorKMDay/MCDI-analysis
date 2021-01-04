if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/Wordbank")
}

library(tidyverse)
library(tidyselect)
library(corrplot)
library(psych)

source("wordbank-csv2rds.R")
source("wordbank-functions.R")
max_size <- c(566, 221)

# Loading RDS is faster, setup in csv2rds
gestures <- read_data("Wordbank/Wordbank-WG-191105.rds")
sentences <- read_data("Wordbank/Wordbank-WS-191105.rds")

################################################################################
### Overlapping items

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
  dplyr::select(definition, category) %>%
  rename(g.cat = category)

s.cat <- sentences.words %>%
  filter(data_id == .$data_id[1]) %>%
  dplyr::select(definition, category) %>%
  rename(s.cat = category) %>%
  mutate(definition = fct_recode(definition, "inside" = "inside/in"))


cats <- merge(g.cat, s.cat, by = "definition") %>%
          filter(definition %in% sg.words)

as.character(cats$g.cat) == as.character(cats$s.cat)

write_csv(cats, path = "sharedwords.csv")

gest.ord <- gestures %>%
  filter(data_id == data_id[1]) %>%
  dplyr::select(type, category, definition)

write_csv(gest.ord, path = "g_dict.csv")


sent.ord <- sentences %>%
  filter(data_id == data_id[1]) %>%
  dplyr::select(type, category, definition)

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
  rename(
    gestures = n.g,
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
  summarise(
    n = n(),
    sum = sum(says),
    perc = sum / n) %>%
  ungroup()

s.words.shd.n <- s.words.shd %>%
  pivot_wider(c(data_id, age), values_from = sum,
              names_from = category)

sentences.words.sc <- sentences.words %>%
  mutate(says = score.produces(value)) %>%
  group_by(data_id, age, category) %>%
  summarise(
    n = n(),
    sum = sum(says),
    perc = sum / n)

sen.merge <- merge(sentences.words.sc, s.words.shd,
                   by = c("data_id", "age", "category"),
                   all = TRUE,
                   suffixes = c(".complete", ".gestures")) %>%
  mutate(perc.gestures = replace(perc.gestures, is.na(perc.gestures), 0))

ggplot(sen.merge, aes(x = perc.gestures, y = perc.complete)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(category)) +
  labs(x = "Gestures set", "Sentences set") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2))

lm(perc.gestures ~ perc.complete, data = sen.merge)

################################################################################

# Score gestures as if it was sentences

# 1: n, 2: %
gw.as.sent <- score.GasS(gestures.words, sc.understands = FALSE)

gw.as.sent.wide <- gw.as.sent[[2]] %>%
                    add_column(instrument = "WG", .before = 1)

gw.as.sent.count <- gw.as.sent[[1]] %>%
                      add_column(instrument = "WG", .before = 1)

# Proportions
## TO DO here
sentences.p <- read_data("Wordbank/WS-scored.rds")[[2]] %>%
  add_column(instrument = "WS", .before = 1)

sentences.n <- read_data("Wordbank/WS-scored.rds")[[1]] %>%
  add_column(instrument = "WS", .before = 1)

all.n <- bind_rows(sentences.n, gw.as.sent.count) %>%
  dplyr::select(instrument, data_id, age, LEXICAL, SYNTAX) %>%
  mutate_at("data_id", as.character)

# ggplot(all.n, aes(x = age, y = value, color = name)) +
#   geom_jitter(alpha = 0.25) +
#   facet_wrap(. ~ name) +
#   geom_smooth(color = "black", method = "lm")

all.p <- bind_rows(sentences.p, gw.as.sent.wide) %>%
  dplyr::select(instrument, data_id, age, LEXICAL, SYNTAX) %>%
  mutate_at("data_id", as.character)

plot(all.n$SYNTAX, all.p$SYNTAX)

ggplot(all.p, aes(x = LEXICAL, y = SYNTAX)) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = 'lm', formula = y ~ poly(x, 2), color = "red", se = TRUE)

ggplot(all.p, aes(x = (LEXICAL), y = log(SYNTAX))) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

all.p <- all.p %>%
  mutate(
    sqrt_SYNTAX = sqrt(SYNTAX)
  )


################################################################################
# Demographics
################################################################################

WG.demographics <- read_data("Wordbank/WG-demographics.rds")
WS.demographics <- read_data("Wordbank/WS-demographics.rds")


mom_ed_levels <- c("Primary", "Some Secondary", "Secondary", "Some College",
                   "College", "Some Graduate", "Graduate")

# Approximations in years
mom_ed_levels_n <- c(5, 8, 12, 14, 16, 18, 20)

# Whether mom finished college (HS v. not is very low: 200 vs 2000)
mom_bachelors <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)

mom_lut <- cbind(mom_ed = mom_ed_levels,
                 mom_ed_n = mom_ed_levels_n,
                 mom_bachelors = mom_bachelors)

demo <- bind_rows(WG.demographics, WS.demographics) %>%
  left_join(mom_lut, by = "mom_ed", copy = TRUE) %>%
  mutate_at("data_id", as.character)

count.demo <- left_join(all.n, demo, by = c("data_id", "age"))

ggplot(count.demo, aes(x = LEXICAL, y = SYNTAX, color = age)) +
  geom_point(alpha = 0.1) +
  scale_color_viridis_c() +
  labs(title = "SYN ~ LEX")

# This shows both lex, lex2, age, and age2 are all significant
lm.1 <- lm(SYNTAX ~ 1 + LEXICAL + I(LEXICAL^2) + age + I(age^2),
           data = count.demo)

library(broom)
aug.1 <- augment(lm.1)

ggplot(aug.1, aes(x = LEXICAL**2,  y = .resid)) +
  geom_point() +
  geom_smooth() +
  labs("Residuals vs. lex^2")

ggplot(aug.1, aes(x = age**2,  y = .resid)) +
  geom_smooth() +
  geom_point() +
  labs("Residuals vs. age^2")


ggplot(aug.1, aes(x = LEXICAL + LEXICAL**2 + age + age**2,  y = .resid)) +
  geom_smooth() +
  geom_point() +
  labs(title = "Residuals vs. all predictors", y = "Syntax > Lexical")


count.demo2 <- count.demo %>%
  mutate(
    born_first = birth_order == "First",
    mom_ed_n = as.numeric(mom_ed_n),
    hispanic = ethnicity == "Hispanic",
    white = ethnicity == "White",
    asian = ethnicity == "Asian"
  )

count.demo2 <- count.demo2 %>%
  filter(ethnicity %in% c("Asian", "Black", "White", "Hispanic"))

lm.2 <- lm(SYNTAX ~ 1 + LEXICAL + I(LEXICAL^2) + age + I(age^2) + sex + mom_ed_n + born_first,
           data = count.demo2)

lm.3 <- lm(SYNTAX ~ 1 + LEXICAL + I(LEXICAL^2) + age + I(age^2) + sex*LEXICAL + sex*I(LEXICAL^2) + sex*age + sex*I(age^2),
           data = count.demo2)

lm.4 <- lm(LEXICAL ~ 1 + age + sex + mom_ed_n + born_first + hispanic + white + asian,
           data = count.demo2)


options(scipen = 999)
coef(lm.3) %>%
  signif(3)
options(scipen = 0)

# 11.1  - 0.0700(L) + 0.000539(L^2) - 1.66(age) + 0.0738(age^2) - 12.7(male) +
# 0.00349(male:L) - 0.00000032(male:L^2) + 1.68(age:male) - 0.0557(age^2:male)



mean_lex_age <- count.demo2 %>%
  group_by(age) %>%
  summarize(
    n = n(),
    mlex = mean(LEXICAL),
    sdlex = sd(LEXICAL),
    msyn = mean(SYNTAX)
  )

plot1 <- tibble(age = 8:30,
                intercept = coef(lm.1)[1],
                lex = coef(lm.1)[2],
                lex2 = coef(lm.1)[3],
                age_b = coef(lm.1)[4],
                age2_b = coef(lm.1)[5]) %>%
  left_join(mean_lex_age) %>%
  mutate(
    age2 = age ^2,
    mlex2 = mlex ^ 2,
    y = intercept + (lex * mlex) + (lex2 * mlex2) + (age_b * age) + (age2_b * age2)
  ) %>%
  select(age, msyn, y) %>%
  pivot_longer(-age)

ggplot(plot1, aes(x = age, y = value, color = name)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "Predicted syntax score", title = "Mean LEX at age")

male_interaction <- tibble(age = 8:30,
                           intercept = coef(lm.3)[1],
                           lex = coef(lm.3)[2],
                           lex2 = coef(lm.3)[3],
                           age_b = coef(lm.3)[4],
                           age2_b = coef(lm.3)[5],
                           male = coef(lm.3)[6],
                           lex_male = coef(lm.3)[7],
                           lex2_male = coef(lm.3)[8],
                           age_male = coef(lm.3)[9],
                           age2_male = coef(lm.3)[10]) %>%
  left_join(mean_lex_age) %>%
  mutate(
    age2  = age ^ 2,
    mlex2 = mlex ^2,
    girls = intercept + (lex * mlex) + (lex2 * mlex2) + (age_b * age) + (age2_b * age2),
    boys  = girls + male + (lex_male * lex) + (lex2_male * mlex2) + (age_male * age) + (age2_male * age2),
    g_minus_b = girls - boys
  )

plot2 <- male_interaction %>%
  select(age, girls, boys) %>%
  pivot_longer(-age, names_to = "gender")

ggplot(plot2, aes(x = age, y = value, color = gender)) +
  geom_smooth(se = FALSE)

 ggplot(male_interaction, aes(x = age, y = g_minus_b)) +
  geom_line()


################################################################################
# How much does WG-as-WS underestimate WS?
################################################################################

# Score as if all words not in gestures were 0
sent.words_WG <- sentences.words %>%
  mutate(
    value = replace(value, !(definition %in% all.g.words), NA))

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
