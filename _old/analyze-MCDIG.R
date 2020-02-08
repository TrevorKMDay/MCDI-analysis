################################################################################
################################################################################

library(ggrepel)

mcdi.g.d.labels <- c("sounds", "animals", "vehicles", "toys", "food",
                     "clothing", "body", "furniture", "items", "outside",
                     "people", "games", "actions", "time", "descriptive",
                     "pronouns", "questions", "prepositions", "quantifiers")

# Clean MCDI G

did.gestures <- mcdi.gestures$gestures.Administration == "All"
did.gestures[is.na(did.gestures)] <- FALSE
mcdi.G <- mcdi.gestures[did.gestures, ] %>%
            select_all(~gsub("demo.", "", .)) %>%
            select_all(~gsub("gestures.", "", .))

table(mcdi.G$CandID) %>% table()

score.mcdi.G.D <- function(x, include.understand = TRUE) {

  understand_only_score <- ifelse(include.understand, 1, 0)

  # Code not answered as 0, understands as 1, and says/understands as 2
  val <- ifelse(x == "not_answered", 0,
                ifelse(x == "understands", understand_only_score, 2))

  return(val)

}

mcdi.G.D.totals <- c(12, 36, 9, 8, 30, 19, 20, 24, 36, 27, 20, 19, 55, 8, 37,
                     11, 6, 11, 8)

# Weight "understands" as 1 ####################################################

mcdi.G.D <- lapply(1:19,
                   function(x) select(mcdi.G,
                                      starts_with(paste0("I_D_", x, "_")))) %>%
            lapply(function(x) mutate_all(.tbl = x, .funs = score.mcdi.G.D)) %>%
            sapply(function(x) apply(x, 1, sum, na.rm = TRUE)) %>%
            as_tibble()

mcdi.G.D.perc <- apply(mcdi.G.D, 1, function(x) x / (mcdi.G.D.totals * 2)) %>%
                  t() %>%
                  as_tibble()
colnames(mcdi.G.D.perc) <- paste0("I_D_", formatC(1:19, width = 2,
                                                  format = "d", flag = "0"))

colnames(mcdi.G.D.perc) <- mcdi.g.d.labels
mcdi.G.D.labels <- mcdi.G.D.perc %>%
                    add_column(CandID = as.factor(mcdi.G$CandID),
                               .before = 1) %>%
                    add_column(age = as.numeric(mcdi.G$Candidate_Age),
                                .after = 1) %>%
                    filter(!is.na(age))

mcdi.G.D.long <- mcdi.G.D.labels %>%
                  pivot_longer(c(-CandID, -age), names_to = "category",
                               values_to = "value")

################################################################################

# Weight "understands" as 1 ####################################################

mcdi.G.D.2 <- lapply(1:19,
                   function(x) select(mcdi.G,
                                      starts_with(paste0("I_D_", x, "_")))) %>%
              lapply(function(x) mutate_all(.tbl = x, .funs = score.mcdi.G.D,
                                            include.understand = FALSE)) %>%
              sapply(function(x) apply(x, 1, sum, na.rm = TRUE)) %>%
              as_tibble()

mcdi.G.D.2.perc <- apply(mcdi.G.D.2, 1, function(x) x / (mcdi.G.D.totals * 2)) %>%
                    t() %>%
                    as_tibble()

colnames(mcdi.G.D.2.perc) <- paste0("I_D_", formatC(1:19, width = 2,
                                                  format = "d", flag = "0"))

colnames(mcdi.G.D.2.perc) <- mcdi.g.d.labels
mcdi.G.D.2.labels <- mcdi.G.D.2.perc %>%
                      add_column(CandID = as.factor(mcdi.G$CandID),
                                 .before = 1) %>%
                      add_column(age = as.numeric(mcdi.G$Candidate_Age),
                                 .after = 1) %>%
                      filter(!is.na(age))

mcdi.G.D.2.long <- mcdi.G.D.2.labels %>%
  pivot_longer(c(-CandID, -age), names_to = "category",
               values_to = "value")

################################################################################

mcdi.G.D.und <- mcdi.G.D.long %>%
                  add_column(no_understands = mcdi.G.D.2.long$value) %>%
                  mutate(category = as.factor(category))

ggplot(mcdi.G.D.und, aes(x = age)) +
  stat_smooth(aes(y = value), method = "lm", formula = y ~ poly(x, 2),
              color = "darkred", fill = "pink") +
  stat_smooth(aes(y = no_understands), method = "lm", formula = y ~ poly(x, 2),
              color = "blue", fill = "blue") +
  facet_wrap(vars(category))

# Fits

fits.SU <- lapply(levels(mcdi.G.D.und$category),
                   function(x) lm(value ~ poly(age, 2),
                                  data = filter(mcdi.G.D.und, category == x))) %>%
              sapply(function(x) summary(x)$r.squared)

fits.U <- lapply(levels(mcdi.G.D.und$category),
                  function(x) lm(no_understands ~ poly(age, 2),
                                 data = filter(mcdi.G.D.und, category == x)))%>%
              sapply(function(x) summary(x)$r.squared)

# mcdi.G.D.age <- mcdi.G.D.labels %>%
#                   group_by(age) %>%
#                   select(-CandID) %>%
#                   summarize_all(list(mean = mean, SD = sd)) %>%
#                   add_column(n = table(mcdi.G.D.labels$age), .after = "age") %>%
#                   mutate_at(vars(ends_with("SD")),
#                             funs(SE = . / sqrt(n))) %>%
#                   select(-ends_with("SD")) %>%
#                   filter(n > 3)
#
# mcdi.G.D.age.mean <- mcdi.G.D.age %>%
#                       select(age, -n, ends_with("mean")) %>%
#                       melt(id.vars = "age") %>%
#                       mutate(variable = gsub("_mean", "",
#                                              as.character(variable)))  %>%
#                       rename(mean = value)
#
# mcdi.G.D.age.SE <- mcdi.G.D.age %>%
#                     select(age, -n, ends_with("SE")) %>%
#                     melt(id.vars = "age") %>%
#                     mutate(variable = gsub("_SD", "",
#                                            as.character(variable))) %>%
#                     rename(SE = value)
#
# mcdi.G.D.age.mean <- mcdi.G.D.age.mean %>%
#                       add_column(se = mcdi.G.D.age.SE$SE)
#
# ggplot(mcdi.G.D.age.mean,
#        aes(x = age, y = mean, fill = variable)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.5)
