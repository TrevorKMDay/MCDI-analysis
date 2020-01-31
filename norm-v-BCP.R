library(tidyverse)
library(wordbankr)

WS.norms <- get_administration_data(language = "English (American)", 
                                    form = "WS")

age.norms <- WS.norms %>%
                group_by(age) %>%
                summarise(n = n(), mean = mean(production) / 680, 
                          sd = sd(production) / 680,
                          se = sd / sqrt(n))

ggplot(WS.norms, aes(x = age, y = production / 680)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.1) +
  geom_boxplot(aes(group = age), alpha = 0.75, outlier.shape = NA, notch = TRUE)

##

# our data
sI.sum <- apply(sI, 1, sum) %>% 
            as.data.frame() %>%
            add_column(age = syntax$demo.age, .before = 1) %>%
            add_column(norm = FALSE)
colnames(sI.sum)[2] <- "production"

merge <- WS.norms %>%
          select(age, production) %>%
          add_column(norm = TRUE) %>%
          rbind(., sI.sum)

# Plot our participants against the wordbank norm
# Set normed to A=0.2, BCP to A=0.9

ggplot(merge, aes(x = age, y = production / 680, color = norm)) +
  geom_jitter(aes(alpha = !norm), width = 0.25, height = 0) +
  scale_alpha_discrete(range = c(0.2, 0.9)) +
  geom_boxplot(aes(group = interaction(age, norm)), alpha = 0.75, 
               outlier.shape = NA) +
  geom_smooth(method = "lm", fullrange = TRUE)

mean.regression <- lm(mean ~ age, data = age.norms)
sd.regression <- lm(sd ~ age, data = age.norms)

saveRDS(mean.regression, file = "normmean.rds")
saveRDS(sd.regression, file = "sdmean.rds")
          
# ggplot(age.norms, aes(x = age)) +
#   geom_point(aes(y = mean), color = "blue") +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
#   geom_smooth(aes(y = mean), method = "lm", fullrange = TRUE) +
#   geom_point(aes(y = sd), color = "red") +
#   geom_smooth(aes(y = sd), method = "lm", fullrange = TRUE, color = "red") +
#   scale_x_continuous(limits = c(NA, 38))
# 
#                 