
library(tidyverse)
library(growthcurver)

#
# Setup
#

test <- read_csv("data/test-subj.csv") %>%
          arrange(age)

# Inventory theoretical max
max_inv <- 680 + 1

################################################################################
# Use all 5

#
# Gompertz
#

# SG() from growthcurver
model.wt <- SummarizeGrowth(test$age, test$inventory)

extrap.gomp <- function(model, range = 0:40) {

  K <- model$vals$k
  N0 <- model$vals$n0
  r <- model$vals$r

  fit <- K / (1 + ((K - N0) / N0) * exp(-r * range))

  return(as.data.frame(cbind(range, fit)))

}


fit <- extrap.gomp(model.wt, 0:60)

ggplot(NULL) +
  geom_line(data = test, aes(x = age, y = inventory), size = 1) +
  geom_line(data = fit, aes(x = range, y = fit), color = "blue", size = 2) +
  scale_y_continuous(limits = c(0, 680)) +
  geom_hline(yintercept = 680) +
  scale_x_continuous(limits = c(0, 60))

#
# nls logistic
#

model <- nls(test$inventory ~ max_inv/(1 + exp(-1 * k * (age - x0))),
             data = test,
             start = list(k = .3, x0 = 20))

extrap.logi <- function(model, range) {

  k <- summary(model)$parameters[1, 1]
  x0 <- summary(model)$parameters[2, 1]

  fit <- max_inv/(1 + exp(-1 * k * (range - x0)))

  return(as.data.frame(cbind(range, fit)))

}

logi_fit <- extrap.logi(model, 0:60)

ggplot(NULL) +
  geom_line(data = test, aes(x = age, y = inventory), size = 1) +
  geom_line(data = fit, aes(x = range, y = fit), color = "blue", size = 2) +
  geom_line(data = logi_fit, aes(x = range, y = fit), color = "red", size = 2) +
  scale_y_continuous(limits = c(0, 680)) +
  geom_hline(yintercept = 680) +
  scale_x_continuous(limits = c(0, 60))

#
# Gompertz with max
#

# The maximum model also takes a minimum, which can't be 0, so we use the
# smallest value representable by the computer

min_inv <- .Machine$double.eps

model.g2 <- nls(test$inventory ~ min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b * age))),
                 data = test,
                 start = list(b = 0.11))

extrap.gomp2 <- function(model, range, se = FALSE) {

  b <- summary(model)$parameters[1, 1]

  fit <- min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b * range)))

  if (se) {

    se <- summary(model)$parameters[1, 2]
    b <- c(b - se, b + se)

    fit1 <- min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b[1] * range)))
    fit2 <- min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b[2] * range)))

    out1 <- as.data.frame(cbind(range, fit1))
    out <- as.data.frame(cbind(range, fit))
    out2 <- as.data.frame(cbind(range, fit2))

    colnames(out1)[2] <- colnames(out2)[2] <- "fit"

    return(list(out1, out, out2))

  } else {

    return(as.data.frame(cbind(range, fit)))

  }

}

g2_fit <- extrap.gomp2(model.g2, seq(0, 60, by = 0.1), se = TRUE)

ggplot(NULL) +
  geom_point(data = test, aes(x = age, y = inventory), size = 2) +
  geom_line(data = g2_fit[[2]], aes(x = range, y = fit), color = "blue",
            size = 1) +
  geom_ribbon(aes(x = g2_fit[[2]]$range, ymin = g2_fit[[1]]$fit,
                  ymax = g2_fit[[3]]$fit), alpha = 0.5) +
  scale_y_continuous(limits = c(0, 680)) +
  geom_hline(yintercept = c(680, 340)) +
  scale_x_continuous(limits = c(10, 40), breaks = 10:40)

lower <- (g2_fit[[1]]$fit - 480) %>%
          abs() %>%
          which.min()

upper <- (g2_fit[[3]]$fit - 480) %>%
  abs() %>%
  which.min()

lower.val <- g2_fit[[1]][lower, 1]
upper.val <- g2_fit[[3]][upper, 1]

diff <- abs(upper.val - lower.val) * 1.96
diff_days <- diff * 30

################################################################################
# Use first ones

test2 <- test[1:2, ]
test3 <- test[1:3, ]

model.g2_small <- nls(inventory ~ min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b * age))),
                      data = test2,
                      start = list(b = 0.11))

g2s_fit <- extrap.gomp2(model.g2_small, seq(0, 60, by = 0.1))

model.g2_3 <- nls(inventory ~ min_inv * exp(log(max_inv / min_inv) * (1 - exp(-1 * b * age))),
                      data = test3,
                      start = list(b = 0.11))

g2_3_fit <- extrap.gomp2(model.g2_3, seq(0, 60, by = 0.1))

ggplot(NULL) +
  geom_point(data = test, aes(x = age, y = inventory), size = 2) +
  geom_line(data = g2_fit[[2]], aes(x = range, y = fit), color = "blue",
            size = 1) +
  geom_ribbon(aes(x = g2_fit[[2]]$range, ymin = g2_fit[[1]]$fit,
                  ymax = g2_fit[[3]]$fit), alpha = 0.5) +
  scale_y_continuous(limits = c(0, 680)) +
  geom_hline(yintercept = c(680, 340)) +
  scale_x_continuous(limits = c(10, 36), breaks = 10:40) +
  geom_line(data = g2s_fit, aes(x = range, y = fit), color = "green",
            size = 1) +
  geom_line(data = g2_3_fit, aes(x = range, y = fit), color = "red", size = 1)
