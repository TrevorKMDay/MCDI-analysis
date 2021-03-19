if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/code/PHE")
}

library(tidyverse)
library(data.table)
library(magrittr)

source("../growth_curving/growth-curve-functions.R")

# Parameters
kU <- 37.52
A  <- 681
kg1 <- exp(1) * kU / A

a <- seq(0, 681, length.out = 6)

predict_gomp <- function(x, kg, A, W0 = .Machine$double.eps) {

  exponent <- 1 - exp(-1 * kg * x)
  y <- W0 * ((A / W0) ^ exponent)

  return(y)

}

model <- data.frame(month = seq(0, 60, by = 1)) %>%
  mutate(
    y = predict_gomp(month, kg1, A),
    moj = jitter(month, amount = 1),
    yj = jitter(y, amount = 50)
  )

ggplot(model, aes(x = moj, y = yj)) +
  geom_point()

select_5pts <- function(month_vec, kg, mo_jitter = 1, y_jitter = 50) {

  a <- seq(0, 681, length.out = 6)

  jittered <- data.frame(month = month_vec) %>%
    mutate(
      y   = predict_gomp(month, kg, 681),
      moj = jitter(month, amount = mo_jitter),
      yj  = jitter(y, amount = y_jitter)
    )

  points <- data.frame(
      month = rep(NA, length(a) - 1),
      y = rep(NA, length(a) - 1)
    )

  for (i in seq(2, length(a))) {

    point <- jittered %>%
      filter(yj < a[i], yj > a[i - 1]) %>%
      slice_sample(n = 1)

    points[i - 1,] <- point[, c("moj", "yj")]

  }

  return(points)

}

# Generate 1000 data frames
sets <- lapply(1:1000, function(x) select_5pts(seq(0, 60), kg = kg1))

# Sets of rows to pick
combinations <- lapply(2:5, function(x) combn(1:5, x))

fit_model_to_data <- function(data, rows) {

  new_data <- data %>%
    slice(rows)

  model <- gomp2.fit(new_data, t_var = "month", response_var = "y")

  return(model)

}

## Two points

# Run models on each of the 1000 random data frames, and extract the calculated
# k_g value from the model object and unlist
two_pts <- lapply(sets, function(x)
            lapply(1:10, function(y)
              fit_model_to_data(x, combinations[[1]][, y])) %>%
                extract.kg() %>%
                unlist() )

# Extract values and plot by extracted pair of (x, y) values
results2 <- two_pts %>%
  unlist() %>%
  matrix(ncol = 10) %>%
  as.data.frame() %>%
  set_colnames(paste0("x", c("11000", "10100", "10010", "10001", "01100",
                             "01010", "01001", "00110", "00101", "00011"))) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id)

ggplot(results2, aes(x = name, y = value, fill = name)) +
  geom_hline(yintercept = kg1, size = 1) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5) +
  labs(x = "Quantiles", y = "k_g", title = "Two points") +
  scale_y_continuous(limits = c(0.1, 0.2))

## three points

three_pts <- lapply(sets, function(x)
  lapply(1:10, function(y)
    fit_model_to_data(x, combinations[[2]][, y])) %>%
    extract.kg() %>%
    unlist() )

results3 <- three_pts %>%
  unlist() %>%
  matrix(ncol = 10) %>%
  as.data.frame() %>%
  set_colnames(paste0("x", c("11100", "11010", "11001", "10110", "10101",
                             "10011", "01110", "01101", "01011", "00111"))) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id)

ggplot(results3, aes(x = name, y = value, fill = name)) +
  geom_hline(yintercept = kg1, size = 1) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5) +
  labs(x = "Quantiles", y = "k_g", title = "Three points") +
  scale_y_continuous(limits = c(0.1, 0.2))

## four points

four_pts <- lapply(sets, function(x)
  lapply(1:5, function(y)
    fit_model_to_data(x, combinations[[3]][, y])) %>%
    extract.kg() %>%
    unlist() )

results4 <- four_pts %>%
  unlist() %>%
  matrix(ncol = 5) %>%
  as.data.frame() %>%
  set_colnames(paste0("x", c("11110", "11101", "11011", "10111", "01111"))) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id)

ggplot(results4, aes(x = name, y = value, fill = name)) +
  geom_hline(yintercept = kg1, size = 1) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5) +
  labs(x = "Quantiles", y = "k_g", title = "Four points") +
  scale_y_continuous(limits = c(0.1, 0.2))

## five_pts

five_pts <- data.frame(
    n = 1:1000,
    kg = lapply(sets,
                   function(x) gomp2.fit(x, t_var = "month",
                                         response_var = "y")) %>%
            extract.kg()
  )

ggplot(five_pts, aes(x = 0, y = kg)) +
  geom_hline(yintercept = kg1, size = 1) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5) +
  labs(x = "Quantiles", y = "k_g", title = "Five points") +
  scale_y_continuous(limits = c(0.1, 0.2))

################################################################################

kgs <- seq(0.1, 0.2, by = 0.01)

# List of lists
sets2 <- lapply(kgs, function(kg)
            lapply(1:1000, function(x) select_5pts(seq(0, 60), kg = kg)))

calculate_kg <- function(set, kg, points) {

  if (points == 3){
    combin <- combinations[[2]]
    combin_colnames <- c("11100", "11010", "11001", "10110", "10101", "10011",
                         "01110", "01101", "01011", "00111")
  } else if (points == 4) {
    combin <- combinations[[3]]
    combin_colnames <- c("11110", "11101", "11011", "10111", "01111")

  }

  combin_ncol <- ncol(combin)

  pts <- lapply(set, function(x)
          lapply(1:combin_ncol, function(y)
            fit_model_to_data(x, combin[, y])) %>%
            extract.kg() %>%
            unlist() )

  results <- pts %>%
    unlist() %>%
    matrix(ncol = combin_ncol) %>%
    as.data.frame() %>%
    set_colnames(paste0("x", combin_colnames)) %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id)

  return(results)

}

all_kg_estimates_3 <- lapply(sets2,
                             function(x) calculate_kg(x, kg = 0.15, points = 3))

summary_kg_3 <- data.frame(
    kg = kgs,
    mean_kg = sapply(all_kg_estimates_3, function(x) mean(x$value,
                                                          na.rm = TRUE)),
    sd_kg   = sapply(all_kg_estimates_3, function(x) sd(x$value, na.rm = TRUE))
  ) %>%
  filter(kg < 0.2)


all_kg_estimates_4 <- lapply(sets2,
                             function(x) calculate_kg(x, kg = 0.15, points = 4))

summary_kg_4 <- data.frame(
    kg = kgs,
    mean_kg = sapply(all_kg_estimates_4, function(x) mean(x$value,
                                                          na.rm = TRUE)),
    sd_kg   = sapply(all_kg_estimates_4, function(x) sd(x$value, na.rm = TRUE))
  ) %>%
  filter(kg < 0.2)

################################################################################
# Plots

ggplot() +
  geom_point(data = summary_kg_3, aes(x = kg, y = kg - mean_kg)) +
  geom_line(data = summary_kg_3, aes(x = kg, y = kg - mean_kg)) +
  geom_smooth(data = summary_kg_3, aes(x = kg, y = kg - mean_kg)) +
  geom_point(data = summary_kg_4, aes(x = kg, y = kg - mean_kg), color = "red") +
  geom_line(data = summary_kg_4, aes(x = kg, y = kg - mean_kg), color = "red") +
  geom_smooth(data = summary_kg_4, aes(x = kg, y = kg - mean_kg), color = "red",
              fill = "red")  +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks = kgs) +
  theme_bw()

ggplot() +
  geom_point(data = summary_kg_3, aes(x = kg, y = kg - mean_kg)) +
  geom_line(data = summary_kg_3, aes(x = kg, y = kg - mean_kg)) +
  geom_errorbar(data = summary_kg_3,
                aes(x = kg, ymin = kg - mean_kg - sd_kg,
                    ymax = kg - mean_kg + sd_kg),
                width = 0.002) +
  geom_point(data = summary_kg_4, aes(x = kg, y = kg - mean_kg), color = "red") +
  geom_line(data = summary_kg_4, aes(x = kg, y = kg - mean_kg), color = "red") +
  geom_errorbar(data = summary_kg_4,
                aes(x = kg, ymin = kg - mean_kg - sd_kg,
                    ymax = kg - mean_kg + sd_kg),
                width = 0.002, color = "red") +
  scale_x_continuous(breaks = kgs) +
  theme_bw()


all_kg_3 <- rbindlist(all_kg_estimates_3) %>%
  mutate(
    kg = rep(kgs, each = 10000),
    diff = value - kg,
    pts = 3,
  ) %>%
  na.omit()

all_kg_4 <- rbindlist(all_kg_estimates_4) %>%
  mutate(
    kg = rep(kgs, each = 5000),
    diff = value - kg,
    pts = 4
  ) %>%
  na.omit()

all_kg <- bind_rows(all_kg_3, all_kg_4)

ggplot(all_kg, aes(x = as.factor(kg), y = value, fill = as.factor(pts))) +
  geom_violin() +
  scale_y_continuous(limits = c(NA, 0.25)) +
  labs(x = "true kg", y = "modeled kg")

################################################################################

# More extreme jitter (x3)

extreme_sets <- lapply(1:1000,
                       function(x) select_5pts(seq(0, 60), kg = 0.15,
                                               mo_jitter = 1,
                                               y_jitter = 130 / 2))

extreme_estimates <- lapply(extreme_sets, function(x)
  lapply(1:5, function(y)
    fit_model_to_data(x, combinations[[3]][, y])) %>%
    extract.kg() %>%
    unlist() )

results_extreme_4 <- extreme_estimates %>%
  unlist() %>%
  matrix(ncol = 5) %>%
  as.data.frame() %>%
  set_colnames(paste0("x", c("11110", "11101", "11011", "10111", "01111"))) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id)

ggplot(results_extreme, aes(x = name, y = value, fill = name)) +
  geom_hline(yintercept = kg1, size = 1) +
  geom_violin(alpha = 0.5, draw_quantiles = 0.5) +
  labs(x = "Quantiles", y = "k_g", title = "Four points") +
  scale_y_continuous(limits = c(0.1, 0.2))
