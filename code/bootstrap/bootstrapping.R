require("tidyverse")
require("optparse")

require("parallel")
require("doParallel")
require("foreach")

if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis/bootstrap")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/bootstrap")
}

if (!interactive()) {
  
  # If being called as an Rscript, do to bootstrapping

  #
  # Options
  #
  
  option_list <- list(
    make_option(c("-n", "--n_iter"), type = "integer", default = 100,
                help = "Number of iterations (default 1000)",
                metavar = "character"),
    make_option(c("-c", "--n_cores"), type = "integer",
                default = detectCores() - 1,
                help = "Number of cores to use (default is to detect - 1)",
                metava = "character"),
    make_option(c("-o", "--output"), type = "character", 
                default = "bootstrap_",
                help = "Output prefix  (default 'bootstrap_')",
                metavar = "character")
  );
  
  opt_parser <- OptionParser(option_list = option_list);
  opt <- parse_args(opt_parser);
  
  #
  # Set up parallelism
  #
  
  registerDoParallel(opt$n_cores)
  
  #
  # Functions
  #
  
  gomp2.fit <- function(data, response = "inventory", max = 681,
                        max.iter = 50) {
  
    A <- max
    W_0 <- .Machine$double.eps
  
    # This is the formula that works best to solve in R
    # 19 refers to its number in the Tjorve and Tjorve paper
    fit19 <- NULL
    try( fit19 <- nls(as.formula(paste(response,
                                       "~ W_0 * (A / W_0) ^ (1 - exp(-k_g * age))")),
                      data = data,
                      start = list(k_g = .1),
                      control = list(maxiter = max.iter)) )
  
    if (is.null(fit19))
      fit19 <- NA
  
    return(fit19)
  
  }
  
  # Solve for a given X or Y, given kg
  solve.gomp2 <- function(x = NA, y = NA, k_g) {
  
    A <- 680 + 1
    W_0 <- .Machine$double.eps
  
    if (is.na(y) & !is.na(x))
      result <-  W_0 * (A / W_0) ^ (1 - exp(-k_g * x))
    else if (is.na(x) & !is.na(y))
      result <- -log(1 - log(y / W_0) / log(A / W_0)) / k_g
    else
      message("Exactly one of x/y must be specified")
  
    return(result)
  
  }
  
  
  #
  # Actual processing
  #
  
  BCP.WS <- readRDS("data/BCP_WS_scored.rds")[[1]]
  BCP.WG <- readRDS("data/BCP_WG_scored.rds")[[1]]
  
  # Everyone's ns
  BCP.MCDI <- rbind.fill(BCP.WS, BCP.WG) %>%
    select(-how_use_words) %>%
    mutate_all(~replace_na(.x, 0)) %>%
    arrange(data_id, age)
  
  BCP.summarize <- BCP.MCDI %>%
    group_by(data_id, age) %>%
    mutate(sum = rowSums(.[-(1:2)])) %>%
    ungroup() %>% group_by(data_id) %>%
    dplyr::summarise(n = n(),
                     max = max(sum))
  
  # Anyone with 3 or more points, who reached at least 125 words (680/(2e))
  usable <- filter(BCP.summarize, n >=3, max >= 125)$data_id
  BCP.MCDI.usable <- filter(BCP.MCDI, data_id %in% usable)
  
  ncol <- 28         # Number of subcategories
  size.of.A <- 15    # How many in larger group
  combinations <- choose(ncol, size.of.A)
  
  columns <- combn(ncol, size.of.A)
  bootstrap.results <- rep(NA, combinations)
  
  est.time <- .4 * opt$n_iter / opt$n_cores
  
  message(paste(opt$n_iter, "samples on", opt$n_cores))
  message(paste("Estimated completion time:", est.time, "sec"))
  
  # Value to estimate difference at (1/exp(1)=e)
  Wi <- 1 / exp(1)
  
  bootstrap.results <- foreach (i=round(seq(1, combinations,
                                            length.out = opt$n_iter)),
                                .combine = c) %dopar% {
  
    # Select which columns are in which group
    columnsA <- columns[, i]
    columnsB <- (1:ncol)[!(1:ncol %in% columnsA)]
  
    ids <- BCP.MCDI.usable[, 1:2]
  
    # Separate, except drop data_id,age
    A <- BCP.MCDI.usable[, columnsA + 2]
    B <- BCP.MCDI.usable[, columnsB + 2]
  
    max.A <- sum(apply(A, 2, max))
    max.B <- sum(apply(B, 2, max))
  
    perc.A <- apply(A, 1, sum) / max.A
    perc.B <- apply(B, 1, sum) / max.B
  
    # Percentage in each category
    result <- ids %>%
      add_column(perc.A = perc.A) %>%
      add_column(perc.B = perc.B)
  
    # Split into list to operate on with *apply
    results <- result %>%
      split(., f = .$data_id)
  
    # Need to load gomp2.fit() function
    results.A <- lapply(results, gomp2.fit, response = "perc.A", max = max.A)
    results.B <- lapply(results, gomp2.fit, response = "perc.B", max = max.B)
  
    # k_g values for both models
    # Need to load solve.gomp2()
    model.results <-  data.frame(data_id = unique(result$data_id)) %>%
      mutate(A.kg = sapply(results.A, coef),
             B.kg = sapply(results.B, coef),
             A.Ti = solve.gomp2(y = Wi, k_g = A.kg),
             B.Ti = solve.gomp2(y = Wi, k_g = B.kg),
             diff = B.Ti - A.Ti)
  
    mean(model.results$diff)
    # message(paste0(i, "/", combinations))
  
  }
  
  # End parallelism
  stopImplicitCluster()
  
  # Save numerical result
  bsr.df <- data.frame(bootstrap.results = bootstrap.results) %>% na.omit()
  saveRDS(file = paste0(opt$out, "results.RDS"), object = bsr.df)
  
  plot.pfx <- opt$out
  
} else {
  
  files <- list.files(pattern = "bs.*results.RDS")
  
  # To do: handle multiple files, would want to use the biggest
  if (length(files) == 1) 
    the.file <- files
  
  bsr.df <- readRDS(the.file)
  
  plot.pfx <- gsub("results.RDS", "", the.file)
  
}

# Save plot
# Red/dashed = normal fit
p <- ggplot(bsr.df, aes(bootstrap.results)) +
      geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.7) +
      geom_density(size = 1) +
      stat_function(fun = dnorm,
                    args = list(mean = mean(bsr.df[, 1]), 
                                sd = sd(bsr.df[, 1])),
                    color = "red", size = 1, linetype = 5) +
      labs(x = "T_i,syn - T_i,lex")

mu <- mean(bsr.df[, 1])
se <- sd(bsr.df[,1]) / sqrt(nrow(bsr.df)) * 1.96

mu + c(-se, se)

png(filename = paste0(plot.pfx, "plot.png"), width = 5, height = 5, 
    units = "in", res = 300)
print(p)
dev.off()

1 - pnorm(q = 4.16,
          mean = mean(bsr.df$bootstrap.results),
          sd = sd(bsr.df$bootstrap.results))

