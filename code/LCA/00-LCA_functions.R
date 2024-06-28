get_fit_stats <- function(g1_model = NA, list_of_models, extra = NULL) {

  if (length(g1_model) > 0) {
    g1_model <- list_of_models[[1]]
    list_of_models <- list_of_models[-1]
  }

  n_models <- 1 + length(list_of_models)

  # Perform model selection
  post <- lapply(list_of_models, function(x) x$pprob[, -c(1, 2)])
  lcga_entropy <- sapply(post, function(x) sum(-1 * x * log(x)))

  g1_param <- length(g1_model$best)
  list_param <- sapply(list_of_models, function(x) length(x$best))

  # Fit indices
  # computing fit indices for class enumeration
  fit1 <- tibble(

      K       = 1:n_models,

      # Get number of participants in each model
      n       = c(g1_model$ns,
                  sapply(list_of_models, function(x) x$ns)),

      # Number of parameters in the model
      p       = c(g1_param, list_param),

      # Log-likelihood
      LL      = c(g1_model$loglik,
                  sapply(list_of_models, function(x) x$loglik)),

      # Measures classification uncertainty: 0 when all pprobs are 0/1, gets
      # bigger as uncertainty increases
      entropy = c(0, lcga_entropy),

      # Flag main vs. extra
      group   = "main",
    )

  if (length(extra) > 0) {

    for (i in extra)

      post <- i$pprob[, -c(1, 2)]
      e <- sum(-1 * post * log(post))

      fit1 <- fit1 %>%
        add_row(
          K = i$ng,
          n = i$ns,
          p = length(i$best),
          LL = i$loglik,
          entropy = e,
          group = "extra"
        )


  }

  fit2 <- fit1 %>%
    mutate(

      # AIC has no penalty and favors more classes
      AIC     = -2*LL + 2*p,

      # Penalizes for model complexity
      #  - BIC favors fewer classes
      BIC     = -2*LL + p*log(n),

      # Corrected AIC for small sample sizes
      #  - CAIC favors fewer classes as well
      CAIC    = -2*LL + p*(log(n) + 1),

      # Sample-size corrected BIC
      #  - for small sample sizes
      ssBIC   = -2*LL + p*log((n + 2) / 24),

      # Classification-likelihood criterion penalizes poor entropy
      CLC     = -2*LL + 2*entropy,

      # Integrated completed likelihood criterion penalizes for poor entropy
      # and model complexity
      ICL.BIC = -2*LL + p*log(n) + 2*entropy,

      # Smaller is better - 0 is perfect clustering, undefined for 1 group.
      # Suggest that if no values are <1, there's no clusters
      NEC     = entropy / (LL - LL[1]),

    )

  return(fit2)

}

plot_fit_stats <- function(fit_stats, plot_NEC = TRUE, text_size = 11,
                           title = NULL, bg = "#ffffff") {

  require(patchwork)

  maxK <- max(fit_stats$K)

  extra <- fit_stats %>%
    filter(
      group == "extra"
    ) %>%
    pivot_longer(cols = c(AIC, BIC, CAIC, ssBIC, CLC, ICL.BIC, NEC))

  fit_stats <- fit_stats %>%
    filter(
      group == "main"
    )

  fit_long <- pivot_longer(fit_stats,
                           cols = c(AIC, BIC, CAIC, ssBIC, CLC, ICL.BIC, NEC))

  fit_plot1 <- ggplot(filter(fit_long, name != "NEC"),
                      aes(x = K, y = value, color = name)) +
    geom_line() +
    geom_point(data = filter(extra, name != "NEC")) +
    scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
    theme_bw() +
    theme(text = element_text(size = text_size),
          plot.background = element_rect(fill = bg, color = NA),
          legend.position = "bottom") +
    labs(title = title)

  if (plot_NEC) {

    fit_plotNEC  <- ggplot(filter(fit_long, name == "NEC"),
                           aes(x = K, y = value, color = name)) +
      geom_line() +
      geom_point(data = filter(extra, name == "NEC")) +
      scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_bw() +
      theme(text = element_text(size = text_size),
            legend.position = "none") +
      labs(title = title)

    final_plot <- fit_plot1 + fit_plotNEC

  } else {

    final_plot <- fit_plot1

  }

  return(final_plot)

}

calc_ass <- function(df, column, dx_class) {

  result <- df %>%
    filter(
      status %in% c("lg_dx", "no_lg_dx")
    ) %>%
    mutate(
      TP =  ((!!as.name(column)) %in% dx_class) & (status == "lg_dx"),
      TN = !((!!as.name(column)) %in% dx_class) & (status == "no_lg_dx"),
      FP =  ((!!as.name(column)) %in% dx_class) & (status == "no_lg_dx"),
      FN = !((!!as.name(column)) %in% dx_class) & (status == "lg_dx")
    ) %>%
    ungroup() %>%
    select(matches("[TF][PN]")) %>%
    colSums()

  acc  <- unname((result["TP"] + result["TN"]) / sum(result))
  sens <- unname(result["TP"] / (result["TP"] + result["FN"]))
  spec <- unname(result["TN"] / (result["TN"] + result["FP"]))

  return(c("accuracy" = acc, "sensitivity" = sens, "specificity" = spec))

}

ass2str <- function(ass) {

  rounded <- round(ass, 2)

  string <- paste(c("acc =", "sens =", "spec ="), rounded) %>%
              paste(collapse = "; ")

  return(string)

}
