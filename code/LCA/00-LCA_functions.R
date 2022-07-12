get_fit_stats <- function(g1_model, list_of_models) {

  n_models <- 1 + length(list_of_models)

  # Perform model selection
  post <- lapply(list_of_models, function(x) x$pprob[, -c(1, 2)])
  lcga_entropy <- sapply(post, function(x) sum(-1 * x * log(x)))

  g1_param <- length(g1_model$best)
  list_param <- sapply(list_of_models, function(x) length(x$best))

  # Fit indices
  #computing fit indices for class enumeration
  fit <- tibble(

    K       = 1:n_models,

    # Number of participants should be constant
    n       = rep(g1_model$ns, n_models),

    # Number of parameters in the model
    p       = c(g1_param, list_param),

    # Log-likelihood
    LL      = c(g1_model$loglik,
                sapply(list_of_models, function(x) x$loglik)),

    # Measures classification uncertainty: 0 when all pprobs are 0/1, gets
    # bigger as uncertainty increases
    entropy = c(0, lcga_entropy)
  ) %>%
    mutate(

      # AIC has no penalty and favors more classes
      AIC     = -2*LL + 2*p,

      # Penalizes for model complexity
      # BIC favores fewer classes
      BIC     = -2*LL + p*log(n),

      # Corrected AIC for small sample sizes
      # CAIC favores fewer classes as well
      CAIC    = -2*LL + p*(log(n) + 1),

      # Sample-size corrected BIC - for small sample sizes
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

  return(fit)

}

plot_fit_stats <- function(fit_stats, plot_NEC = TRUE, text_size = 11,
                           title = NULL, bg = "#ffffff") {

  maxK <- max(fit_stats$K)

  fit_long <- pivot_longer(fit_stats,
                           cols = c(AIC, BIC, CAIC, ssBIC, CLC, ICL.BIC, NEC))

  fit_plot1 <- ggplot(filter(fit_long, name != "NEC"),
                      aes(x = K, y = value, color = name)) +
    geom_line() +
    scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
    theme_bw() +
    theme(text = element_text(size = text_size),
          plot.background = element_rect(fill = bg, color = NA)) +
    labs(title = title)

  if (plot_NEC) {

    fit_plotNEC  <- ggplot(filter(fit_long, name == "NEC"),
                           aes(x = K, y = value, color = name)) +
      geom_line() +
      scale_x_continuous(limits = c(1, maxK), breaks = 1:maxK) +
      theme_bw() +
      theme(text = element_text(size = text_size)) +
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
