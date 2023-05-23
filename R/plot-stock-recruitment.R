plot_stock_recruitment <- function(model,
                                   probs = c(0.025, 0.5, 0.975)){

  yrs <- start_yr:end_yr

  b <- seq(0, 1.5, 0.005)
  h <- model$mcmc |>
    pull(SR_BH_steep)

  beverton_holt <- \(h, b = seq(0, 1.5, 0.005)){
    (4.0 * h * b) / ((1.0 - h) + (5.0 * h - 1.0) * b)
  }

  r_mat <- matrix(NA,
                  nrow = nrow(model$mcmc),
                  ncol = length(b))

  for(i in model$mcmc$Iter){
    r_mat[i, ] <- beverton_holt(h[model$mcmc$Iter == i])
  }

  r_quants <- apply(r_mat, 1, quantile, probs = probs)
  # Bias adjustment
  sigma_r <- model$sigma_R_in
  adj <- exp(-0.5 * sigma_r ^ 2)

  # Get quantities of interest and calculate lognormal distribution
  # over a range of values
  ymax <- 7
  r_vec <- seq(0, ymax, length = 1000)
  meanlog <- (sigma_r ^ 2) / 2
  dlnorm_vec <- dlnorm(x = r_vec,
                       meanlog = meanlog,
                       sdlog = sigma_r)

  # Right-hand plot - dlnorm distribution (right panel)
  y_breaks <- c(0, 1, 2.5)
  d <- tibble(r_vec = r_vec,
              dlnorm_vec = dlnorm_vec)
  plot_lst <- list()
  plot_lst[[2]] <- ggplot(d) +
    geom_ribbon(aes(y = r_vec,
                    xmin = 0,
                    xmax = dlnorm_vec),
                fill = "grey80") +
    geom_segment(aes(x = 0,
                     xend = dlnorm(x = 1,
                                   meanlog = meanlog,
                                   sdlog = sigma_r),
                     y = 1,
                     yend = 1),
                 color = "red") +
    geom_segment(aes(x = 0,
                     xend = dlnorm(x = 2.5,
                                   meanlog = meanlog,
                                   sdlog = sigma_r),
                     y = 2.5,
                     yend = 2.5)) +
    scale_y_continuous(breaks = y_breaks,
                       limits = c(0, 7),
                       expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    xlab("") +
    ylab("") +
    annotate("text",
             label = "Expected distribution of absolute recruitments",
             x = 0.2,
             y = 5,
             angle = 90)

  # Stock-recruitment part of the plot (left panel)
}