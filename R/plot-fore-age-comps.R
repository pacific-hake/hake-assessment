#' Create a two-panel plot showing the forecasted age compositions by
#' numbers and weight for the first forecast year (`endyr` + 1)
#'
#' @param model  A model object, created by [create_rds_file()]
#' @param x_lim A vector of 2 representing the minimum and maximum x-axis
#' values to plot
#' @param bar_alpha The transparency of the bars from 0 to 1
#' @param probs A vector of three quantiles for the lower CI,
#' median, and upper CI. The second value must be 0.5
#' @param x_breaks A vector of ages to show labels for on the x-axis
#' @param whisker_width The width of the top and bottom crossbars
#' (whiskers) on the error bars
#'
#' @return A [cowplot::plot_grid()] object
#' @export
plot_fore_age_comps <- function(model,
                                probs = c(0.025, 0.5, 0.975),
                                x_lim = c(1, 15),
                                x_breaks = seq(x_lim[1], x_lim[2], 2),
                                bar_alpha = 0.3,
                                whisker_width = 0.5){

  if(length(probs) != 3){
    stop("The `probs` vector must be length 3, composed of the lower CI ",
         "median, and upper CI",
         call. = FALSE)
  }
  if(probs[2] != 0.5){
    stop("The middle value in `probs` must be 0.5 (median)",
         call. = FALSE)
  }
  natsel_prop <- model$extra_mcmc$natsel.prop
  natselwt_prop <- model$extra_mcmc$natselwt.prop

  perc <- paste0(probs[c(1, 3)] * 100, "%")

  reformat <- function(df){
    df |>
      apply(2, quantile, probs = probs) |>
      as_tibble(rownames = "quant") |>
      mutate(quant = ifelse(quant == perc[1],
                            "lower",
                            ifelse(quant == perc[2],
                                   "upper",
                                   "med"))) |>
      pivot_longer(-quant, names_to = "age", values_to = "prop") |>
      pivot_wider(names_from = "quant", values_from = "prop") |>
      mutate(age = as.numeric(age))
  }

  by_nums <- reformat(natsel_prop)
  by_weight <- reformat(natselwt_prop)

  plist <- NULL
  plist[[1]] <- ggplot(by_nums,
                       aes(x = age, y = med)) +
    geom_bar(stat = "identity", alpha = bar_alpha) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = whisker_width) +
    geom_point() +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim, expand = FALSE) +
    xlab("Age") +
    ylab(paste0("Expected proportion in ",
                model$endyr + 1,
                " catch"))
  plist[[2]] <- ggplot(by_weight,
                       aes(x = age, y = med)) +
    geom_bar(stat = "identity", alpha = bar_alpha) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = whisker_width) +
    geom_point() +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim)

  plot_grid(plotlist = plist, nrow = 1)
}
