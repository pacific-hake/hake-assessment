#' Plot the density histogram for the posteriors for catchability for
#' either the acoustic age-2+ survey or the age-1 index
#'
#' @param model The base model
#' @param model2 The second model, typically last year's base model
#' @param type One of "age2" or "age1
#' @param num_bins The number of bins to use for the histogram created using
#' [ggplot2::geom_histogram()]
#' @param x_breaks A vector of values to show on the x-axis
#' @param y_breaks A vector of values to show on the y-axis
#' @param bar_outline_color The color of the outline of each bar in the histogram
#' @param bar_fill The color of the fill for each bar in the histogram
#' @param bar_alpha The amount of transparency for each bar in the histogram
#' @param line_colors A vector of two line colors, one for the base model
#' (`model`) median catchability and the second for last year's model
#' (`model2`)median catchability
#' @param line_types A vector of two line types for the two median lines
#' @param line_widths A vector of two line widths for the two median lines
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catchability_density <- function(model,
                                      model2 = NULL,
                                      type = c("age2", "age1"),
                                      num_bins = 30,
                                      x_breaks = seq(0, 1.4, 0.1),
                                      y_breaks = seq(0, 3, 0.5),
                                      bar_outline_color = "black",
                                      bar_fill = main_fill,
                                      bar_alpha = main_alpha,
                                      line_colors = c("green", "red"),
                                      line_types = c("solid", "solid"),
                                      line_widths = c(1, 1)){

  type <- match.arg(type)

  if(type == "age2"){
    qvec <- model$extra_mcmc$q_vector
    set.seed(42)
    # For testing:
    # qvec <- rnorm(1000, 0.8, 0.2)
    qmed <- median(qvec)
    q <- qvec |>
      as_tibble() |>
      setNames("value")
    if(!is.null(model2)){
      qvec2 <- model2$extra_mcmc$q_vector
      set.seed(69)
      # For testing:
      # qvec2 <- rnorm(1000, 0.8, 0.2)
      qmed2 <- median(qvec2)
    }
  }else if(type == "age1"){
    qvec <- model$extra_mcmc$q_vector_age1
    qmed <- median(qvec)
    q <- qvec |>
      as_tibble() |>
      setNames("value")
    if(!is.null(model2)){
      qvec2 <- model2$extra_mcmc$q_vector_age1
      qmed2 <- median(qvec2)
    }
  }

  g <- ggplot(q) +
    geom_histogram(data = q,
                   mapping = aes(value, after_stat(density)),
                   fill = bar_fill,
                   alpha = bar_alpha,
                   col = bar_outline_color,
                   bins = num_bins) +
    geom_vline(data = q,
               aes(xintercept = !!qmed),
               linetype = line_types[1],
               linewidth = line_widths[1] * 1.5,
               color = "white") +
    geom_vline(data = q,
               aes(xintercept = qmed),
               linetype = line_types[1],
               linewidth = line_widths[1],
               color = line_colors[1]) +
    scale_x_continuous(breaks = x_breaks,
                       limits = c(min(x_breaks), max(x_breaks)),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks,
                       limits = c(min(y_breaks), NA),
                       expand = c(0, 0)) +
    xlab("") +
    ylab("")

  if(!is.null(model2)){
    g <- g +
      geom_vline(data = q,
                 aes(xintercept = qmed2),
                 linetype = line_types[1],
                 linewidth = line_widths[1] * 1.5,
                 color = "white") +
      geom_vline(data = q,
                 aes(xintercept = qmed2),
                 linetype = line_types[2],
                 linewidth = line_widths[2],
                 color = line_colors[2])
  }

 g
}