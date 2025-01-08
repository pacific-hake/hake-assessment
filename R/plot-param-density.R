#' Create a plot showing the density of the posterior for the catch forecast
#' for a given year
#'
#' @param model The model output from Stock Synthesis as loaded by
#' [create_rds_file()]
#' @param param_nm A parameter name present in `names(model$mcmc)`
#' @param prob_vec A vector of probabilities to use in the `quantile()`
#' calculation
#' @param scale A value to divide the parameter values by
#' @param x_lab The x-axis label
#' @param y_lab The y-axis label. Only shown if `show_y_labels` is `TRUE`
#' @param x_lim A vector of two values. The x-axis minimum and maximum values
#' @param expand_vec A vector to pass to the `expand` argument of the
#' [ggplot2::scale_x_continuous()] function. Default is `c(0, 0)`
#' @param fill_color The color used to fill under the density curve
#' @param fill_alpha_tail The transparency used for the color of the tails
#' @param fill_alpha_main The transparency used for the color of the main part
#' of the curve (-1SD - 1SD)
#' @param show_y_labels Logical. If `TRUE`, show the y-axis label , ticks,
#' and tick labels
#' @param show_med_label Logical. If `TRUE`, show the median label with arrow
#' @param med_label_units Units (as text) to place in the label showing the
#' median. Only used if `show_med_label` is `TRUE`
#' @param show_med_digits Number of decimal places to show in the median label.
#'  Only used if `show_med_label` is `TRUE`
#' @param label_x_offset An x-axis offset to move the median label to,
#' Positive means move right. Only used if `show_med_label` is `TRUE`
#' @param label_y_offset A y-axis offset to move the median label to,
#' Positive means move up. Only used if `show_med_label` is `TRUE
#' @param ... Arguments to pass to [theme_axis_text()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_param_density <- function(model,
                               param_nm,
                               prob_vec = probs,
                               scale = 1e3,
                               x_lab = "",
                               y_lab = "Density",
                               x_lim = c(0, 3000),
                               y_lim = c(0, 0.0001),
                               expand_vec = c(0, 0),
                               x_breaks = seq(x_lim[1],
                                              x_lim[2],
                                              by = (x_lim[2] - x_lim[1]) / 6),
                               fill_color = ts_single_model_ribbon_fill,
                               fill_alpha_tail = 0.3,
                               fill_alpha_main = 0.7,
                               show_y_labels = TRUE,
                               show_med_label = TRUE,
                               med_label_units = "kt",
                               med_label_digits = 3,
                               arrow_obj = arrow(length = unit(0.5, "cm"),
                                                 angle = 20,
                                                 type = "closed"),
                               label_x_offset = 500,
                               label_y_offset = 0.0001,
                               ...){

  if(!param_nm %in% names(model$mcmc)){
    stop("The parameter name `", param_nm, "` is not present in the `mcmc` ",
         "data frame in the model output")
  }
  col_sym <- sym(param_nm)

  d <- model$mcmc |>
    as_tibble() |>
    mutate(!!col_sym := !!col_sym / scale) |>
    select(!!col_sym) |>
    rename(value = !!col_sym)

  quants <- quantile(d |> pull(), probs = prob_vec)

  g <- ggplot(d) +
    geom_density(aes(x = value),
                 fill = fill_color,
                 alpha = fill_alpha_tail)
  gb <- ggplot_build(g)$data[[1]] |>
    as_tibble()

  if(nrow(d) %% 2 == 0){
    # Even number of rows
    wch_less_than_median <- which(gb$x < quants[2])
    med_ind <- tail(wch_less_than_median, 1)
    med_dens <- (gb$density[med_ind] + gb$density[med_ind + 1]) / 2
  }else{
    # Odd number of rows
    med_ind <- which(gb$x == quants[2])
    med_dens <- gb$density[med_ind]
  }

  med <- tibble(x = quants[2],
                y = med_dens)

  g <- g +
    geom_area(data = gb |>
                dplyr::filter(x >= quants[1] & x <= quants[3]),
              aes(x = x,
                  y = y),
              fill = fill_color,
              alpha = fill_alpha_main) +
    geom_segment(data = med,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1.5,
                 color = "white") +
    geom_segment(data = med,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1) +
    scale_x_continuous(breaks = x_breaks,
                       labels = comma(x_breaks),
                       expand = expand_vec) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim,
                    expand = FALSE) +
    xlab(x_lab) +
    ylab(y_lab)
  if(!show_y_labels){
    g <- g +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }

  if(show_med_label){
    g <- g +
      geom_segment(data = med,
                   aes(x = x + label_x_offset,
                       y = y + label_y_offset,
                       xend = x,
                       yend = y),
                   arrow = arrow_obj) +
      geom_label(data = med,
                 aes(x = x,
                     y = y),
                 label = paste0("Median = ",
                                f(med$x, med_label_digits),
                                " ", med_label_units),
                 nudge_x = label_x_offset,
                 nudge_y = label_y_offset,
                 fill = "white")
  }

  g
}
