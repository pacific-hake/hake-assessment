#' Create a plot showing the densities of two posteriors for a parameter
#'
#' @param model1 The model output from Stock Synthesis as loaded by
#' [create_rds_file()]
#' @param model2 The model output from Stock Synthesis as loaded by
#' [create_rds_file()]
#' @param param_nm1 A parameter name present in `names(model1$mcmc)`
#' @param param_nm2 A parameter name present in `names(model2$mcmc)`
#' @param prob_vec A vector of probabilities to use in the `quantile()`
#' calculation
#' @param scale A value to divide the parameter values by
#' @param x_lab The x-axis label
#' @param y_lab The y-axis label. Only shown if `show_y_labels` is `TRUE`
#' @param x_lim A vector of two values. The x-axis minimum and maximum values
#' @param expand_vec A vector to pass to the `expand` argument of the
#' [ggplot2::scale_x_continuous()] function. Default is `c(0, 0)`
#' @param fill_color1 The color used to fill under the density curve for model 1
#' @param fill_color2 The color used to fill under the density curve for model 2
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
#' @param label_x_offset1 An x-axis offset to move the median label for model 1
#' to, positive means move right. Only used if `show_med_label` is `TRUE`
#' @param label_y_offset1 A y-axis offset to move the median label for model 1
#' to, positive means move up. Only used if `show_med_label` is `TRUE
#' @param label_x_offset2 An x-axis offset to move the median label for model 2
#' to, positive means move right. Only used if `show_med_label` is `TRUE`
#' @param label_y_offset2 A y-axis offset to move the median label for model 2
#' to, positive means move up. Only used if `show_med_label` is `TRUE
#' @param ... Arguments to pass to [theme_axis_text()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_param_density_double <- function(model1,
                                      model2,
                                      param_nm1,
                                      param_nm2,
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
                                      fill_color1 = ts_single_model_ribbon_fill,
                                      fill_color2 = "orange",
                                      fill_alpha_tail = 0.3,
                                      fill_alpha_main = 0.7,
                                      show_y_labels = TRUE,
                                      show_med_label = TRUE,
                                      med_label_units = "kt",
                                      med_label_digits = 3,
                                      arrow_obj = arrow(length = unit(0.5, "cm"),
                                                        angle = 20,
                                                        type = "closed"),
                                      label_x_offset1 = 500,
                                      label_y_offset1 = 0.0001,
                                      label_x_offset2 = 700,
                                      label_y_offset2 = -0.0001,
                                      ...){

  if(!param_nm1 %in% names(model1$mcmc)){
    stop("The parameters name `", param_nm1, "` is not ",
    "present in the `mcmc` data frame in the model output")
  }
  if(!param_nm2 %in% names(model2$mcmc)){
    stop("The parameters name `", param_nm2, "` is not ",
         "present in the `mcmc` data frame in the model output")
  }
  col_sym1 <- sym(param_nm1)
  col_sym2 <- sym(param_nm2)

  d1 <- model1$mcmc |>
    as_tibble() |>
    mutate(!!col_sym1 := !!col_sym1 / scale) |>
    select(!!col_sym1) |>
    rename(value = !!col_sym1)
  d2 <- model2$mcmc |>
    as_tibble() |>
    mutate(!!col_sym2 := !!col_sym2 / scale) |>
    select(!!col_sym2) |>
    rename(value = !!col_sym2)

  quants1 <- quantile(d1 |> pull(), probs = prob_vec)
  quants2 <- quantile(d2 |> pull(), probs = prob_vec)

  g <- ggplot(d1) +
    geom_density(aes(x = value),
                 fill = fill_color1,
                 alpha = fill_alpha_tail) +
    geom_density(data = d2,
                 aes(x = value),
                 fill = fill_color2,
                 alpha = fill_alpha_tail)

  g1 <- ggplot(d1) +
    geom_density(aes(x = value),
                 fill = fill_color1,
                 alpha = fill_alpha_tail)
  g2 <- ggplot(d2) +
    geom_density(aes(x = value),
                 fill = fill_color1,
                 alpha = fill_alpha_tail)

  gb1 <- ggplot_build(g1)$data[[1]] |>
    as_tibble()
  gb2 <- ggplot_build(g2)$data[[1]] |>
    as_tibble()

  g2 <- ggplot(d2) +
    geom_density(aes(x = value),
                 fill = fill_color2,
                 alpha = fill_alpha_tail)

  if(nrow(d1) %% 2 == 0){
    # Even number of rows
    wch_less_than_median <- which(gb1$x < quants1[2])
    med_ind1 <- tail(wch_less_than_median, 1)
    med_dens1 <- (gb1$density[med_ind1] + gb1$density[med_ind1 + 1]) / 2
  }else{
    # Odd number of rows
    med_ind1 <- which(gb1$x == quants1[2])
    med_dens1 <- gb1$density[med_ind1]
  }
  if(nrow(d2) %% 2 == 0){
    # Even number of rows
    wch_less_than_median <- which(gb2$x < quants2[2])
    med_ind2 <- tail(wch_less_than_median, 1)
    med_dens2 <- (gb2$density[med_ind2] + gb2$density[med_ind2 + 1]) / 2
  }else{
    # Odd number of rows
    med_ind2 <- which(gb2$x == quants2[2])
    med_dens2 <- gb2$density[med_ind2]
  }

  med1 <- tibble(x = quants1[2],
                 y = med_dens1)
  med2 <- tibble(x = quants2[2],
                 y = med_dens2)

  g <- g +
    geom_area(data = gb1 |>
                dplyr::filter(x >= quants1[1] & x <= quants1[3]),
              aes(x = x,
                  y = y),
              fill = fill_color1,
              alpha = fill_alpha_main) +
    geom_area(data = gb2 |>
                dplyr::filter(x >= quants2[1] & x <= quants2[3]),
              aes(x = x,
                  y = y),
              fill = fill_color2,
              alpha = fill_alpha_main) +
    geom_segment(data = med1,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1.5,
                 color = "white") +
    geom_segment(data = med2,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1.5,
                 color = "darkgrey") +
    geom_segment(data = med1,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1) +
    geom_segment(data = med2,
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
      geom_segment(data = med1,
                   aes(x = x + label_x_offset1,
                       y = y + label_y_offset1,
                       xend = x,
                       yend = y),
                   arrow = arrow_obj) +
      geom_label(data = med1,
                 aes(x = x,
                     y = y),
                 label = paste0("Median = ",
                                f(med1$x, med_label_digits),
                                " ", med_label_units),
                 nudge_x = label_x_offset1,
                 nudge_y = label_y_offset1,
                 fill = "white") +
      geom_segment(data = med2,
                   aes(x = x + label_x_offset2,
                       y = y + label_y_offset2,
                       xend = x,
                       yend = y),
                   arrow = arrow_obj) +
      geom_label(data = med2,
                 aes(x = x,
                     y = y),
                 label = paste0("Median = ",
                                f(med2$x, med_label_digits),
                                " ", med_label_units),
                 nudge_x = label_x_offset2,
                 nudge_y = label_y_offset2,
                 fill = "white")
  }

  g
}
