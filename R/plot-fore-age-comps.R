#' Create a two-panel plot showing the forecasted age compositions by
#' numbers and weight for the first forecast year (`endyr` + 1)
#'
#' @param model  A model object, created by [create_rds_file()]
#' @param probs A vector of three quantiles for the lower CI,
#' median, and upper CI. The second value must be 0.5
#' @param x_lim A vector of 2 representing the minimum and maximum x-axis
#' values to plot
#' @param x_breaks A vector of values to show on the x-axis
#' @param y_lim  A vector of 2 representing the minimum and maximum y-axis
#' values to plot
#' @param bar_alpha The transparency of the bars from 0 to 1
#' @param whisker_width The width of the top and bottom crossbars
#' (whiskers) on the error bars
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return A [cowplot::plot_grid()] object
#' @export
plot_fore_age_comps <- function(model,
                                probs = c(0.025, 0.5, 0.975),
                                x_lim = c(1, 15),
                                x_breaks = seq(x_lim[1], x_lim[2], 2),
                                y_lim = c(0, 0.4),
                                bar_alpha = 0.3,
                                whisker_width = 0.5,
                                axis_title_font_size = 14,
                                axis_tick_font_size = 12,
                                axis_label_color = "black"){

  if(length(probs) != 3){
    stop("The `probs` vector must be length 3, composed of the lower CI ",
         "median, and upper CI",
         call. = FALSE)
  }
  if(probs[2] != 0.5){
    stop("The middle value in `probs` must be 0.5 (median)",
         call. = FALSE)
  }
  natsel_prop <- model$extra_mcmc$natsel_prop
  natselwt_prop <- model$extra_mcmc$natselwt_prop

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
    geom_bar(stat = "identity",
             alpha = bar_alpha) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  width = whisker_width) +
    geom_point() +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim,
                    expand = FALSE) +
    xlab("Age") +
    ylab(paste0("Expected proportion in ",
                model$endyr + 1,
                " catch")) +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(12, 6, 6, 6))


  plist[[2]] <- ggplot(by_weight,
                       aes(x = age, y = med)) +
    geom_bar(stat = "identity",
             alpha = bar_alpha) +
    geom_errorbar(aes(ymin = lower,
                      ymax = upper),
                  width = whisker_width) +
    geom_point() +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim) +
    xlab("Age") +
    ylab("") +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(12, 6, 6, 0))

  plot_grid(plotlist = plist, nrow = 1, align = "h")
}
