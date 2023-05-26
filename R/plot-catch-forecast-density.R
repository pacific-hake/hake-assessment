#' Title
#'
#' @param model The model output from Stock Synthesis as loaded by
#' [create_rds_file()].
#' @param yr Forecast year to plot. Must be a column in the MCMC output
#' @param xlim A vector of two values. The x-axis minimum and maximum values
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_forecast_density <- function(model,
                                        yr,
                                        xlim = c(0, 4),
                                        axis_title_font_size = 14,
                                        axis_tick_font_size = 11,
                                        axis_label_color = "black"){

  col <- paste0("ForeCatch_", yr)
  col_sym <- sym(col)

  d <- model$mcmc |>
    as_tibble() |>
    mutate(!!col_sym := !!col_sym / 1e6) |>
    select(!!col_sym) |>
    rename(value = !!col_sym)

  quants <- quantile(d |> pull(), probs = probs)

  g <- ggplot(d) +
    geom_density(aes(x = value),
                 fill = "grey40",
                 alpha = 0.5)
  gb <-  ggplot_build(g)$data[[1]] |>
    as_tibble()

  if(nrow(d) %% 2 == 0){
    # Even number of rows
    wch_less_than_median <- which(gb$x < quants[2])
    med_ind <- tail(wch_less_than_median, 1)
    med_dens <- (gb$density[med_ind] + gb$density[med_ind + 1]) / 2
  }else{
    med_ind <- which(gb$x == quants[2])
    med_dens <- gb$density[med_ind]
  }

  med <- tibble(x = quants[2],
                y = med_dens)

  g <- g + geom_area(data = gb |>
                       filter(x >= quants[1] & x <= quants[3]),
                     aes(x = x, y = y),
                     fill = "blue",
                     alpha = 0.5) +
    geom_segment(data = med,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1) +
    coord_cartesian(xlim = xlim) +
    xlab(paste0("Projected ", yr, " catch (Mt) based on the default ",
                "harvest policy")) +
    ylab("Density") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(0, 0, 6, 6))

  g
}
