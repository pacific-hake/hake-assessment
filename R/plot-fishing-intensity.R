#' Create a plot of fishing intensity
#'
#' @rdname plot_biomass
#' @param fspr40_text_x X-axis value for the location of the FSPR=40% text
#' @param fspr40_text_y Y-axis value for the location of the FSPR=40% text
#'
#' @export
plot_fishing_intensity <- function(model,
                                   show_arrows = TRUE,
                                   xlim = c(1964,
                                            year(Sys.time()) - 1),
                                   x_breaks = 1966:year(Sys.time()),
                                   x_labs_mod = 5,
                                   x_expansion = 1,
                                   tick_prop = 1,
                                   ylim = c(0, 1.4),
                                   y_breaks = seq(ylim[1], ylim[2], by = 0.2),
                                   point_size = ts_single_model_pointsize,
                                   point_color = ts_single_model_pointcolor,
                                   point_shape = ts_single_model_pointshape,
                                   point_stroke = ts_single_model_pointstroke,
                                   line_width = ts_single_model_linewidth,
                                   line_color = ts_single_line_color,
                                   fspr40_text_x = xlim[1] + 2,
                                   fspr40_text_y = 1.05){

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  calcs <- model$mcmccalcs

  # Relative fishing intensity quantiles
  inds <- grep("^[0-9]{4}$", names(calcs$plower))
  plower <- calcs$plower[inds]
  pmed <- calcs$pmed[inds]
  pupper <- calcs$pupper[inds]
  year <- as.numeric(names(plower))

  d <- tibble(year,
              plower,
              pmed,
              pupper)

  # Remove projection years
  d <- d |>
    dplyr::filter(year <= xlim[2] & year >= xlim[1])

  # Calculate the data outside the range of the y limits and
  # change the CI in the data to cut off at the limits
  yoob <- calc_yoob(d, ylim, "plower", "pmed", "pupper", show_arrows)

  g <- ggplot(yoob$d,
              aes(x = year,
                  xend = year,
                  y = plower,
                  yend = pupper)) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    geom_hline(yintercept = 1,
               linewidth = 0.5) +
    geom_segment(linewidth = line_width,
                 lineend = "round",
                 color = line_color) +
    geom_point(aes(y = pmed),
               shape = point_shape,
               color = point_color,
               size = point_size,
               stroke = point_stroke) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    annotate("text",
             x = fspr40_text_x,
             y = fspr40_text_y,
             label = expression(~F['SPR=40%'])) +
    xlab("Year") +
    ylab(expression(paste("Rel. fishing intensity",
                          ~~(1-italic(SPR))/(1-italic(SPR)['40%']))))

  # Add arrows to the plot to point toward the out of bounds data points
  g <- g |>
    draw_arrows_yoob(yoob)

  # Add major tick marks
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    # This proportion must be set by trial and error
                    # Make sure to change `vjust` value above in the `theme()`
                    # call so the labels are not overlapping the lines or
                    # too far away from the lines
                    prop = tick_prop)

  g
}