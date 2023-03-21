#' Create a plot of fishing intensity
#'
#' @rdname plot_biomass
#' @export
plot_fishing_intensity <- function(model,
                                   xlim = c(1964,
                                            year(Sys.time()) - 1),
                                   x_breaks = 1966:year(Sys.time()),
                                   x_labs_mod = 5,
                                   x_expansion = 1,
                                   ylim = c(0, 1.4),
                                   y_breaks = seq(ylim[1], ylim[2], by = 0.2),
                                   axis_title_font_size = 12,
                                   axis_tick_font_size = 11,
                                   point_size = 1,
                                   point_shape = 16,
                                   line_width = 0.5,
                                   line_color = "blue"){


  # Remove labels for the minor x-axis ticks
  x_labels <- NULL
  for(i in x_breaks){
    if(i %% x_labs_mod == 0){
      x_labels <- c(x_labels, i)
    }else{
      x_labels <- c(x_labels, "")
    }
  }
  # Major tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = ylim[1]
  bot_y_pos = ylim[1] - (ylim[2] - ylim[1]) / 40
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  calcs <- model$mcmccalcs

  # Relative fishing intensity quantiles
  inds <- grep("^[0-9]{4}$", names(calcs$plower))
  plower <- calcs$plower[inds]
  pmed <- calcs$pmed[inds]
  pupper <- calcs$pupper[inds]
  yrs <- as.numeric(names(plower))

  df <- tibble(yrs,
               plower,
               pmed,
               pupper)

  # Remove projection years
  df <- df |>
    filter(yrs <= xlim[2])

  g <- ggplot(df,
              aes(x = yrs, xend = yrs, y = plower, yend = pupper)) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    geom_hline(yintercept = 1,
               linewidth = 0.5) +
    geom_segment(linewidth = line_width,
                 lineend = "round",
                 color = line_color) +
    geom_point(aes(y = pmed),
               size = point_size) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    annotate("text",
             x = xlim[1] + 2,
             y = 1.05,
             label = expression(~F['SPR=40%']))

  g <- g +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -1,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm")) +
    theme(# plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 6, 0)) +
    xlab("Year") +
    ylab(expression(paste("Rel. fishing intensity", ~~(1-italic(SPR))/(1-italic(SPR)['40%']))))

  # Add major tick marks
  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = top_y_pos,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)

  g
}