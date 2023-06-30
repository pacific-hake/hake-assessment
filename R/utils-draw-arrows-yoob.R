#' Draw arrows on the top and/or bottom of the plot pointing to where
#' out-of-bounds points are located. YOOB means Y-axis Out-Of-Bounds
#'
#' @param g A [ggplot2::ggplot()] object
#' @param yoob Anlist returned from the function [calc_yoob()]
#' @param arrow_angle See `angle` parameter in  [grid::arrow()]
#' @param arrow_size_npc See `length` parameter in  [grid::arrow()]
#' @param arrow_units  See `length` parameter in  [grid::arrow()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
draw_arrows_yoob <- function(g,
                             yoob,
                             arrow_angle = 10,
                             arrow_size_npc = 0.03,
                             arrow_units = "npc"){

  if(!is.null(yoob$hi_outside)){
    g <- g +
      # Add arrows at top of plot where data are outside the plot region
      geom_segment(data = yoob$hi_outside,
                   aes(x = year,
                       y = yoob$ylim[2] - ((yoob$ylim[2] - yoob$ylim[1]) / 15),
                       xend = year,
                       yend = yoob$ylim[2]),
                   color = "black",
                   linewidth = 0.5,
                   linetype = "solid",
                   arrow = arrow(type = "closed",
                                 angle = arrow_angle,
                                 length = unit(arrow_size_npc,
                                               arrow_units)),
                   inherit.aes = FALSE)
  }

  if(!is.null(yoob$lo_outside)){
    g <- g +
      # Add arrows at bottom of plot where data are outside the plot region
      geom_segment(data = yoob$lo_outside,
                   aes(x = year,
                       y = yoob$ylim[1] + ((yoob$ylim[2] - yoob$ylim[1]) / 15),
                       xend = year,
                       yend = yoob$ylim[1]),
                   color = "black",
                   linewidth = 0.5,
                   linetype = "solid",
                   arrow = arrow(type = "closed",
                                 angle = arrow_angle,
                                 length = unit(arrow_size_npc,
                                               arrow_units)),
                   inherit.aes = FALSE)
  }

  g
}