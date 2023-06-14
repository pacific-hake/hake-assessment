#' Create a vector of labels to use when major and minor tick marks are
#' to be used in a plot, and the custom tick data frame to be used to draw
#' the lines outside the ggplot panel area using [ggplot2::geom_linerange()]
#'
#' @param g A [ggplot2::ggplot()] object
#' @param x_breaks A vector of values which are where all the x-axis ticks
#' are located on the plot `g`. If `NULL`, this will become a sequence from
#' the lower limit of the x-axis to the upper limit, by 1.
#' @param modulo The number to use as a modulus so that every n-th tick mark
#' is a major tick
#' @param prop A value between 0 and 1 to multiply the length of the major
#' tick lines by. This is used in case the full length ticks are overlapping
#' the tick labels
#'
#' @return A [ggplot2::ggplot()] object, the original plot `g` with x-axis
#' ticks modified to be minor and major ticks
#'
#' @export
add_major_ticks <- function(g,
                            x_breaks = NULL,
                            modulo = 5,
                            prop = 0.8){

  gr <- ggplot_build(g)

  x_limits <- gr$layout$panel_params[[1]]$x.range
  y_limits <- gr$layout$panel_params[[1]]$y.range

  x_breaks <- x_breaks %||% x_limits[1]:x_limits[2]

  x_breaks_nth <- x_breaks[x_breaks %% modulo == 0]
  tick_length <- (y_limits[2] - y_limits[1]) / (.pt * 10) * 0.8
  bot_pos <- y_limits[1] - tick_length

  custom_ticks <- tibble(group = x_breaks_nth,
                         end = bot_pos)

  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = y_limits[1],
                       ymin = end),
                   size = 0.5,
                   inherit.aes = FALSE)

}