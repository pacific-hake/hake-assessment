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
#' @param major_tick_thickness The thickness of the major tick marks
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A [ggplot2::ggplot()] object, the original plot `g` with x-axis
#' ticks modified to be minor and major ticks
#'
#' @export
add_major_ticks <- function(g,
                            x_breaks = NULL,
                            modulo = 5,
                            prop = 0.8,
                            major_tick_thickness = 0.5,
                            ...){

  gr <- ggplot_build(g)
  scale_limits <- gr$plot$scales$scales[[2]]$limits
  if(!is.null(scale_limits)){
    if(!is.na(scale_limits[1]) && !is.na(scale_limits[2])){
      warning("The plot has `limits` set to non-`NA`values in the ",
              "`scale_y_continuous()` function, which prevents major ",
              "ticks from being drawn as those limits clip the y-axis ",
              "values to the data. Offending function call is:\n\n",
              deparse(sys.calls()[[sys.nframe()-1]]),
              "\n")
    }
  }

  if(gr$layout$coord$clip == "on"){
    g <- g +
      coord_cartesian(clip = "off")
  }
  x_limits <- gr$layout$panel_params[[1]]$x.range
  y_limits <- gr$layout$panel_params[[1]]$y.range

  x_breaks <- x_breaks %||% x_limits[1]:x_limits[2]

  x_breaks_nth <- x_breaks[x_breaks %% modulo == 0]
  tick_length <- (y_limits[2] - y_limits[1]) / (.pt * 10) * prop
  bot_pos <- y_limits[1] - tick_length

  custom_ticks <- tibble(group = x_breaks_nth,
                         end = bot_pos)

  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = y_limits[1],
                       ymin = end),
                   size = major_tick_thickness,
                   inherit.aes = FALSE)

}