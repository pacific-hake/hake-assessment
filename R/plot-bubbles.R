#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column
#' names `Year`, `Age`, and `Proportion`
#' @param clines An optional vector of years to draw cohort lines through
#' @param mean_age A two-column tibble with column names `Year` and `Age` where
#' each row contains a year and `Age` represents the mean age for each year
#' @param mean_age_line_color The line color for the mean age line
#' @param mean_age_line_size The line thickness for the mean age line
#' @param mean_age_line_type The line type for the mean age line
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param leg_pos See the `legend.position` parameter in
#' [ggplot2::theme()]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param leg_title The legend title text
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bubbles <- function(d,
                         clines = c(1980, 1984, 1999, 2010, 2014, 2016),
                         mean_age = NULL,
                         mean_age_line_color = "red",
                         mean_age_line_size = 1.5,
                         mean_age_line_type = "solid",
                         yrs = NULL,
                         by = 5,
                         leg_pos = "none",
                         alpha = 0.3,
                         xlim = c(1975, year(Sys.Date())),
                         leg_title = "Proportion",
                         ...){

  if(!is.null(xlim[1])){
    d <- d |>
      filter(Year %in% xlim[1]:xlim[2])
  }

  g <- ggplot(d, aes(x = Year, y = Age, size = Proportion)) +
    geom_point(alpha = alpha, ...) +
    scale_x_continuous(breaks = seq(from = xlim[1], to = xlim[2], by = by),
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_size_continuous(range = c(0.5, 10))
  if(!is.null(clines)){
    clines <- tibble(year = clines,
                     y = 0,
                     xend = clines + max(as.numeric(d$Age)),
                     yend = max(as.numeric(d$Age)))
    g <- g +
      geom_segment(data = clines,
                   x = clines$year,
                   y = clines$y,
                   aes(xend = clines$xend,
                       yend = clines$yend),
                   size = 1,
                   color = "red",
                   ...)
  }

  if(!is.null(mean_age)){
    g <- g +
      geom_line(data = mean_age,
                aes(x = Year, y = Age),
                inherit.aes = FALSE,
                color = mean_age_line_color,
                size = mean_age_line_size,
                linetype = mean_age_line_type)
  }

  g <- g +
    theme(legend.position = leg_pos, ...) +
    guides(size = guide_legend(title = leg_title))

  g
}
