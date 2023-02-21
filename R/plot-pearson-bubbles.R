#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column
#' names `Year`, `Age`, and `Proportion`
#' @param type 1 = Fishery, any other value = Survey
#' @param clines An optional vector of years to draw cohort lines through
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param legend.position See the `legend.position` parameter of
#' [ggplot2::theme()]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_pearson_bubbles <- function(model,
                                 type = 1,
                                 clines = c(1980, 1984, 1999, 2010, 2014, 2016),
                                 by = 5,
                                 legend.position = "none",
                                 alpha = 0.3,
                                 xlim = c(1975, year(Sys.Date())),
                                 ...){

  if(type == 1){
    d <- model$extra.mcmc$comp_fishery_median
  }else{
    d <- model$extra.mcmc$comp_survey_median
  }

  g <- ggplot(d, aes(x = Yr,
                     y = Age,
                     size = abs(Pearson),
                     fill = factor(sign(as.numeric(Pearson))))) +
    geom_point(pch = 21, alpha = alpha, ...) +
    scale_x_continuous(breaks = seq(from = xlim[1], to = xlim[2], by = by),
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_manual(values = c("white", "black"), guide = FALSE) +
    scale_size_continuous(breaks = c(1, 1, 2, 2, 3, 3),
                          labels = c(-8, -4, -0.1, 0.1, 4, 8),
                          range = c(0.1, 8))

  if(!is.null(clines)){
    clines <- tibble(year = clines,
                     y = 0,
                     xend = clines + max(as.numeric(d$Age)),
                     yend = max(as.numeric(d$Age)))
    g <- g +
      geom_segment(data = clines,
                   x = clines$year,
                   y = clines$y,
                   aes(xend = xend,
                       yend = yend),
                   linewidth = 1,
                   color = "red",
                   inherit.aes = FALSE,
                   ...)
  }
  g <- g +
    theme(legend.position = legend.position, ...) +
    guides(size = guide_legend(title = "Residuals",
                               nrow = ifelse(legend.position == "right" |
                                               legend.position == "left", 10, 1),
                               override.aes =
                                 list(fill = c("white", "white", "white",
                                                      "black", "black", "black"),
                                                      size = c(8, 4, 0.1,
                                                               0.1, 4, 8))))

  g
}
