#' Make a bubble plot from the given data
#'
#' @param model A model, created by [create_rds_file()]
#' @param type 1 = Fishery, any other value = Survey
#' @param clines An optional vector of years to draw cohort lines through
#' @param by How many years between year labels on the x-axis
#' @param leg_pos See the `leg_pos` parameter of
#' [ggplot2::theme()]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_pearson_bubbles <- function(model,
                                 type = c("fishery", "survey"),
                                 clines = c(1980, 1984, 1999, 2010, 2014, 2016),
                                 by = 5,
                                 leg_pos = "none",
                                 alpha = 0.3,
                                 xlim = c(1975, year(Sys.Date())),
                                 ...){

  type <- match.arg(type)

  if(type == "fishery"){
    d <- model$extra_mcmc$residuals_fishery
  }else{
    d <- model$extra_mcmc$residuals_survey
  }

  g <- ggplot(d, aes(x =yr,
                     y = age,
                     size = abs(pearson_med),
                     fill = factor(sign(as.numeric(pearson_med))))) +
    geom_point(pch = 21,
               alpha = alpha,
               ...) +
    scale_x_continuous(breaks = seq(from = xlim[1],
                                    to = xlim[2],
                                    by = by),
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_manual(values = c("white",
                                 "black"),
                      guide = "none") +
    scale_size_continuous(breaks = c(1, 1, 2, 2, 3, 3),
                          labels = c(-8, -4, -0.1, 0.1, 4, 8),
                          range = c(0.1, 8)) +
    ylab("Age") +
    xlab("Year")

  if(!is.null(clines)){
    clines <- tibble(year = clines,
                     y = 0,
                     xend = clines + max(as.numeric(d$age)),
                     yend = max(as.numeric(d$age)))
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
    theme(legend.position = leg_pos,
          ...) +
    guides(size =
             guide_legend(title = "Residuals",
                          nrow = ifelse(leg_pos == "right" |
                                          leg_pos == "left", 10, 1),
                          override.aes =
                            list(fill = c("white", "white", "white",
                                          "black", "black", "black"),
                                 size = c(8, 4, 0.1,
                                          0.1, 4, 8))))

  g
}
