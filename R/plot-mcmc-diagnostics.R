#' Plot MCMC diagnostics
#'
#' @details Panels are as follows:
#' * Top left: traces of posteriors across iterations,
#' * Top right: cumulative running median with 2.5th and 97.5th percentiles
#' * Bottom left: autocorrelation present in the chain at different lag times
#'   (i.e., distance between samples in the chain), and
#' * Bottom right: distribution of the values in the chain (i.e., the marginal
#'   density from a smoothed histogram of values in the trace plot).
#'
#' @param model A model object as output by [create_rds_file()].
#' @param post_regex  A regular expression representing a parameter as it
#'   appears in the [r4ss::SS_output()] column.
#' @param post_name  A name to show for the posterior on the plot, where
#'   the name can be a string or an expression.
#' @param probs A vector of 3 values for the lower, median, and upper
#' quantiles
#' @param all_alpha Alpha value for density and running mean plots
#' @param ... Arguments passed to [plot_autocor()]
#'
#' @return A 4-panel plot of MCMC diagnostics
#' @export
plot_mcmc_diagnostics <- function(model,
                                  post_regex,
                                  post_name,
                                  probs = c(0.025, 0.5, 0.975),
                                  color = "blue",
                                  fill = "blue",
                                  rib_alpha = 0.2,
                                  line_bar_alpha = 1,
                                  ...){

  plist <- NULL
  plist[[1]] <- plot_trace(model,
                           post_regex,
                           color = color,
                           alpha = line_bar_alpha,
                           ...)

  plist[[2]] <- plot_running_quants(model,
                                    post_regex,
                                    fill = fill,
                                    alpha = rib_alpha,
                                    ...)

  plist[[3]] <- plot_autocor(model,
                             post_regex,
                             fill = fill,,
                             alpha = line_bar_alpha,
                             ...)

  plist[[4]] <- plot_density(model,
                             post_regex,
                             fill = fill,
                             alpha = rib_alpha,
                             ...)

  p <- plot_grid(plotlist = plist,
                 ncol = 2,
                 nrow = 2,
                 byrow = TRUE) #+
    #theme(plot.background = element_rect(color = "black"))
  post_name <- gsub(" +", "~", post_name)

  y_grob <- textGrob(parse(text = post_name),
                     gp = gpar(fontface = "bold",
                               col = "black",
                               fontsize = 15),
                     rot = 90)
  g <- arrangeGrob(p, left = y_grob)

  g
}
