#' Creates the plot showing the 4 MCMC diagnostics for  all parameters
#' combined
#'
#' @param model A model object as output by [create_rds_file()].
#' @param diag_fill The fill color for the bars in the plots
#' @param diag_alpha The transparency for the bars in the plots
#' @param diag_outline_color The bar outline color for the bars in the plots
#' @param ro_arrow_lengths A vector of 4 values to use for the arrow lengths
#' in the 4 plots. Note the plot orders in this vector is:
#' top left, top right bottom left bottom right
#' @param ro_text_nudges A vector of 4 for the R0 text left/right nudges.
#' See [plot_mcmc_histogram()]
#' @param y_lim_geweke A y limit vector of two values for the Geweke plot
#' only
#' @param ... Arguments passed to [calc_mcmc_param_stats()] and
#' [plot_mcmc_histogram()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_mcmc_diagnostics_all_params <- function(model,
                                             diag_fill = "royalblue",
                                             diag_alpha = 0.3,
                                             diag_outline_color = "black",
                                             ro_arrow_lengths = rep(20, 4),
                                             ro_text_nudges = rep(0, 4),
                                             y_lim_geweke = c(0, 30),
                                             ...){

  d <- calc_mcmc_param_stats(model, ...)

  p <- list()
  p[[1]] <- plot_mcmc_histogram(d,
                                col_nm = "autocor",
                                color = diag_outline_color,
                                x_lab = "Autocorrelation, lag = 1",
                                y_lab = "",
                                ro_arrow_length = ro_arrow_lengths[1],
                                ro_text_nudge = ro_text_nudges[1],
                                fill = diag_fill,
                                alpha = diag_alpha,
                                y_lim = NULL,
                                x_lim = c(-1, 1),
                                x_breaks = seq(-1, 1, 0.5),
                                ...)

  p[[2]] <- plot_mcmc_histogram(d,
                                col_nm = "effn",
                                x_lab = "Effective sample size (thousands)",
                                y_lab = "",
                                scale_effn = 1e3,
                                color = diag_outline_color,
                                fill = diag_fill,
                                alpha = diag_alpha,
                                ro_arrow_length = ro_arrow_lengths[2],
                                ro_text_nudge = ro_text_nudges[2],
                                x_lim = c(0, max(d$effn)),
                                x_brk = 1,
                                y_brk = 50,
                                ...)

  p[[3]] <- plot_mcmc_histogram(d,
                                col_nm = "geweke",
                                x_lab = "Geweke statistic",
                                y_lab = "",
                                color = diag_outline_color,
                                fill = diag_fill,
                                alpha = diag_alpha,
                                bar_label_limit = 2,
                                ro_arrow_length =  ro_arrow_lengths[3],
                                ro_text_nudge = ro_text_nudges[3],
                                x_lim = c(-3, 3),
                                y_lim = y_lim_geweke,
                                x_breaks = seq(-3, 3, by = 1),
                                x_brk = 0.25,
                                y_brk = 5,
                                ...)

  p[[4]] <- plot_mcmc_histogram(d,
                                col_nm = "heidelwelch",
                                x_lab = "Heidelberger and Welch statistic",
                                y_lab = "",
                                barplot = TRUE,
                                lvls = c("No test", "Failed", "Passed"),
                                color = diag_outline_color,
                                fill = diag_fill,
                                alpha = diag_alpha,
                                ro_arrow_length =  ro_arrow_lengths[4],
                                ro_text_nudge = ro_text_nudges[4],
                                y_brk = 50,
                                y_lim = c(0, 350),
                                ...)

  p <- plot_grid(plotlist = p, nrow = 2)

  # Add the y-axis label outside the grid panels
  y_lab <- "Counts"
  y_grob <- textGrob(y_lab,
                     gp = gpar(fontface = "bold",
                               col = "black",
                               fontsize = 15),
                     rot = 90)
  g <- arrangeGrob(p, left = y_grob)

  plot_grid(g)
}
