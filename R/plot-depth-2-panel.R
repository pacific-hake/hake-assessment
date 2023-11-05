#' Create a 2-panel plot with fishing depth on the left and bottom depth on
#' the right
#'
#' @param fishing_depth_df A data frame containing boxplot input, see files like
#' `depth-us-*.csv`
#' @param bottom_depth_df A data frame containing boxplot input, see files like
#' `depth-us-*.csv`
#' @param label_size The font size for the two axis labels,
#' `Year` and `Depth (m)`
#' @param ... Arguments passed to [plot_depth()]
#'
#' @return A [gridExtra::grid.arrange()] object
#' @export
plot_depth_2_panel <- function(fishing_depth_df,
                               bottom_depth_df,
                               label_size = 14,
                               ...){

  p_lst <- list()
  p_lst[[1]] <- plot_depth(fishing_depth_df,
                           title_text = "Fishing depth",
                           ...)
  p_lst[[2]] <- plot_depth(bottom_depth_df,
                           title_text = "Bottom depth",
                           ...)
  plt <- plot_grid(plotlist = p_lst,
                   ncol = 2)

  y_grob <- textGrob("Depth (m)",
                     gp = gpar(fontsize = label_size),
                     rot = 90)
  x_grob <- textGrob("Year",
                     gp = gpar(fontsize = label_size),
                     vjust = -0.5)
  grid.arrange(arrangeGrob(plt, left = y_grob, bottom = x_grob))
}
