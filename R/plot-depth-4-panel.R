#' Create a 4-panel plot with fishing depth on the left and bottom depth on
#' the right, with one fleet on top and another below
#'
#' @param fishing_depth_df1 A data frame containing boxplot input, see files
#' like `depth-us-*.csv`
#' @param bottom_depth_df1 A data frame containing boxplot input, see files
#' like `depth-us-*.csv`
#' @param fishing_depth_df2 A data frame containing boxplot input, see files
#' like `depth-us-*.csv`
#' @param bottom_depth_df2 A data frame containing boxplot input, see files
#' like `depth-us-*.csv`
#' @param label_size The font size for the two axis labels,
#' `Year` and `Depth (m)`
#' @param ... Arguments passed to [plot_depth()]
#'
#' @return A [gridExtra::grid.arrange()] object
#' @export
plot_depth_4_panel <- function(fishing_depth_df1,
                               bottom_depth_df1,
                               fishing_depth_df2,
                               bottom_depth_df2,
                               fleet_1_name = "",
                               fleet_2_name = "",
                               label_size = 14,
                               ...){

  p_lst <- list()
  p_lst[[1]] <- plot_depth(fishing_depth_df1,
                           title_text = paste0("Fishing depth - ",
                                               fleet_1_name),
                           ...)
  p_lst[[2]] <- plot_depth(bottom_depth_df1,
                           title_text = paste0("Bottom depth - ",
                                               fleet_1_name),
                           ...)
  p_lst[[3]] <- plot_depth(fishing_depth_df2,
                           title_text = paste0("Fishing depth - ",
                                               fleet_2_name),
                           ...)
  p_lst[[4]] <- plot_depth(bottom_depth_df2,
                           title_text = paste0("Bottom depth - ",
                                               fleet_2_name),
                           ...)
  plt <- plot_grid(plotlist = p_lst,
                   ncol = 2,
                   nrow = 2)

  y_grob <- textGrob("Depth (m)",
                     gp = gpar(fontsize = label_size),
                     rot = 90)
  x_grob <- textGrob("Year",
                     gp = gpar(fontsize = label_size),
                     vjust = -0.5)
  grid.arrange(arrangeGrob(plt, left = y_grob, bottom = x_grob))
}
