#' Add a theme element to a [ggplot2::ggplot()] object to standardize the
#' tick and titla labels on a plot
#'
#' @param g A [ggplot2::ggplot()] object
#' @param face Font face. one of "plain", "bold", or "italics"
#' @param angle_title_x The angle to set the x-axis title to
#' @param angle_title_y  The angle to set the y-axis title to
#' @param vjust_tick_x The amount of vertical adjustment to the x-axis tick
#' labels
#' @param vjust_title_x The amount of vertical adjustment to the x-axis title
#' @param vjust_tick_y The amount of vertical adjustment to the x-axis tick
#' labels
#' @param vjust_title_y The amount of vertical adjustment to the y-axis title
#' @param hjust_tick_x The amount of horizontal adjustment to the x-axis tick
#' labels
#' @param hjust_title_x The amount of horizontal adjustment to the x-axis title
#' @param hjust_tick_y The amount of horizontal adjustment to the x-axis tick
#' labels
#' @param hjust_title_y The amount of horizontal adjustment to the y-axis title
#' @param ax_title_font_size Size of the font for the x and y-axis labels
#' @param ax_tick_font_size Size of the font for the x and y-axis tick labels
#' @param ax_label_color Color of the font for the x and y-axis tick and
#' title labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
theme_axis_text <- function(g,
                            face = "plain",
                            angle_title_x = 0,
                            angle_title_y = 90,
                            vjust_tick_x = -1,
                            vjust_title_x = -1,
                            vjust_tick_y = 0.5,
                            vjust_title_y = 0,
                            hjust_tick_x = 0.5,
                            hjust_title_x = 0.5,
                            hjust_tick_y = 0.5,
                            hjust_title_y = 0.5,
                            ax_title_font_size = axis_title_font_size,
                            ax_tick_font_size = axis_tick_font_size,
                            ax_label_color = axis_label_color){

  g +
    theme(axis.text.x = element_text(color = ax_label_color,
                                     size = ax_tick_font_size,
                                     hjust = hjust_tick_x,
                                     vjust = vjust_tick_x,
                                     face = face),
          axis.title.x = element_text(color = ax_label_color,
                                      size = ax_title_font_size,
                                      hjust = hjust_title_x,
                                      vjust = vjust_title_x,
                                      angle = angle_title_x,
                                      face = face),
          axis.text.y = element_text(color = ax_label_color,
                                     size = ax_tick_font_size,
                                     hjust = hjust_tick_y,
                                     vjust = vjust_tick_y,
                                     face = face),
          axis.title.y = element_text(color = ax_label_color,
                                      size = ax_title_font_size,
                                      hjust = hjust_title_y,
                                      vjust = vjust_title_y,
                                      angle = angle_title_y,
                                      face = face),
          axis.ticks.length = unit(0.15, "cm"))
}