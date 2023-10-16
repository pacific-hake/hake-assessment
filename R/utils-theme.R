#' Theme for the [ggplot2::ggplot()] plots in the document
#'
#' @param ax_label_color The color of the text for all tick labels and
#' axis titles in the project
#' @param ax_title_font_size The font size for all the axis titles in the
#' project
#' @param ax_tick_font_size The font size for all the axis tick labels in the
#' project
#' @param ax_title_font_face The font face for all the axis titles in the
#' project. One of "plain", "bold", or "italics"
#' @param ax_tick_font_face The font face for all the axis tick labels in the
#' project. One of "plain", "bold", or "italics"
#' @param ax_tick_length_cm  The length of all the tick mark lines in the
#' project
#' @param margin_def A call to the function [ggplot2::margin()]. The format
#' must be like this: margin(12, 12, 12, 12) where the order of the arguments
#' is for each side top, right, bottom, left. Defaults to zero margin on all
#' sides
#'
#' @export
hake_theme <- function(ax_label_color = axis_label_color,
                       ax_title_font_size = axis_title_font_size,
                       ax_tick_font_size = axis_tick_font_size,
                       ax_title_font_face = "plain",
                       ax_tick_font_face  = "plain",
                       ax_tick_length_cm = 0.15,
                       margin_def = margin(0, 0, 0, 0)){

  theme_bw() +
    theme(legend.key = element_blank(),
          legend.text.align = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          axis.text.x = element_text(color = ax_label_color,
                                     size = ax_tick_font_size,
                                     hjust = 0.5,
                                     vjust = -1,
                                     face = ax_tick_font_face),
          axis.title.x = element_text(color = ax_label_color,
                                      size = ax_title_font_size,
                                      hjust = 0.5,
                                      vjust = -1,
                                      face = ax_title_font_face),
          axis.text.y = element_text(color = ax_label_color,
                                     size = ax_tick_font_size,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = ax_tick_font_face),
          axis.title.y = element_text(color = ax_label_color,
                                      size = ax_title_font_size,
                                      hjust = 0.5,
                                      vjust = 2,
                                      angle = 90,
                                      face = ax_title_font_face),
          axis.ticks.length = unit(ax_tick_length_cm, "cm"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin_def)

}