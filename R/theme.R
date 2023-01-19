#' ggplot2 theme for hake
#'
#' @export
#' @importFrom ggplot2 theme theme_bw element_rect element_blank element_line margin unit alpha
hake_theme <- function(){
  theme_bw() +
    theme(legend.key = element_blank(),
          legend.text.align = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"))
}

# ggplot globals for project
scale_colour_continuous <- scale_colour_viridis_c
scale_fill_continuous <- scale_fill_viridis_c

sensitivity_colors <- c("#000000", RColorBrewer::brewer.pal(8L, "Dark2"))
scale_colour_discrete <- function(...) scale_colour_manual(..., values = sensitivity_colors)
scale_fill_discrete <- function(...) scale_fill_manual(... , values = sensitivity_colors)
