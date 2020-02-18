#' ggplot2 theme for hake
#'
#' @export
#' @importFrom ggplot2 theme theme_bw element_rect element_blank element_line margin unit alpha
hake_theme <- function(){
  theme_bw() +
    theme(legend.box.background = element_rect(fill = alpha("white", 0.7)),
          legend.box.margin = margin(1, 1, 1, 1, "mm"),
          legend.key = element_blank(),
          legend.margin = margin(),
          legend.text.align = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.grid.major = element_line(colour = "darkgrey", size = 0.2),
          #panel.grid.minor = element_line(colour = "darkgrey", size = 0.1),
          legend.background = element_rect(fill = "transparent"),
          #panel.spacing.x=unit(3, "lines"),
          plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"))
}

## ggplot globals for project
##ggplot2::theme_set(gfplot::theme_pbs())
scale_colour_continuous <- scale_colour_viridis_c
scale_fill_continuous <- scale_fill_viridis_c

sensitivity_colors <- c("#000000", RColorBrewer::brewer.pal(8L, "Dark2"))
scale_colour_discrete <- function(...) scale_colour_manual(..., values = sensitivity_colors)
scale_fill_discrete <- function(...) scale_fill_manual(... , values = sensitivity_colors)
