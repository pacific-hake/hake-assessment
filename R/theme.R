#' ggplot2 theme for hake
#'
#' @export
hake_theme <- function(){
  theme_bw() +
    theme(legend.key = element_blank(),
          legend.text.align = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"))
}
