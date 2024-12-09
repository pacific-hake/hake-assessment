#' Theme For \code{\link[ggplot2]{theme}}
#'
#' Create a special theme for hake plots when plotting using the
#' \code{\link[ggplot2]{ggplot}} functions.
#'
#' @author Kelli F. Johnson
#' @return A \code{\link[ggplot2]{theme}} object with specific
#' instructions on how to create plots for this repository.
#'
plot_theme <- function() {
  theme <- ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA, size = 0.1
    ),
    legend.key = element_blank()
  )
  return(theme)
}
