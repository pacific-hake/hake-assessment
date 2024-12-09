#' Colours for the Hake Assessment
#'
#' Generate the colour scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same colour.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#'
#' @param n The default number of colours to generate is 10.
#'
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @author Kelli F. Johnson
#'
#' @return A vector of colours
#'
#' @examples
#' n <- 15
#' plot_colour(n)
#'
plot_colour <- function(n = 10) {
  base <- RColorBrewer::brewer.pal(name = "Set1", n = 9)
  colors <- c(base[(n - 1):1], "#000000")
  if (n > 10 & n < 18) {
    extra <- RColorBrewer::brewer.pal(name = "Set2", n = 7)
    colors <- c(extra[(n - 10):1], rev(base), "#000000")
  }
  if (n >= 18) stop(n, " is too many colors, only 17 are allowed.")
  return(colors)
}
