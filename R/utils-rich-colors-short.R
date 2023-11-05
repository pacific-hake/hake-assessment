#' Create rich colors as RGB strings
#'
#' @param n The number of colors
#' @param alpha The transparency for all colors
#'
#' @return A vector of RGB strings representing rich colors
#' @export
rich_colors_short <- function(n, alpha = 1){

  x <- seq(0, 1, length = n)
  r <- 1 / (1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x ^ 2), 1)
  dn <- dnorm(x, 0.25, 0.15)
  b <- dn / max(dn)
  matrix(c(r, g, b), ncol = 3) |>
    as_tibble() |>
    pmap_chr(~{rgb(..1, ..2, ..3, alpha = alpha)})
}
