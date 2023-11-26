#' A cleaned-up copy of hexbins::hexcoords()
#'
#' @param dx horizontal width of the hexagon(s)
#' @param dx vertical width of the hexagon(s)
#' @param n number of hexagon “repeats”
#' @param sep separator value to be put between coordinates of different
#' hexagons. The default, `NULL` doesn't use a separator
#'
#' @return A vector of six coordinates
#' @export
hex_coords <- function (dx,
                        dy = NULL,
                        n = 1,
                        sep = NULL){

  stopifnot(length(dx) == 1)

  dy <- dy %||% dx / sqrt(3)

  if(is.null(sep)){
    list(x = rep.int(c(dx, dx, 0, -dx, -dx, 0), n),
         y = rep.int(c(dy, -dy, -2 * dy, -dy, dy, 2 * dy), n),
         no.sep = TRUE)
  }else{
    list(x = rep.int(c(dx, dx, 0, -dx, -dx, 0, sep), n),
         y = rep.int(c(dy, -dy, -2 * dy, -dy, dy, 2 * dy, sep), n),
         no.sep = FALSE)
  }
}

