my_hexcoords <- function (dx, dy = NULL, n = 1, sep = NULL){

  # from hexbin::hexcoords

  stopifnot(length(dx) == 1)

  dy <- dy %||% dx/sqrt(3)

  if(is.null(sep)){
    list(x = rep.int(c(dx, dx, 0,-dx,-dx, 0),
                     n),
         y = rep.int(c(dy,-dy, -2 * dy, -dy, dy, 2 * dy),
                     n),
         no.sep = TRUE)
  }else{
    list(x = rep.int(c(dx, dx, 0, -dx, -dx, 0, sep),
                     n),
         y = rep.int(c(dy, -dy, -2 * dy, -dy, dy, 2 * dy, sep),
                     n),
         no.sep = FALSE)
  }
}

hex_coords <- function(x, y, unitcell_x = 1, unitcell_y = 1){

  # data.frame(
  #   x = hexbin::hexcoords(unitcell_x)$x + x,
  #   y = hexbin::hexcoords(unitcell_y)$y + y
  # )
  data.frame(x = my_hexcoords(unitcell_x)$x + x,
             y = my_hexcoords(unitcell_y)$y + y)
  # data.frame(
  #   x = c(1, 1, 0, -1, -1, 0) + x,
  #   y = c(0.577350269189626, -0.577350269189626, -1.15470053837925, -0.577350269189626,
  #     0.577350269189626, 1.15470053837925) + y
  # )
}
