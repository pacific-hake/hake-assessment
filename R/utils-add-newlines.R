#' Replace '+' with a newline in the given string
#'
#' @details
#' Used mainly for ggplot y axis labels, if they are too long
#' and get cut off
#'
#' @param x A character string
#' @param ... Absorbs other arguments not meant for this function
#'
#' @return A modified character string
add_newlines <- function(x, ...){
  gsub("\\+", "\n", x)
}
