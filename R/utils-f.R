#' Format a number
#'
#' @details
#' The formatted number will have comma-separated thousands, millions, etc.
#' and the exact number of decimal points given by `digits`, even if the
#' last one is a zero.
#'
#' @param x The number
#' @param digits The number of decimal points to use
#'
#' @return A Character string. A formatted version of the number `x`.
#' @export
f <- function(x, digits = 0){

  if(is.null(x)){
    return(x)
  }

  format(round(x,
               digits),
         big.mark = ",",
         nsmall = digits)
}
