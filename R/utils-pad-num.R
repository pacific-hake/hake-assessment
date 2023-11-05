#' Pad the beginning of a number with zeroes
#'
#' @param num A vector of the numbers to pad
#' @param digits The number of characters that the resulting strings
#' should have
#'
#' @return A vector of strings of the padded numbers
#' @export
pad_num <- function(num, digits = 1){
  stopifnot(digits >= 1, !any(nchar(num) > digits))
  sapply(num,
         function(x){
           paste0(paste0(rep("0",
                             digits - nchar(as.character(x))),
                         collapse = ""),
                  as.character(x))})
}
