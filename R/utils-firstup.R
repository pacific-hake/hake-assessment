#' Convert the first letter of a string to uppercase
#'
#' @param str The string
#'
#' @return The string `str` with the first letter in uppercase
#' @export
firstup <- function(str){

  gsub("(^[[:alpha:]])", "\\U\\1", str, perl = TRUE)
}