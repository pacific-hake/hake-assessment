#' Add the disclaimer to the title slide for the presentations
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_disclaimer_presentations <- function(x, ...){

  browser()
  ind <- grep("\\date")
}