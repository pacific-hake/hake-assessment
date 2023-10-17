#' Post-process the TEX file output by Bookdown for beamer presentations
#'
#' @details
#' Called by [hake_beamer()] to post-process the LaTeX compiled by the
#' [bookdown] package
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer <- function(x, ...){

  # Add disclaimer to the presentation title pages
  x <- post_process_add_disclaimer_presentations(x, ...)

  x
}
