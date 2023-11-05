#' Change tildes (~) so that they will make it through the Pandoc conversion
#' to be processed in latex as intended
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments intended for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_fix_tildes <- function(x, ...){

  # Fix line binders (~) ----
  # bookdown turns ~ into "\\textasciitilde " in the TEX, so we change it back
  # Don't remove the space-* at the end of the pattern, some cases
  # eg. (million~t) have a space and some don't eg. (Fig~\@ref{})
  x <- gsub("\\\\textasciitilde *", "~", x)
  # In captions, the two escape characters are not correctly dealt with,
  # because it is technically not Rmarkdown code inside the caption text in
  # R function calls. this makes it so we can use \\@ref() inside those
  # captions
  x <- gsub("\\\\\\\\ref", "\\\\ref", x)

  # Change double-dashes to en-dashes ----
  x <- gsub("-- ", "\\\\textendash\\\\ ", x)
  x <- gsub("--", "\\\\textendash ", x)

  x
}