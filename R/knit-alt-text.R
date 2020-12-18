#' Knit the document and place `pdftooltip{}` wrappers around `includegraphics{}` statements
#' in the output TEX file qhich are associated with `knitr` chunks containing the chunk
#' option `is.fig=TRUE`
#'
#' @param doc_name Base name of the document
#'
#' @return Nothing
#' @export
knit_alttext <- function(doc_name = "hake-assessment"){
  knit(paste0(doc_name, ".rnw"))
  # The knitting process creates the global `alt_fig_text`
  add_alt_text(paste0(doc_name, ".tex"), alt_fig_text)
  invisible()
}