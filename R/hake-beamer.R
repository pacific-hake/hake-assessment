#' Custom function to allow the post processing steps to be run before
#' PDF rendering for beamer presentations
#'
#' @param ...  Variables from the YAML code (in `000-launcher.rmd``)
#' passed to [bookdown::pdf_document2()] and [post_process()]
#'
#' @return The output from [bookdown::pdf_document2()]
#' @export
hake_beamer <- function(...){

  base <- bookdown::beamer_presentation2(...)

  base$knitr$opts_chunk$comment <- NA

  options(bookdown.post.latex = function(x){
    post_process_beamer(x = x, ...)
  })

  base
}
