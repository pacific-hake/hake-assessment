#' Bookdown function to build the hake assessment document
#'
#' @param latex_engine See [bookdown::pdf_document2()]
#' @param prepub Logical. Is the document a draft or the final document
#' @param line_nums Logical. Show line numbers
#' @param line_nums_mod If `line_nums` is `TRUE`, show every nth line number
#' @param pandoc_args  See [bookdown::pdf_document2()]
#' @param ...  Passed to [bookdown::pdf_document2()]
#'
#' @return The output from [bookdown::pdf_document2()]
#' @export
hake_pdf <- function(latex_engine = "lualatex",
                     prepub = FALSE,
                     accessibility_tagging = FALSE,
                     line_nums = FALSE,
                     line_nums_mod = 1,
                     pandoc_args = c("--top-level-division=chapter",
                                     "--wrap=none",
                                     "--default-image-extension=png"),
                     ...){

  base <- bookdown::pdf_document2(pandoc_args = pandoc_args,
                                  latex_engine = latex_engine,
                                  ...)

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")

  options(bookdown.post.latex = function(x){
    post_process(x = x,
                 prepub = prepub,
                 accessibility_tagging = accessibility_tagging)})

  on.exit(options(bookdown.post.late = old_opt))

  base
}
