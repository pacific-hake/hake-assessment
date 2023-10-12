#' Render the Hake assessment document PDF file
#'
#' @param input The name of the launching `rmd` file. This should be the
#' same as the first `rmd` file named in the `_bookdown.yml` file.
#' See `input` argument for [bookdown::render_book()]
#' @param output_dir Which directory to build the PDF in.
#' See `output_dir` argument for [bookdown::render_book()]
#' @param ... Arguments to pass to the [bookdown::render_book()] and
#' [post_process()] functions
#'
#' @return Nothing
#' @export
render <- function(input = "000-launcher.rmd",
                   output_dir = ".",
                   ...){

  bookdown::render_book(envir = globalenv(),
                        ...)
}
