#' Check the format of the margin command entered into the YAML header.
#'
#' @param fig_margins The string containing the command to verify
#'
#' @return [invisible()] if format is correct, or stops if the format is
#' incorrect
#' @export
yaml_check_margin_format <- function(fig_margins){

  margin_format_ok <-
    length(grep("^margin\\(\\d+, *\\d+, *\\d+, \\d+\\) *#?.*$",
                fig_margins))

  if(!margin_format_ok){
    stop("`figure_margin` in 000-launcher.rmd is not in the correct format. ",
         "It should be formatted this way:\nmargin(A, B, C, D) where A, B, C, ",
         "and D are integers representing the top, right, bottom, and left ",
         "margin sizes for all figures in the document")
  }

  invisible()
}
