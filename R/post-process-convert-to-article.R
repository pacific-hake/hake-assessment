#' Replace bok referrences with article and chapter with section
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
post_process_convert_to_article <- function(x){

  x <- gsub("\\{book\\}", "\\{article\\}", x)
  x <- gsub("\\\\chapter", "\\\\section", x)
  x <- gsub("\\{chapter\\}", "{section}", x)

  x
}