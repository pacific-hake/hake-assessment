#' Check the input LaTex for errors or omissions before post processing occurs
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return `[invisible()]
#' @export
post_process_error_check <- function(x, ...){

  dc_ind <- grep("documentclass", x)
  if(!length(dc_ind)){
    stop("\\documentclass not found, document is not valid LaTeX and ",
         "cannot be built")
  }

  article_ind <- grep("\\{article\\}", x)
  if(!length(article_ind)){
    stop("{article} not found, document must have a documentclass type ",
         "`article`")
  }

  return(invisible())
}