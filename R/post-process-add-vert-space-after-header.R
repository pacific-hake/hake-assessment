#' Add LaTeX vertical space code after the Executive Summary section header
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_vert_space_after_header <- function(x,
                                                     ...){

  ind <- grep("^\\\\addcontentsline\\{toc\\}\\{section\\}\\{Executive summary\\}$", x)
  if(length(ind)){
    pre <- x[1:ind]
    post <- x[(ind + 1):length(x)]
    x <- c(pre,
           "%",
           "% The following code was injected by",
           "% hake::post_process_add_vert_space_after_header()",
           "%",
           "\\vspace{5mm}",
           "%",
           "% End of injected code",
           "%",
           post)
  }

  x
}