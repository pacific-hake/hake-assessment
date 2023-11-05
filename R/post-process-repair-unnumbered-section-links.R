#' Add phantomsection tag to sections that are unnumbered to force the
#' bookmark links to work right for them
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_repair_unnumbered_section_links <- function(x, ...){

  inds <- grep("^\\\\(sub)*section\\*\\{", x)
  if(!length(inds)){
    return(x)
  }

  for(i in seq_along(inds)){
    inds <- grep("^\\\\(sub)*section\\*\\{", x)
    pre <- x[1:inds[i]]
    post <- x[(inds[i] + 1):length(x)]
    x <- c(pre,
           "%",
           "% The following code was injected by",
           "% hake::post_process_repair_unnumbered_section_links()",
           "%",
           "\\phantomsection",
           "%",
           "% End of injected code",
           "%",
           post)
  }

  x
}
