#' Removes the page number from the title page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' @param ... Absorbs other arguments not meant for this function
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_remove_title_page_number <- function(x, ...){

  title_ind <- grep("^\\\\maketitle", x)

  if(!length(title_ind)){
    stop("`\\maketitle` not found. You must be using the `\\maketitle` ",
         "method to produce the title page for this document")
  }

  pre <- x[1:(title_ind)]
  post <- x[(title_ind + 1):length(x)]

  x <- c(pre, "\\thispagestyle{empty}", post)

  x
}