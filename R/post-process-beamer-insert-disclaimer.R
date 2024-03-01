#' Insert a disclaimer notice at the bottom of the title page of beamer
#' presentations
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param show_disclaimer Logical. If `TRUE`, show the disclaimer text at the
#' bottom of the title slide. If `FALSE`, no text will be added to the title
#' slide
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_insert_disclaimer <- function(x,
                                                  show_disclaimer = TRUE,
                                                  ...){

  if(!show_disclaimer){
    return(x)
  }

  ind <- grep("\\\\usebeamerfont\\{subtitle\\}", x)

  if(!length(ind)){
    stop("The \\usebeamerfont{subtitle} tag was not found. It must be present ",
         " to insert the disclaimer on the title page")
  }
  if(length(ind) > 1){
    stop("The \\usebeamerfont{subtitle} tag was found more than once in the ",
         "document. It must appear only once to insert the disclaimer on ",
         "the title page")
  }

  pre <- x[1:(ind - 1)]
  post <- x[ind:length(x)]

  x <- c(pre,
         paste0("\\centering \\tiny \\textcolor{blue}{\\parbox[c]{12cm}",
                "{Disclaimer: These materials do not constitute a formal ",
                "publication and are for information only. They are in a ",
                "pre-review, pre-decisional state and should not be ",
                "formally cited or reproduced. They are to be considered ",
                "provisional and do not represent any determination or ",
                "policy of NOAA or the Department of Commerce. \\newline ",
                "\\newline The results presented here have yet to be ",
                "approved by the  Scientific Review Group (SRG).}}"),
         post)

  x
}