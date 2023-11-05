#' Insert a disclaimer notice at the bottom of the title page of beamer
#' presentations
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_insert_disclaimer <- function(x, ...){

  ind <- grep("\\\\usebeamerfont\\{subtitle\\}", x)

  if(!length(ind)){
    stop("The \\usebeamerfont{subtitle} tag was not found. It must be present ",
         " to insert the disclaimer on the title page",
         call. = FALSE)
  }
  if(length(ind) > 1){
    stop("The \\usebeamerfont{subtitle} tag was found more than once in the ",
         "document. It must appear only once to insert the disclaimer on ",
         "the title page",
         call. = FALSE)
  }

  pre <- x[1:(ind - 1)]
  post <- x[ind:length(x)]

  x <- c(pre,
         paste0("\\tiny \\textcolor{blue}{Disclaimer: These materials ",
                "do not constitute a formal publication and are for ",
                "information only. They are in a pre-review, ",
                "pre-decisional state and should not be formally cited or ",
                "reproduced. They are to be considered provisional and do ",
                "not represent any determination or policy of NOAA or the ",
                "Department of Commerce.}\\par"),
         post)

  x
}