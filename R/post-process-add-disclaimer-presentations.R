#' Add the disclaimer to the title slide for the presentations
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_disclaimer_presentations <- function(x,
                                                      show_disclaimer = TRUE,
                                                      ...){
  ind <- grep("\\\\date", x)
  if(!length(ind)){
    stop("The \\date line was not found. It must be present to add the ",
         "disclaimer to the title page")
  }
  if(length(ind) > 1){
    stop("The \\date line was found more than once. It must be present only ",
         "once to add the disclaimer to the title page")
  }
  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]

  x <- c(pre,
         paste0("{\\tiny Disclaimer: These materials do not constitute a ",
                "formal publication and are for information only. They are ",
                "in a pre-review, pre-decisional state and should not be ",
                "formally cited or reproduced. They are to be considered ",
                "provisional and do not represent any determination or ",
                "policy of NOAA or the Department of Commerce.}"),
         post)

  x
}