#' Add left and right footer text to the pages
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_headers_footers <- function(x,
                                             ...){

  left_ind <- grep("fancyfoot\\[L\\]\\{\\}", x)
  if(!length(left_ind)){
    stop("`\\fancyfoot[L]{}` not found in the preamble.tex code",
         call. = FALSE)
  }
  right_ind <- grep("fancyfoot\\[R\\]\\{\\}", x)
  if(!length(right_ind)){
    stop("`\\fancyfoot[R]{}` not found in the preamble.tex code",
         call. = FALSE)
  }

  x[left_ind] <- paste0("\\fancyfoot[L]{", footer_left, "}")
  x[right_ind] <- paste0("\\fancyfoot[R]{", footer_right, "}")

  x
}