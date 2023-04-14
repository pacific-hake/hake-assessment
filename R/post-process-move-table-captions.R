#' Move certain table captions, usually to left-justify. There is no way
#' to center kable tables and left-justify the captions to the left side
#' of the page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_move_table_captions <- function(x){

  pat <- "\\\\caption\\{\\\\label\\{tab:catches-us\\}"
  ind <- grep(pat, x)
  if(length(ind)){
    lst <- post_process_extract_chunks(x, ind, ind)
    lst$inbetween[[1]] <- c(lst$inbetween[[1]], "\\captionsetup{margin=-4pt}")
    x <- post_process_interlace_chunks(lst)
  }

  pat <- "\\\\caption\\{\\\\label\\{tab:main-assessment-changes-table\\}"
  ind <- grep(pat, x)
  if(length(ind)){
    lst <- post_process_extract_chunks(x, ind, ind)
    lst$inbetween[[1]] <- c(lst$inbetween[[1]], "\\captionsetup{margin=-26pt}")
    x <- post_process_interlace_chunks(lst)
  }

  pat <- "\\\\caption\\{\\\\label\\{tab:main-median-posterior-table\\}"
  ind <- grep(pat, x)
  if(length(ind)){
    lst <- post_process_extract_chunks(x, ind, ind)
    lst$inbetween[[1]] <- c(lst$inbetween[[1]], "\\captionsetup{margin=-22pt}")
    x <- post_process_interlace_chunks(lst)
  }

  x
}