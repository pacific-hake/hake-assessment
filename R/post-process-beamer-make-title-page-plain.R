#' Remove theme shadow and navigation bar from the title page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_make_title_page_plain <- function(x,...){

  ind <- grep("\\\\titlepage", x)

  if(!length(ind)){
    stop("The \\titlepage tag was not found. It must be present to remove ",
         "the theme shadow and navigation bar from the title page")
  }
  if(length(ind) > 1){
    stop("The \\titlepage tag was found more than once in the ",
         "document. It must appear only once to to remove the theme shadow ",
         "and navigation bar from the title page")
  }

  ind_next_begin_frame <- ind + 1
  repeat{
    if(length(ind_next_begin_frame) == length(x)){
      stop("There was no \\begin{frame} following the \\titlepage command")
    }
    is_begin_frame <- length(grep("^\\\\begin\\{frame\\}$",
                                  x[ind_next_begin_frame]))

    if(is_begin_frame){
      break
    }
    ind_next_begin_frame <- ind_next_begin_frame + 1
  }

  x[ind_next_begin_frame] <- paste0(x[ind_next_begin_frame], "[plain]")

  pre <- x[1:ind_next_begin_frame]
  post <- x[(ind_next_begin_frame + 1):length(x)]

  x <- c(pre,
         "\\titlepage",
         post)

  # Remove \\frame{\\titlepage} - it always comes before the changes we made
  # so we can use the original ind for this
  x <- x[-ind]

  x
}

