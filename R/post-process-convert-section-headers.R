#' Replace chapter with section for all sections, and prepend a "sub" to
#' each section, subsection etc to lower each by one level
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_convert_section_headers <- function(x,
                                                 ...){

  titlesec_ind <- grep("usepackage\\{titlesec\\}", x)
  x_pre <- x[1:(titlesec_ind - 1)]
  x_post <- x[(titlesec_ind + 1):length(x)]

  x <- c(x_pre,
         c("",
           "% Need the following to make the fourth level headers work right",
           "\\usepackage{titlesec}",
           "\\setcounter{secnumdepth}{4}",
           "\\titleformat{\\paragraph}",
           "{\\normalfont\\normalsize\\bfseries}{\\theparagraph}{1em}{}",
           "\\titlespacing*{\\paragraph}",
           "{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}",
           ""),
         x_post)

  x
}