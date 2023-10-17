#' Insert the NOAA and DFO logos and a hake picture on the title page of a
#' beamer presentation
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_insert_title_page_logos <- function(x, ...){

  # Insert title page logos
  ind <- grep("\\usepackage\\{pgfpages\\}", x)

  if(!length(ind)){
    stop("The \\usepackage{pgfpages} tag was not found. It must be present ",
         " to insert the custom beamer title page",
         call. = FALSE)
  }
  if(length(ind) > 1){
    stop("The \\usepackage{pgfpages} tag was found more than once in the ",
         "document. It must appear only once to insert the custom beamer ",
         "title page",
         call. = FALSE)
  }

  pre <- x[1:ind]
  post <- x[(ind + 1):length(x)]
  dat <- c(
    "",
    "\\setbeamertemplate{title page}",
    "{",
    "  \\includegraphics[height=0.5in]{../../images/NOAA.eps}",
    "  \\hfill",
    "  \\includegraphics[height=0.5in]{../../images/DFO.eps}",
    "",
    "  \\vskip0pt plus 1fill",
    "  \\begin{center}",
    "  {\\usebeamerfont{title}\\usebeamercolor[fg]{title}\\inserttitle}\\\\",
    "  \\vskip22pt",
    "  \\includegraphics[height=1in, width=4in]{../../images/hake-on-board.eps}\\\\",
    "  \\insertauthor",
    "  \\vskip5pt",
    "  \\insertdate",
    "  \\end{center}",
    #"  \\tiny \\textcolor{blue}{Disclaimer: These materials do not constitute a formal publication and are for information only. They are in a pre-review, pre-decisional state and should not be formally cited or reproduced. They are to be considered provisional and do not represent any determination or policy of NOAA or the Department of Commerce.}\\par",
    "  \\usebeamerfont{subtitle}\\usebeamercolor[fg]{subtitle}\\insertsubtitle\\par",
    "  \\vskip0pt plus 1filll",
    "}",
    "")

  x <- c(pre, dat, post)

  x
}
