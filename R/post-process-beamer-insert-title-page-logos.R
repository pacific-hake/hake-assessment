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
  browser()
  if(length(ind)){
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
      "  \\usebeamerfont{subtitle}\\usebeamercolor[fg]{subtitle}\\insertsubtitle\\par",
      "  \\vskip0pt plus 1filll",
      "}",
      "")

    x <- c(pre, dat, post)

    x
  }
}
