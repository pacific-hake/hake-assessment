#' Post-process Beamer Rmarkdown TeX files
#'
#' @details
#' Inserts the logos and picture on the title slide, and makes the disclaimer
#' text tiny so it fits on the slide
#' Creates a backup of the `tex_fn` file before modifying it. The backup file
#' is called 'backup.tex',
#' Must run `lualatex beamer-hake-data.tex;lualatex beamer-hake-data.tex` in
#' a command-line terminal once done to re-create the PDF.
#'
#' @param tex_fn The TeX file name
#'
#' @return Nothing, overwrites the input file `tex_fn`
#' @export
post_process_beamer <- function(tex_fn = "beamer-hake-data.tex"){

  d <- readLines(tex_fn)
  writeLines(d, "backup.tex")

  # Insert title page logos
  ind <- grep("\\usepackage\\{pgfpages\\}", d)
  if(length(ind)){
    pre <- d[1:ind]
    post <- d[(ind + 1):length(d)]
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

    d <- c(pre, dat, post)
  }

  # Add theme
  # ind <- grep("\\\\begin\\{document\\}", d)
  # if(length(ind)){
  #   pre <- d[1:(ind - 1)]
  #   post <- d[ind:length(d)]
  #   dat <- c("", "\\usetheme{Singapore}", "")
  #   d <- c(pre, dat, post)
  # }

  # Remove headline from title page (section buttons)
  # ind <- grep("\\\\begin\\{document\\}", d)
  # if(length(ind)){
  #   pre <- d[1:ind]
  #   post <- d[(ind + 1):length(d)]
  #   dat <- c("", "\\setbeamertemplate{headline}{}", "")
  #   d <- c(pre, dat, post)
  # }

  # ind <- grep("\\\\frame\\{\\\\titlepage\\}", d)
  # if(length(ind)){
  #   pre <- d[1:ind]
  #   post <- d[(ind + 1):length(d)]
  #   dat <- c("", "\\setbeamertemplate{headline}[miniframes theme]")
  #   d <- c(pre, dat, post)
  # }

  # Make the disclaimer on the bottom of the title page tiny
  ind <- grep("\\\\subtitle\\{", d)
  if(length(ind)){
    pre <- d[1:(ind - 1)]
    post <- d[(ind + 1):length(d)]
    dat <- d[ind]
    dat <- gsub("\\\\subtitle\\{", "\\\\subtitle{\\\\tiny ", dat)
    d <- c(pre, dat, post)
  }

  # Remove captions from longtables
  ind <- grep("\\\\caption", d)
  if(length(ind)){
    d <- d[-ind]
  }
  writeLines(d, tex_fn)
}