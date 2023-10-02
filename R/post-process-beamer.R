
post_process_beamer <- function(tex_fn = "beamer-hake-data.tex"){

  d <- readLines(tex_fn)
  writeLines(d, "backup.tex")

  # Insert title page logos
  ind <- grep("\\usepackage\\{pgfpages\\}", d)
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
      "  \\insertauthor",
      "  \\vskip22pt",
      "  \\insertdate",
      "  \\end{center}",
      "  \\usebeamerfont{subtitle}\\usebeamercolor[fg]{subtitle}\\insertsubtitle\\par",
      "  \\vskip0pt plus 1filll",
    "}",
    "")

  d <- c(pre, dat, post)

  # Remove headline from title page (section buttons)
  ind <- grep("\\\\begin\\{document\\}", d)
  pre <- d[1:ind]
  post <- d[(ind + 1):length(d)]
  dat <- "\\setbeamertemplate{headline}{}"
  d <- c(pre, dat, post)

  ind <- grep("\\\\frame\\{\\\\titlepage\\}", d)
  pre <- d[1:ind]
  post <- d[(ind + 1):length(d)]
  dat <- "\\setbeamertemplate{headline}[Singapore theme]"
  d <- c(pre, dat, post)

  writeLines(d, tex_fn)
}