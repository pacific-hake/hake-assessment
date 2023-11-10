#' Add extra headers needed for tables, colors in tables, special
#' cell formats and page numbering
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer_add_extra_headers_etc <- function(x, ...){

  ind <- grep("\\\\ifLuaTeX", x)

  if(!length(ind)){
    stop("`\\ifLuaTex` not found. It is needed to inject all the header ",
         "calls so the tables work properly and so page numbers show up")
  }
  if(length(ind) > 1){
    stop("`\\ifLuaTex` found more than once. It must be present once only ",
         "to inject all the header calls so the tables work properly and ",
         "so page numbers show up")
  }

  pre <- x[1:(ind - 1)]
  post <- x[ind:length(x)]

  data <- c(paste0("\\usepackage{makecell, booktabs, makecell, array, ",
                   "longtable, bm, colortbl, latexcolors}"),
            paste0("\\makeatletter\\def\\fnum@table{\\usebeamercolor",
                   "{caption name}\\usebeamerfont*{caption name}",
                   "\\tablename~\\thetable}\\makeatother"),
            "\\AtBeginSubsection{}",
            "\\AtBeginSection{}",
            paste0("\\newcommand{\\PreserveBackslash}[1]{",
                   "\\let\\temp=\\\\#1\\let\\\\=\\temp}"),
            paste0("\\newcolumntype{C}[1]{>{\\PreserveBackslash",
                   "\\centering}p{#1}}"),
            paste0("\\newcolumntype{R}[1]{>{\\PreserveBackslash",
                   "\\raggedleft}p{#1}}"),
            paste0("\\newcolumntype{L}[1]{>{\\PreserveBackslash",
                   "\\raggedright}p{#1}}"),
            "\\setbeamertemplate{navigation symbols}{}",
            "\\setbeamertemplate{footline}[page number]",
            "\\newcommand{\\bsmall}{\\begin{small}}",
            "\\newcommand{\\esmall}{\\end{small}}",
            "\\newcommand{\\btiny}{\\begin{tiny}}",
            "\\newcommand{\\etiny}{\\end{tiny}}")

  c(pre,
    data,
    post)
}
