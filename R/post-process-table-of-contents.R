#' Insert the table of contents into TeX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param toc_depth The depth of the entries in the table of contents:
#' 2 means 1.2, 3 means 1.2.3, 4 means 1.2.3.4, etc
#' @param toc_section_indent_inch Indent value in inches for section headers in
#' the TOC
#' @param toc_section_num_width_inch Width of section numbers in inches in
#' the TOC
#' @param toc_subsection_indent_inch Indent value in inches for subsection headers
#' in the TOC
#' @param toc_subsection_num_width_inch Width of subsection numbers in inches in
#' the TOC
#' @param toc_subsubsection_indent_inch Indent value in inches for subsubsection
#' headers in the TOC
#' @param toc_subsubsection_num_width_inch Width of subsubsection numbers in inches
#' in the TOC
#' @param ... Absorb arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_table_of_contents <- function(
    x,
    toc_depth = NULL,
    toc_section_indent_inch = NULL,
    toc_section_num_width_inch = NULL,
    toc_subsection_indent_inch = NULL,
    toc_subsection_num_width_inch = NULL,
    toc_subsubsection_indent_inch = NULL,
    toc_subsubsection_num_width_inch = NULL,
    ...){

  toc_depth <- toc_depth %||% 2
  toc_section_indent_inch <- toc_section_indent_inch %||% 0
  toc_section_num_width_inch <- toc_section_num_width_inch %||% 0.2
  toc_subsection_indent_inch <- toc_subsection_indent_inch %||% 0.3
  toc_subsection_num_width_inch <- toc_subsection_num_width_inch %||% 0.35
  toc_subsubsection_indent_inch <- toc_subsubsection_indent_inch %||% 0.5
  toc_subsubsection_num_width_inch <- toc_subsubsection_num_width_inch %||% 0.45

  toc_indicator_line <- "TABLE OF CONTENTS GOES HERE"

  toc_ind <- grep(toc_indicator_line, x)
  if(!length(toc_ind)){
    stop("The Table of contents tag was not found. It must be present ",
         " and is:\n", toc_indicator_line)
  }
  if(length(toc_ind) > 1){
    stop("The Table of contents tag was found more than once in the ",
         "document. It must appear only once. It is:\n", toc_indicator_line)
  }

  x <- c(x[1:(toc_ind - 1)],
         "%",
         "% The following code was injected by",
         "% hake::post_process_table_of_contents()",
         "%",
         "\\newpage",
         "\\renewcommand{\\contentsname}{TABLE OF CONTENTS}",
         "\\renewcommand{\\cfttoctitlefont}{\\hfill\\Large\\bf}",
         "\\renewcommand{\\cftaftertoctitle}{\\hfill}",
         "\\makeatletter",
         paste0("\\renewcommand*\\l@section{\\@dottedtocline{1}",
                "{", toc_section_indent_inch, "in}",
                "{", toc_section_num_width_inch, "in}}"),
         paste0("\\renewcommand*\\l@subsection{\\@dottedtocline{2}",
                "{", toc_subsection_indent_inch, "in}",
                "{", toc_subsection_num_width_inch, "in}}"),
         paste0("\\renewcommand*\\l@subsubsection{\\@dottedtocline{3}",
                "{", toc_subsubsection_indent_inch, "in}",
                "{", toc_subsubsection_num_width_inch, "in}}"),
         "\\makeatother",
         "\\renewcommand{\\cftdot}{.}",
         "\\begin{center}",
         "\\tableofcontents",
         "\\end{center}",
         "\\thispagestyle{fancy}",
         "%",
         "% End of injected code",
         "%",
         x[(toc_ind + 1):length(x)])

  # Change the TOC depth
  tocloft_ind <- grep("\\\\usepackage\\{tocloft\\}", x)
  if(!length(tocloft_ind)){
    stop("The line \\usepackage{tocloft} was not found in the ",
         "document LaTeX. It needs to be present in the `preamble.tex` file")
  }
  tocdepth_ind <- grep("\\\\setcounter\\{tocdepth\\}", x)
  if(!length(tocdepth_ind)){
    stop("The line \\setcounter{tocdepth}{x} was not found in the ",
         "document LaTeX. It needs to be present in the `preamble.tex` file")
  }
  x[tocdepth_ind] <- paste0("\\setcounter{tocdepth}{", toc_depth, "}")

  x
}