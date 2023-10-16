#' Insert the table of contents into TeX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param section_indent_inch Indent value in inches for section headers in
#' the TOC
#' @param section_num_width_inch Width of section numbers in inches in
#' the TOC
#' @param subsection_indent_inch Indent value in inches for subsection headers
#' in the TOC
#' @param subsection_num_width_inch Width of subsection numbers in inches in
#' the TOC
#' @param subsubsection_indent_inch Indent value in inches for subsubsection
#' headers in the TOC
#' @param subsubsection_num_width_inch Width of subsubsection numbers in inches
#' in the TOC
#' @param ... Absorb arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_table_of_contents <- function(x,
                                           section_indent_inch = 0,
                                           section_num_width_inch = 0.2,
                                           subsection_indent_inch = 0.3,
                                           subsection_num_width_inch = 0.35,
                                           subsubsection_indent_inch = 0.5,
                                           subsubsection_num_width_inch = 0.45,
                                           ...){

  toc_indicator_line <- "TABLE OF CONTENTS GOES HERE"

  toc_ind <- grep(toc_indicator_line, x)
  if(!length(toc_ind)){
    stop("The Table of contents tag was not found. It must be present ",
         " and is:\n", toc_indicator_line,
         call. = FALSE)
  }
  if(length(toc_ind) > 1){
    stop("The Table of contents tag was found more than once in the ",
         "document. It must appear only once. It is:\n", toc_indicator_line,
         call. = FALSE)
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
                "{", section_indent_inch, "in}",
                "{", section_num_width_inch, "in}}"),
         paste0("\\renewcommand*\\l@subsection{\\@dottedtocline{2}",
                "{", subsection_indent_inch, "in}",
                "{", subsection_num_width_inch, "in}}"),
         paste0("\\renewcommand*\\l@subsubsection{\\@dottedtocline{3}",
                "{", subsubsection_indent_inch, "in}",
                "{", subsubsection_num_width_inch, "in}}"),
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

  # Fix line binders (~) ----
  # bookdown turns ~ into "\\textasciitilde " in the TEX, so we change it back
  # Don't remove the space-* at the end of the pattern, some cases
  # eg. (million~t) have a space and some don't eg. (Fig~\@ref{})
  x <- gsub("\\\\textasciitilde *", "~", x)
  # In captions, the two escape characters are not correctly dealt with,
  # because it is technically not Rmarkdown code inside the caption text in
  # R function calls. this makes it so we can use \\@ref() inside those
  # captions
  x <- gsub("\\\\\\\\ref", "\\\\ref", x)

  # Change double-dashes to en-dashes ----
  x <- gsub("-- ", "\\\\textendash\\\\ ", x)
  x <- gsub("--", "\\\\textendash ", x)

  x
}