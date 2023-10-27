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
#' @param toc_underline_links Logical. If `TRUE` make all the ljnks in the
#' TOC and the section links in the document text be underlined
#' @param toc_underline_link_color If `toc_underline_links` is `TRUE`, this
#' color will be the underline color. See LaTeX package `xcolor` for allowable
#' colors
#' @param toc_link_text_color If `toc_underline_links` is `FALSE`, this color
#' will be the color of the link text (without underlines). See LaTeX package
#' `xcolor` for allowable colors
#' @param ... Absorb arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_table_of_contents <- function(
    x,
    toc_depth = 2,
    toc_section_indent_inch = 0,
    toc_section_num_width_inch = 0.2,
    toc_subsection_indent_inch = 0.3,
    toc_subsection_num_width_inch = 0.35,
    toc_subsubsection_indent_inch = 0.5,
    toc_subsubsection_num_width_inch = 0.45,
    toc_underline_links = FALSE,
    toc_underline_link_color = "blue",
    toc_link_text_color = "black",
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
         "document LaTeX. It needs to be present in the `preamble.tex` file",
         call. = FALSE)
  }
  snd_pat <- "\\\\setcounter\\{secnumdepth\\}(\\{[0-9]+\\})"
  secnumdepth_ind <- grep(snd_pat, x)
  if(!length(secnumdepth_ind)){
    stop("The line \\setcounter{secnumdepth}{X} was not found in the ",
         "document LaTeX. It needs to be present in the `preamble.tex` file",
         call. = FALSE)
  }
  ind <- secnumdepth_ind[which(abs(secnumdepth_ind - tocloft_ind) < 10)]
  x[ind] <- paste0("\\setcounter{secnumdepth}{", toc_depth, "}")

  # Remove color from TOC links
  # Inject hyperref setup for link colors
  start_ind <- grep("hypersetup", x)
  if(!length(start_ind)){
    #grep("\\\\title\\{", x)
  }
  if(length(start_ind) > 1){
    stop("\\hypersetup was found more than once in the LaTeX code",
         call. = FALSE)
  }
  # Look for closing brace, keeping a count of open ones
  # Assumes closing brace is not on the same line as opening one
  end_ind <- start_ind + 1
  ob_count <- 1
  repeat{
    if(end_ind == length(x)){
      stop("Could not find matching end brace for the \\hypersetup call",
           call. = FALSE)
    }
    if(!ob_count){
      break
    }
    for(i in seq_len(nchar(x[end_ind]))){
      ch <- substr(x[end_ind], i, i)
      if(ch == "{"){
        ob_count <- ob_count + 1
      }else if(ch == "}"){
        ob_count <- ob_count - 1
      }
    }
    end_ind <- end_ind + 1
  }

  # Remove the hypersetup chunk and add replacement
  x <- c(x[1:(start_ind - 1)],
         "%",
         "% The following code was injected by",
         "% hake::post_process_table_of_contents()",
         "%",
         "\\hypersetup{",
         paste0("colorlinks = ", ifelse(toc_underline_links,
                                        "false",
                                        "true"),
                ","),
         "plainpages = false,",
         paste0("linkcolor = ",
                toc_link_text_color,
                ","),
         paste0("linkbordercolor = ",
                toc_underline_link_color,
                ","),
         "pdfborderstyle = {/S/U/W 1},",
         "citecolor = black,",
         #"urlcolor = black",
         "pdflang = {en-US},",
         paste0("pdftitle = {",
                # `doc_title` created in 002-load-globals.rmd as a global
                doc_title,
                "},"),
         paste0("pdfauthor = {",
                # `doc_author` created in 002-load-globals.rmd as a global
                doc_author,
                "}"),
         "}",
         "%",
         "% End of injected code",
         "%",
         "",
         x[(end_ind + 1):length(x)])

  x
}