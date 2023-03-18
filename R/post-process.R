#' Post-process the TEX file output by Bookdown
#'
#' @details
#' Add centering for section names,
#'
#' @param fn The TEX filename
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
#'
#' @return Nothing, overwrite the file `fn` with the modified TEX
#' @export
post_process <- function(fn,
                         section_indent_inch = 0,
                         section_num_width_inch = 0.1,
                         subsection_indent_inch = 0.2,
                         subsection_num_width_inch = 0.1,
                         subsubsection_indent_inch = 0.4,
                         subsubsection_num_width_inch = 0.1){

  fn_base <- tools::file_path_sans_ext(fn)
  fn_ext <- tools::file_ext(fn)
  fn_bck <- paste0(fn_base, "-bck.", fn_ext)
  file.copy(fn, fn_bck, overwrite = TRUE)

  x <- readLines(fn)

  # Allow unnumbered headers with custom labels in format {-, #label_name}
  # inds <- grep("\\{-, *\\\\#.*\\}", x)
  # preinds <- inds - 1
  # if(length(inds)){
  #   labels <- gsub("^.*section\\{.*?\\{ *-, *\\\\#([a-zA-Z:-]+).*",
  #                  "\\1",
  #                  x[inds])
  #   # Fix hyperlink references 1 line above sections
  #   hyperlinks <- map2_chr(x[preinds], labels, ~{
  #     sub("^(\\\\hypertarget\\{).*(\\}.*)$",
  #         paste0("\\1",
  #                .y,
  #                "\\2"),
  #         .x)
  #   })
  #   x[preinds] <- hyperlinks
  #
  #   # Fix labels on the actual lines of the sections
  #   pat <- paste0("^(.*section\\{.*?\\{ *-, *\\\\#[a-zA-Z:-]+",
  #                 "\\\\}\\}\\\\label\\{)[a-zA-Z-]+(\\}\\})$")
  #   sec_hdrs <- map2_chr(x[inds], labels, ~{
  #     sub(pat,
  #         paste0("\\1",
  #                .y,
  #                "\\2"),
  #         .x)
  #   })
  #
  #   # Remove the {-,#label} par
  #   pat <- paste0("^(.*section\\{[a-zA-Z0-9 ]+).*$")
  #   sec_hdrs <- map2_chr(sec_hdrs, labels, ~{
  #     sub(pat,
  #         paste0("\\1}\\\\label{",
  #                .y,
  #                "}}"),
  #         .x)
  #   })
  #   x[inds] <- sec_hdrs
  # }

  # Make section headers uppercase and centered ----
  section_inds <- grep("\\\\section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]

    chunks <- map2(section_inds[1:(length(section_inds) - 1)],
                   section_inds[2:length(section_inds)], \(st, en){
                     c("\\begin{center}",
                       "\\sectionfont{\\MakeUppercase}",
                       x[st],
                       "\\end{center}",
                       x[(st + 1):(en - 1)])
                   })
    end <- c("\\begin{center}",
             "\\sectionfont{\\MakeUppercase}",
             x[section_inds[length(section_inds)]],
             "\\end{center}",
             x[(section_inds[length(section_inds)] + 1):length(x)])

    x <- c(beg, unlist(chunks), end)
  }

  # Make subsections uppercase ----
  section_inds <- grep("\\\\(sub){1}section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]

    if(length(section_inds) > 1){
      chunks <- map2(section_inds[1:(length(section_inds) - 1)],
                     section_inds[2:length(section_inds)], \(st, en){
                       c("\\subsectionfont{\\MakeUppercase}",
                         x[st],
                         x[(st + 1):(en - 1)])
                     })
      end <- c("\\subsectionfont{\\MakeUppercase}",
               x[section_inds[length(section_inds)]],
               x[(section_inds[length(section_inds)] + 1):length(x)])

      x <- c(beg, unlist(chunks), end)
    }else{
      x <- c(beg,
             "\\subsectionfont{\\MakeUppercase}",
             x[section_inds[1]:length(x)])
    }
  }

  # Make subsubsections uppercase ----
  section_inds <- grep("\\\\(sub){2}section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]

    if(length(section_inds) > 1){
      chunks <- map2(section_inds[1:(length(section_inds) - 1)],
                     section_inds[2:length(section_inds)], \(st, en){
                       c("\\subsubsectionfont{\\MakeUppercase}",
                         x[st],
                         x[(st + 1):(en - 1)])
                     })
      end <- c("\\subsubsectionfont{\\MakeUppercase}",
               x[section_inds[length(section_inds)]],
               x[(section_inds[length(section_inds)] + 1):length(x)])

      x <- c(beg, unlist(chunks), end)
    }else{
      x <- c(beg,
             "\\subsubsectionfont{\\MakeUppercase}",
             x[section_inds[1]:length(x)])
    }
  }

  # Insert the Table of contents ----
  toc_indicator_line <- paste0("TABLE OF CONTENTS GOES HERE - ",
                               "DO NOT DELETE OR MODIFY THIS LINE")
  toc_ind <- grep(toc_indicator_line, x)
  if(!length(toc_ind)){
    stop("The Table of contents indicator line was not found. ",
         "It must be present somewhere and is:\n",
         toc_indicator_line,
         call. = FALSE)
  }
  if(length(toc_ind) > 1){
    stop("The Table of contents indicator line was found more than once ",
         "in the document. It must appear only once. It is:\n",
         toc_indicator_line,
         call. = FALSE)
  }

  x <- c(x[1:(toc_ind - 1)],
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
         x[(toc_ind + 1):length(x)])

  # Fix line binders (~) ----
  # bookdown turns ~ into "\\textasciitilde " in the TEX, so we change it back
  x <- gsub("\\\\textasciitilde ", "~", x)

  writeLines(x, fn)

}