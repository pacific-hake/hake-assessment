#' Post-process the TEX file output by Bookdown
#'
#' @details
#' Add centering for section names,
#'
#' @param fn The TEX filename
#' @param accessibility Logical. If `TRUE`, include lines for LaTeX `tagpdf`
#' package to make a more web-accessible document. This will make the compile
#' time much longer and is typically used when done the document and doing the
#' final compile for government web distribution
#' @param figs_dir The name of the directory in which the knitr-built
#' figures reside. Used for matching figure import locations inside the tex
#' file
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
                         accessibility = FALSE,
                         figs_dir = ifelse(exists("knitr_figs_dir"),
                                           knitr_figs_dir,
                                           NULL),
                         section_indent_inch = 0,
                         section_num_width_inch = 0.2,
                         subsection_indent_inch = 0.2,
                         subsection_num_width_inch = 0.3,
                         subsubsection_indent_inch = 0.4,
                         subsubsection_num_width_inch = 0.4){

  if(is.null(figs_dir)){
    stop("`figs_dir` is `NULL`. You must provide the directory name ",
         "in which the knitr-built figures reside, relative to the doc ",
         "directory",
         call. = FALSE)
  }

  fn_base <- tools::file_path_sans_ext(fn)
  fn_ext <- tools::file_ext(fn)
  fn_bck <- paste0(fn_base, "-bck.", fn_ext)

  x <- readLines(fn)

  modification_text <- "% This file has been modified by hake::post_process()"
  hasbeen_modified <- grep(modification_text, x)
  if(length(hasbeen_modified)){
    if(file.exists(fn_bck)){
      # Copy from the backup, erasing the previous post processing changes
      file.copy(fn_bck, fn, overwrite = TRUE)
      x <- readLines(fn)
    }else{
      stop("The file `", fn, "` has already been modified by the ",
           "hake::post_process() function, and the backup file `", fn_bck,
           "` was not found. Re-run the bookdown command to create `", fn, "`",
           call. = FALSE)
    }
  }else{
    file.copy(fn, fn_bck, overwrite = TRUE)
  }

  if(accessibility){
    x <- c(
      "\\RequirePackage{pdfmanagement-testphase}",
      "\\DocumentMetadata{testphase=phase-II, uncompress, pdfstandard=A-2U, lang=en-US}",
      x)
  }

  dc_ind <- grep("documentclass", x)
  if(!length(dc_ind)){
    stop("\\documentclass not found, document cannot be built",
         call. = FALSE)
  }

  # Make section headers uppercase and centered ----
  section_inds <- grep("\\\\section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]
    if(length(section_inds) == 1){
      x <- c(beg,
             "\\begin{center}",
             "\\sectionfont{\\MakeUppercase}",
             x[section_inds[1]],
             "\\end{center}",
             x[(section_inds[1] + 1):length(x)])
    }else{
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
  }

  # Make subsections uppercase ----
  section_inds <- grep("\\\\(sub){1}section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]
    if(length(section_inds) == 1){
      x <- c(beg,
             "\\subsectionfont{\\MakeUppercase}",
             x[section_inds[1]:length(x)])
    }else{
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
    }
  }

  # Make subsubsections uppercase ----
  section_inds <- grep("\\\\(sub){2}section\\*?\\{.*(})?", x)
  if(length(section_inds)){
    beg <- x[1:(section_inds[1] - 1)]
    if(length(section_inds) == 1){
      x <- c(beg,
             "\\subsubsectionfont{\\MakeUppercase}",
             x[section_inds[1]:length(x)])
    }else{
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

  # Set manual figure placements ----
  check_ind <- function(ind, label){
    if(!length(ind)){
      stop("Figure label `", label, "`, was not found in the tex file.",
           "It is needed to set the plot placement in post-processing",
           call. = FALSE)
    }
    if(length(ind) > 1){
      stop("There was more than 1 figure label `", label, "`, found in ",
           "the tex file. Only one can exist to correctly set the plot ",
           "placement in post-processing",
           call. = FALSE)
    }
  }
  # Search previous `n-lines` tex lines for \begin{figure} to match the line at
  # `fig_ind`
  get_beg_fig_ind <- function(fig_ind, n_lines = 5){
    srch_lines <- x[(fig_ind - n_lines):(fig_ind - 1)]
    beg_fig_ind <- grep("\\\\begin\\{figure\\}", srch_lines)
    srch_line <- gsub("\\{", "\\\\{", srch_lines[beg_fig_ind])
    srch_line <- gsub("\\}", "\\\\}", srch_line)
    srch_line <- gsub("\\[", "\\\\[", srch_line)
    srch_line <- gsub("\\]", "\\\\]", srch_line)
    srch_line <- gsub("\\\\begin", "\\\\\\\\begin", srch_line)
    if(!length(srch_line)){
      stop("Did not find the line \begin{figure} associated with the ",
           "figure inclusion line `", x[fig_ind], "`. Consider increasing ",
           "the number of lines searched above it",
           call. = FALSE)
    }
    grep(srch_line, x)
  }
  # Replace the figure line in the tex code.
  replace_beg_figure_line <- function(ind, place){
    gsub("(\\[[a-zA-Z\\!]+\\])(.*)$", paste0("[", place, "]\\2"), x[ind])
  }
  # Modify the tex code `x` by changing the figure label placement code for the
  # knitr_label` to the value of `place`
  #
  # The place parameter can be any of these (or a combination of them)
  # h: Place the float here, i.e., approximately at the same point it occurs
  #    in the source text.
  # t: Position at the top of the page.
  # b: Position at the bottom of the page.
  # p: Put on a special page for floats only.
  # !: Override internal parameters LaTeX uses for determining “good” float
  #    positions.
  # H: Place the float at precisely the location in the LaTeX code. This
  #    requires the float package
  set_figure_placement <- function(knitr_label, place){
    fig_label <- paste0(figs_dir, knitr_label)
    fig_ind <- grep(fig_label, x)
    if(!length(fig_ind)){
      warning("Figure label `", knitr_label, "` not found. Bypassing...")
      return(invisible())
    }
    check_ind(fig_ind, fig_label)
    beg_fig_ind <- get_beg_fig_ind(fig_ind)
    # Replace any placement values
    x[beg_fig_ind] <<- replace_beg_figure_line(beg_fig_ind, place)
    invisible()
  }

  # Executive summary catch plot placement----
  set_figure_placement("es-catches-1", "!b")

  # Mark file with modification text ----
  x <- c(modification_text,
         x)

  writeLines(x, fn)
}