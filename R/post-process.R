#' Post-process the TEX file output by Bookdown
#'
#' @details
#' Add centering for section names,
#'
#' @param accessibility_tagging Logical. If `TRUE`, include lines for
#' LaTeX `tagpdf` package to make a more web-accessible document. This will
#' make the compile time much longer and is typically used when done the
#' document and doing the final compile for government web distribution
#' @param ... Arguments passed to [post_process_table_of_contents()]
#'
#' @return Nothing, overwrite the file `fn` with the modified TEX
#' @export
post_process <- function(x,
                         prepub = FALSE,
                         accessibility_tagging = FALSE,
                         ...){

  if(accessibility_tagging){
    x <- c(
      "\\RequirePackage{pdfmanagement-testphase}",
      paste0("\\DocumentMetadata{testphase=phase-II, uncompress, ",
             "pdfstandard=A-2U, lang=en-US}"),
      x)
  }

  dc_ind <- grep("documentclass", x)
  if(!length(dc_ind)){
    stop("\\documentclass not found, document is not valid LaTeX and ",
         "cannot be built",
         call. = FALSE)
  }

  # Remove page number from title page
  title_ind <- grep("^\\\\maketitle", x)
  if(!length(title_ind)){
    stop("`\\maketitle` not found. You must be using the `\\maketitle` ",
         "method to produce the title page for this document",
         call. = FALSE)
  }
  pre <- x[1:(title_ind)]
  post <- x[(title_ind + 1):length(x)]
  x <- c(pre, "\\thispagestyle{empty}", post)

  # Table of contents injection ----
  #x <- post_process_table_of_contents(x, ...)

  # Change from book/chapter back to article/section ----
  x <- post_process_convert_to_article(x)

  # Placements for figures and tables ----
  x <- post_process_set_object_placement(x, ...)

  # Longtable customization ----
  x <- post_process_longtables(x, ...)

  # Landscape figures and table customization
  x <- post_process_landscape_figures(x)
  x <- post_process_landscape_tables(x)
  x <- post_process_fix_landscape_issues(x)

  # Table caption alignment ----
  x <- post_process_move_table_captions(x)

  # Figure/table lettering/numbering ----
  x <- post_process_add_counters(x)

  # Subtract vertical space before section headers ----
  x <- post_process_subtract_section_space(x)

  x
}