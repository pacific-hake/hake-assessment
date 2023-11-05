#' Post-process the TEX file output by Bookdown for beamer presentations
#'
#' @details
#' Called by [hake_beamer()] to post-process the LaTeX compiled by the
#' [bookdown] package
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_beamer <- function(x, ...){

  # Include some LaTeX packages, and custom macros including the ones for
  # special table widths such as L{2cm}, C{2cm}, and R{2cm} (example values)
  # used in the decision table
  x <- post_process_beamer_add_extra_headers_etc(x, ...)

  # Insert the NOAA and DFO logos and the hake picture on the title
  # page
  x <- post_process_beamer_insert_title_page_logos(x, ...)

  # Add disclaimer to the presentation title page. Requires that
  # `post_process_beamer_insert_title_page_logos()` was run previously
  x <- post_process_beamer_insert_disclaimer(x, ...)

  # Make the title page plain (no navigation bar or theme shadow and remove
  # extra blank title page
  x <- post_process_beamer_make_title_page_plain(x, ...)

  # Add horizontal lines to the decision table headers ----
  # across multiple columns
  x <- post_process_add_horiz_lines_decision_table(x, ...)

  # Replace tildes (~) in the code so they pas through Pandoc to LaTeX
  x <- post_process_fix_tildes(x, ...)

  x
}
