#' Post-process the TEX file output by Bookdown
#'
#' @details
#' Called by [hake_pdf()] to post-process the LaTeX compiled by the
#' [bookdown] package
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Arguments passed to all the post-processing functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process <- function(x, ...){

  # Make sure the LaTeX code is viable (basic checks)
  #post_process_error_check(x, ...)

  # Need to add this here - cannot be done anywhere else - to ensure the
  # xcolor colors are all loaded
  x <- post_process_add_xcolor_package(x, ...)

  # Change the font and font size according to YAML selections
  x <- post_process_modify_font_info(x, ...)

  # Remove the page number from the title page only
  x <- post_process_remove_title_page_number(x, ...)

  # Add headers and footers information to the pages
  x <- post_process_add_headers_footers(x, ...)

  # Add LaTeX code for the start of the appendices section. This is needed
  # to tell the LaTeX compiler to start numbering the sections using letters
  # and set up tables, figures, and equations to be of the format A.2 for
  # example. It also adds TOC information so this has to come before the TOC
  # post-processing step
  x <- post_process_add_start_appendices_code(x, ...)

  # Add counter reset for all appendices so each lettered appendix starts
  # at 1 again, e.g. A.1, A.2 ... B.1, B.2 ...
  x <- post_process_add_appendix_counter_resets(x, ...)

  # Table of contents injection ----
  x <- post_process_table_of_contents(x, ...)

  # Add any picture to the title page
  x <- post_process_add_picture_to_title_page(x, ...)

  # Modify the reference and URL link colors and types (underlined
  # or text-color-based)
  x <- post_process_modify_link_colors(x, ...)

  # Replace tildes (~) in the code so they pas through Pandoc to LaTeX
  x <- post_process_fix_tildes(x, ...)

  # Placements for figures and tables ----
  x <- post_process_set_latex_placement_options(x, ...)

  # Longtable customization ----
  x <- post_process_longtables(x, ...)

  # Landscape figures and table customization ----
  x <- post_process_landscape_figures(x, ...)
  x <- post_process_landscape_tables(x, ...)
  x <- post_process_fix_landscape_issues(x, ...)

  # Table caption alignment ----
  x <- post_process_move_table_captions(x, ...)

  # Figure/table lettering/numbering ----
  x <- post_process_add_counters(x, ...)

  # Add a little space before the "Stock" subsection header ----
  # as it is bumped up really close to the "Executive Summary" section header
  x <- post_process_add_vert_space_after_header(x, ...)

  # Add phantomsection tag to sections that are unnumbered to force the
  # bookmark links to work right for them
  x <- post_process_repair_unnumbered_section_links(x, ...)

  # Add horizontal lines to the decision table headers ----
  # across multiple columns
  x <- post_process_add_horiz_lines_decision_table(x, ...)

  # Tag the figures in the PDF and add alternative text ----
  x <- post_process_add_alt_text(x, ...)

  x
}