#' Creates a LaTeX font size string which can be used in
#' table header cells to make [kableExtra::kbl()] tables have headers with
#' a different font size than the table cells have
#'
#' @param header_font_size Size of font, in pt
#' @param header_vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Absorbs arguments meant for other functions
#'
#' @return A list of two character strings, one having double-backslashes
#' before the `fontsize` and `selectfont` commands and the other having 4
#' backslashes
#' @export
create_fontsize_str <- function(header_font_size = 10,
                                header_vert_spacing = NULL,
                                header_vert_scale = 1.2,
                                ...){

  out <- list()
  if(is.null(header_vert_spacing)){
    header_vert_spacing <- ceiling(header_font_size * header_vert_scale)
  }
  out$quad <- paste0(" \\\\fontsize{",
                     header_font_size,
                     "}{",
                     header_vert_spacing,
                     "}\\\\selectfont ")

  out$dbl <- paste0(" \\fontsize{",
                    header_font_size,
                    "}{",
                    header_vert_spacing,
                    "}\\selectfont ")

  out
}