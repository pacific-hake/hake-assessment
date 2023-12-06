#' Creates a table of the sampling history for hake
#'
#' @param d Data frame as found in [sampling_history_df]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_sampling_history <- function(d,
                                   start_yr,
                                   end_yr,
                                   font_size = 10,
                                   header_font_size = 10,
                                   header_vert_spacing = 12,
                                   header_vert_scale = 1.2,
                                   ...){

  yrs <- d$Year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  d <- d |>
    filter(Year %in% yrs) |>
    mutate_all(~{as.numeric(.x)}) |>
    mutate_at(.vars = vars(-Year), ~{
      ifelse(is.na(.x),
             "--",
             .x)})

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)
  # Insert header font size after every newline
  col_names <- gsub(" ", "\n", names(d))
  col_names <- gsub("-", "-\n", col_names)
  col_names <- gsub("Shoreside", "Shore-\nside", col_names)
  col_names <- gsub("Mothership", "Mother-\nship", col_names)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  kbl(d,
      format = "latex",
      booktabs = TRUE,
      align = "r",
      linesep = "",
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))

}
