#' Create a table containing the maturiy ogive data as input into the
#' assessment model
#'
#' @param d A data frame containing the maturity ogive information  as read
#' in from the file `maturity-table.csv`
#' @param digits Number of decimal places
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
table_maturity_ogive <- function(d,
                                 digits = 3,
                                 font_size = 10,
                                 header_font_size = 10,
                                 header_vert_spacing = 12,
                                 header_vert_scale = 1.2,
                                 ...){

  d <- d |>
    select(age,
           n_ovaries,
           maturity,
           avg_wt,
           new_fecundity) |>
    mutate_at(vars(-c(age, n_ovaries)), ~{f(.x, digits)})

  col_names <- c("Age",
                 "Number of\nsamples",
                 "Maturity\nogive",
                 "Mean\nweight",
                 "Mean\nfecundity")

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

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
