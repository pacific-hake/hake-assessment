#' Extract the number of samples by fleet and month for a given year
#'
#' @param d A data frame, as one of the list elements of the list output
#' by [canada_extract_sampling_by_month()]
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param right_cols_cm The number of centimeters wide to make all of the
#' rightmost columns (all the value columns)
#' @param ...
#'
#' @return A list of the same length as `fleet_lst` of data frames, explained
#' in details above
#'
#' @export
table_sampling_by_month <- function(d,
                                    font_size = 10,
                                    header_font_size = 10,
                                    header_vert_spacing = 12,
                                    header_vert_scale = 1.2,
                                    right_cols_cm = 1.5,
                                    ...){


  col_names <- c("Month",
                 "Number\nof ages",
                 "Number\nof lengths",
                 "Number\nof weights")

  # Insert custom header font size before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)
  col_names <- gsub("\\n",
                    paste0("\n", hdr_font_str$quad),
                    col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines to create
  # multi-line header cells
  col_names <- linebreaker(col_names, align = "c")

  # Put commas in the numbers if over 1,000
  d <- d |>
    mutate(across(-month, f))

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l",
                     rep(paste0("R{",
                                right_cols_cm,
                                "cm}"),
                         ncol(d) - 1)),
           linesep = "",
           col.names = col_names,
           escape = FALSE,
           ...) |>
    kable_styling(font_size = font_size) |>
    row_spec(c(0, nrow(d)), bold = TRUE) |>
    row_spec(nrow(d) - 1, hline_after = TRUE)



  k
}
