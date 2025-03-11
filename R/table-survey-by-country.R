#' Create a table of values of the age 2+ survey biomass estimates by country
#'
#' @param d A data frame containing the survey history as read in from
#' the file `survey-by-country.csv`
#' @param digits Number of decimal places
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ret_df Logical. If `TRUE`, return a data frame of the table, If
#' `FALSE`, return a [kableExtra::kbl()] object
#' @param ... Arguments passed to [kableExtra::kbl()]
#'
#' @return A [knitr::kable()] object
#' @export
table_survey_by_country <- function(d,
                                    digits = 3,
                                    font_size = 10,
                                    header_font_size = 10,
                                    header_vert_spacing = 12,
                                    header_vert_scale = 1.2,
                                    ret_df = FALSE,
                                    ...){

  d <- d |>
    mutate(total = total / 1e3,
           canada.total = canada.total / 1e3,
           canada.prop = canada.prop / 100,
           us.total = f(total - canada.total, digits),
           us.prop = 1 - canada.prop,
           us.cv = us.cv / 100,
           canada.cv = canada.cv / 100) |>
    select(year,
           us.total,
           us.cv,
           us.prop,
           canada.total,
           canada.cv,
           canada.prop) |>
    mutate_at(vars(-year), ~{f(as.numeric(.x), digits)})

  if(ret_df){
    return(d)
  }
  col_names <- c("Year",
                 "U.S.\nAge-2$+$\nbiomass\n(million t)",
                 "U.S.\nsampling\nCV\nage-2$+$",
                 "U.S.\nprop.\nof\nbiomass",
                 "Canada\nAge-2$+$\nbiomass\n(million t)",
                 "Canada\nsampling\nCV\nage-2$+$",
                 "Canada\nprop.\nof\nbiomass")

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
