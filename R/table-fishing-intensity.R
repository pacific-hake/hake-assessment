#' Create a table of fishing intensity and exploitation fraction medians
#' and quantiles
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal places to round recruitment values to
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_fishing_intensity <- function(model,
                                    start_yr,
                                    end_yr,
                                    digits = 1,
                                    font_size = 8,
                                    header_font_size = 10,
                                    header_vert_spacing = 12,
                                    header_vert_scale = 1.2,
                                    ...){

  calcs <- model$mcmccalcs

  # Relative fishing intensity quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$plower))
  plower <- calcs$plower[inds]
  pmed <- calcs$pmed[inds]
  pupper <- calcs$pupper[inds]

  # Exploitation fraction quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$flower))
  flower <- calcs$flower[inds]
  fmed <- calcs$fmed[inds]
  fupper <- calcs$fupper[inds]

  yrs <- as.numeric(names(flower))
  df <- tibble(yrs,
               f(plower, digits),
               f(pmed, digits),
               f(pupper, digits),
               f(flower, digits),
               f(fmed, digits),
               f(fupper, digits)) |>
    dplyr::filter(yrs %in% start_yr:end_yr)

  names(df) <- c("Year",
                 "Rel.\nFishing\nIntensity\n2.5\\textsuperscript{th}\npercentile",
                 "Rel.\nFishing\nIntensity\nMedian",
                 "Rel.\nFishing\nIntensity\n97.5\\textsuperscript{th}\npercentile",
                 "Exploit.\nFraction\n2.5\\textsuperscript{th}\npercentile",
                 "Exploit.\nFraction\nMedian",
                 "Exploit.\nFraction\n97.5\\textsuperscript{th}\npercentile")

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- names(df)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreak(col_names, align = "c")

  kbl(df,
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
