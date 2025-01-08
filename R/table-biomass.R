#' Create a table of spawning biomass and relative spawning biomass
#' medians and quantiles
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal places to round to
#' @param scale A scale factor to divide the biomass by
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
table_biomass <- function(model,
                          start_yr,
                          end_yr,
                          digits = 1,
                          scale = 1000,
                          font_size = 8,
                          header_font_size = 10,
                          header_vert_spacing = 12,
                          header_vert_scale = 1.2,
                          ...){

  calcs <- model$mcmccalcs

  # Biomass quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$slower))
  slower <- calcs$slower[inds] * scale
  smed <- calcs$smed[inds] * scale
  supper <- calcs$supper[inds] * scale

  # Depletion quantiles
  dlower <- calcs$dlower[inds] * 100
  dmed <- calcs$dmed[inds] * 100
  dupper <- calcs$dupper[inds] * 100

  yrs <- as.numeric(names(slower))
  d <- tibble(yrs,
              f(slower, digits),
              f(smed, digits),
              f(supper, digits),
              paste0(f(dlower, digits), "\\%"),
              paste0(f(dmed, digits), "\\%"),
              paste0(f(dupper, digits), "\\%")) |>
    dplyr::filter(yrs %in% start_yr:end_yr)

  names(d) <- c("Year",
                "SB \n2.5\\textsuperscript{th}\npercentile",
                "SB \nMedian",
                "SB \n97.5\\textsuperscript{th}\npercentile",
                "Rel. SB \n2.5\\textsuperscript{th}\npercentile",
                "Rel. SB \nMedian",
                "Rel. SB \n97.5\\textsuperscript{th}\npercentile")

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- names(d)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreak(col_names, align = "c")

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
