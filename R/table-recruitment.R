#' Create a table of absolute recruitment and relative spawning biomass
#' medians and quantiles
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal places to round recruitment values to
#' @param digits_dev Number of decimal places to round recruitment deviation
#' values to
#' @param scale A scale factor to divide the recruitment by
#' @param font_size The table data and header font size in points
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_recruitment <- function(model,
                              start_yr,
                              end_yr,
                              digits = 1,
                              digits_dev = 3,
                              scale = 1000,
                              font_size = 8,
                              ...){

  calcs <- model$mcmccalcs

  # Recruitment quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$rlower))
  rlower <- calcs$rlower[inds] * scale
  rmed <- calcs$rmed[inds] * scale
  rupper <- calcs$rupper[inds] * scale

  # Recruitment deviation quantiles
  inds <- grep("^20[0-9]{2}$", names(calcs$devlower))
  devlower <- calcs$devlower[inds]
  devmed <- calcs$devmed[inds]
  devupper <- calcs$devupper[inds]

  yrs <- as.numeric(names(rlower))
  df <- tibble(yrs,
               f(rlower, digits),
               f(rmed, digits),
               f(rupper, digits),
               f(devlower, digits_dev),
               f(devmed, digits_dev),
               f(devupper, digits_dev)) |>
    filter(yrs %in% start_yr:end_yr)

  names(df) <- c("Year",
                 "Recruit-\nment\n2.5\\textsuperscript{th}\npercentile",
                 "Recruit-\nment\nMedian",
                 "Recruit-\nment\n97.5\\textsuperscript{th}\npercentile",
                 "Rec.\nDeviations\n2.5\\textsuperscript{th}\npercentile",
                 "Rec.\nDeviations\nMedian",
                 "Rec.\nDeviations\n97.5\\textsuperscript{th}\npercentile")

  col_names <- names(df)
  col_names <- linebreak(col_names, align = "c")
  col_names[col_names == "Year"] <- "\\makecell[c]{Year}"

  kable(df,
        format = "latex",
        booktabs = TRUE,
        align = "r",
        linesep = "",
        col.names = col_names,
        escape = FALSE,
        ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size)
}
