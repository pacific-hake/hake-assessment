#' Create a table of spawning biomass and relative spawning biomass
#' medians and quantiles
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal places to round to
#' @param scale A scale factor to divide the biomass by
#' @param font_size The table data and header font size in points
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_biomass <- function(model,
                          start_yr,
                          end_yr,
                          digits = 1,
                          scale = 1000,
                          font_size = 8,
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
  df <- tibble(yrs,
               f(slower, digits),
               f(smed, digits),
               f(supper, digits),
               paste0(f(dlower, digits), "\\%"),
               paste0(f(dmed, digits), "\\%"),
               paste0(f(dupper, digits), "\\%")) |>
    filter(yrs %in% start_yr:end_yr)

  names(df) <- c("Year",
                 "SSB ($B_t$)\n2.5\\textsuperscript{th}\npercentile",
                 "SSB ($B_t$)\nMedian",
                 "SSB ($B_t$)\n97.5\\textsuperscript{th}\npercentile",
                 "Rel.\nSSB ($B_t/B_0$)\n2.5\\textsuperscript{th}\npercentile",
                 "Rel.\nSSB ($B_t/B_0$)\nMedian",
                 "Rel.\nSSB ($B_t/B_0$)\n97.5\\textsuperscript{th}\npercentile")

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
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
