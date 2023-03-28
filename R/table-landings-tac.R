#' Create a summary table of landings and TACS
#'
#' @param ct Data frame as in the file `landing-tac-histroy.csv`
#' @param start_yr Start year for the table
#' @param end_yr End year for the table. If past data range, the last year
#' in `d` will be used
#' @param font_size The table data and header font size in points
#' @param ... Arguments passed to [knitr::kable()]

#'
#' @return A [knitr::kable()] object
#' @export
table_landings_tac <- function(ct,
                               start_yr,
                               end_yr,
                               font_size = 8,
                               ...){


  yrs <- ct$Year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  df <- ct |>
    select(Year,
           `U.S. Total`,
           `Canada Total`,
           Total,
           us_prop,
           can_prop,
           `U.S. TAC`,
           `Canada TAC`,
           `Total TAC`,
           us_attain,
           can_attain,
           tot_attain)

  df <- df |>
    filter(Year %in% yrs) |>
    mutate(Year = as.character(Year))

  df <- df |>
    mutate_at(.vars = vars(us_prop,
                           can_prop,
                           us_attain,
                           can_attain,
                           tot_attain), ~{ifelse(is.na(.x),
                                                 "--",
                                                 paste0(f(.x, 1), "\\%"))}) |>
    mutate_at(.vars = vars(-c(Year,
                              us_prop,
                              can_prop,
                              us_attain,
                              can_attain,
                              tot_attain)), ~{ifelse(is.na(.x),
                                                     "--",
                                                     f(.x, 0))})

  col_names <- c("Year",
                 "U.S.\nlandings",
                 "Canada\nlandings",
                 "Total\nlandings",
                 "U.S.\nprop.\nof total\ncatch",
                 "Canada\nprop.\nof total\ncatch",
                 "U.S.\ncatch\ntarget",
                 "Canada\ncatch\ntarget",
                 "Total\ncatch\ntarget",
                 "U.S.\nprop.\nof catch\ntarget\nremoved",
                 "Canada\nprop.\nof catch\ntarget\nremoved",
                 "Total\nprop.\nof catch\ntarget\nremoved")

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
