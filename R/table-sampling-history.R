#' Title
#'
#' @param d Data frame as in the file `fishery-sampling-histroy.csv`
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param font_size The table data and header font size in points
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return
#' @export
#'
#' @examples
table_sampling_history <- function(d,
                                   start_yr,
                                   end_yr,
                                   font_size = 10,
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

  col_names <- gsub(" ", "\n", names(d))
  col_names <- gsub("-", "-\n", col_names)
  col_names <- gsub("Shoreside", "Shore-\nside", col_names)
  col_names <- gsub("Mothership", "Mother-\nship", col_names)
  # Add \\makecell{} latex macro to headers with newlines
  col_names <- linebreak(col_names, align = "c")
  # Center header names which don't have newlines, they are ignored by
  # linebreak(), so add \\makecell manually
  col_names[col_names == "Year"] <- "\\makecell[c]{Year}"

  kable(d,
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
