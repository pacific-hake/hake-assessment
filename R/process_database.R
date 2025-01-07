#' Process the raw data from [usa_pull_data()]
#'
#' This function runs through multiple `process_*` functions in a specified
#' order to move the raw data into `data-tables/*.csv` files. See the See Also
#' section for more details.
#' @export
#' @seealso
#' First the catch is processed using
#' * [process_catch_norpac()]
#' * [process_catch_pacfin()]
#' Then the age data are processed using
#' * [process_age_sea()]
#' * [process_age_shore()]
#' * [plot_raw_ages()]
#' Then the weight-at-age data are processed using
#' * [process_weight_at_age_survey()]
#' * [process_weight_at_age_us()]
#' @family process
process_database <- function() {
  # Catch
  processed_catch_norpac <- process_catch_norpac()
  process_catch_pacfin()

  # Age composition
  age_norpac <- process_age_sea(ncatch = processed_catch_norpac)
  age_shore <- process_age_shore()
  plot_raw_age()
}
