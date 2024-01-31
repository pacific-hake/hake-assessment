#' Fill in necessary years of weight-at-age file
#'
#' @details
#' Note that this function can also be used to pad maturity information
#' such that it has the same format as the weight-at-age information allowing
#' for the multiplication of maturity by weight at age to get fecundity.
#'
#' @param data A data frame of weight-at-age data in long form where the
#'   weight-at-age information is stored in a column called `pred_weight`,
#'   the four-digit year is in a column called `year`, and integer ages are
#'   in a column called `age`.
#' @param n_forecast_years An integer specifying the number of forecast years
#'   you want to extend the time series by.
#' @param n_years_used_for_forecast An integer specifying the number of years
#'   of data that you want to average over for the forecast period.
#' @param year_global_average A four digit integer, typically `-1940`, that is
#'   used to delineate the equilibrium year, all years after this and prior to
#'   the first year of data will be filled in by the information stored in this
#'   row.
#' @param ages A vector of integers specifying the ages you want in your file.
#'   If there are no ages in the data to represent the older ages present in
#'   vector, then the information for the oldest age will be repeated for any
#'   given year.
#' @author Kelli F. Johnson
#' @return
#' A data frame of weight-at-age information is returned that can immediately
#' be passed to [r4ss::SS_writewtatage()].
pad_weight_at_age <- function(data,
                              n_forecast_years = 5,
                              n_years_used_for_forecast = 5,
                              year_global_average = -1940,
                              ages = 0:20) {
  data <- dplyr::mutate(
    data,
    pred_weight = ifelse(is.infinite(pred_weight), NA, pred_weight)
  )
  years_used_for_forecast <- max(data[["year"]]) -
    0:(n_years_used_for_forecast - 1)
  global_average <- data |>
    dplyr::group_by(age) |>
    dplyr::reframe(
      pred_weight = mean(pred_weight, na.rm = TRUE)
    ) |>
    dplyr::mutate(year = year_global_average) |>
    dplyr::ungroup()
  forecast_average <- data |>
    dplyr::filter(
      year %in% years_used_for_forecast
    ) |>
    dplyr::group_by(age) |>
    dplyr::reframe(
      pred_weight = mean(pred_weight, na.rm = TRUE)
    ) |>
    dplyr::mutate(year = max(data[["year"]]) + 1) |>
    dplyr::ungroup()

  temp <- dplyr::bind_rows(data, global_average, forecast_average) |>
    dplyr::ungroup() |>
    tidyr::complete(
      year = c(
        year_global_average,
        seq(min(data[["year"]]), max(data[["year"]])),
        max(data[["year"]]) + 1:n_forecast_years
      ),
      age = ages
    ) |>
    tidyr::pivot_wider(
      id_cols = year,
      names_from = age,
      values_from = pred_weight
    ) |>
    tidyr::fill(dplyr::matches("[0-9]+")) |>
    tidyr::pivot_longer(
      cols = -year,
      names_to = "age"
    ) |>
    dplyr::mutate(age = as.numeric(age)) |>
    dplyr::arrange(year, age) |>
    tidyr::fill(value) |>
    tidyr::pivot_wider(
      id_cols = year,
      names_from = age,
      values_from = value
    )

  finish <- temp |>
    dplyr::rename(yr = year) |>
    dplyr::mutate(
      Seas = 1,
      Sex = 1,
      Bio_Pattern = 1,
      BirthSeas = 1,
      fleet = 1,
      .after = "yr"
    ) |>
    as.data.frame()
  return(finish)
}
