#' Update maturity in an SS3 weight-at-age matrix
#'
#' @details
#' Update the maturity information (i.e., `fleet == -2`) in a matrix of
#' weight-at-age data read in using [r4ss::SS_readwtatage()]. Information for
#' the fleets other than fleet -2 will not be altered. The information from
#' fleet 0 (i.e., beginning of the season population weight-at-age) is used to
#' create the fecundity information. This can be changed by altering the
#' default value for the `weight_at_age_fleet` argument.
#'
#' The input data frame for `maturity` need not contain all years available
#' in `weight_at_age`. The last year available will be the last year before
#' a five-year average is taken for the forecast years. The remaining missing
#' years will be filled in using a global average of the available data.
#' Typically, all fleets other than fleet -2 have the same information but in
#' case they do not, then the population information seems to be the best to
#' create fecundity.
#'
#' @param maturity A data frame of maturity at age. The data frame must
#'   have at least three columns, `age`, `year`, and `p_mature` but it can
#'   also have `model` though this column will be removed as well as any other
#'   column not included in the needed three. These column names align with
#'   what is present in `maturity_estimates_df`, a data frame of maturity
#'   estimates by model, but information from only one model can be passed in
#'   a function call. See the example.
#' @param weight_at_age A data frame of weight-at-age values as returned from
#'   [r4ss::SS_readwtatage()] or `r4ss::SS_read()[["wtatage"]]`.
#' @param ages An integer vector specifying which ages must be present. All
#'   missing combinations of ages and year will be filled in with a probability
#'   of being mature of 0.0. This is helpful when there are no values for age-0
#'   fish in the data frame for a given year.
#' @param weight_at_age_fleet An integer specifying which fleet you want to
#'   take the weight-at-age information from. The default is 0, which is the
#'   beginning of the year population-level weight-at-age data.
#'
#' @return
#' A data frame with the same format as the input data frame for `weight_at_age`
#' is returned, and thus, the returned object can be passed to
#' [r4ss::SS_writewtatage()].
#' @author Kelli F. Johnson
#' @examples
#' \dontrun{
#' # The following example does not actually work because the path to the
#' # weight-at-age file does not exist.
#' update_ss3_maturity(
#'   maturity = maturity_estimates_df |>
#'     dplyr::filter(model == "Null"),
#'   weight_at_age = r4ss::SS_readwtatage("wtatage.ss")
#' )
#' }
update_ss3_maturity <- function(maturity,
                                weight_at_age,
                                ages = 0:15,
                                weight_at_age_fleet = 0) {
  # Determine the number of forecast years needed to extend the time series in
  # `maturity` to match the number of years available in `weight_at_age`
  good_years <- weight_at_age |>
    dplyr::pull(year) |>
    unique()
  needed_forecast <- sum(
    !good_years[good_years > min(maturity[["year"]])] %in%
    maturity[["year"]]
  )

  # Check if `model` exists, and ensure no more than 1 is present
  if ("model" %in% colnames(maturity)) {
    stopifnot(length(unique(maturity[["model"]])) == 1)
  }

  # Ensure maturity has all necessary ages and then extend it backwards using
  # global average and forwards using most recent five year average into a
  # matrix with one column per age
  maturity_formatted <- maturity |>
    dplyr::select(age, year, p_mature) |>
    tidyr::complete(age = ages, year, fill = list(p_mature = 0)) |>
    dplyr::arrange() |>
    dplyr::rename(pred_weight = p_mature) |>
    pad_weight_at_age(n_forecast_years = needed_forecast) |>
    tidyr::complete(
      yr = weight_at_age |>
        dplyr::filter(fleet == weight_at_age_fleet) |>
        dplyr::pull(year)
    ) |>
    tidyr::fill(-yr) |>
    dplyr::select(dplyr::matches("^[0-9]"))

  # Multiply weight_age_age by maturity to get fecundity
  fecundity_tv <- weight_at_age |>
    dplyr::filter(fleet == weight_at_age_fleet) |>
    dplyr::select(dplyr::matches("^[0-9]")) *
    maturity_formatted

  # Insert the fecundity values into weight_at_age as fleet -2
  # and return the weight-at-age data frame
  weight_at_age[
    # Select fleet -2, which is weight * maturity = fecundity
    weight_at_age[["fleet"]] == -2,
    # Select just the 0, 1, 2, ..., 20 columns
    grepl("[0-9]", colnames(weight_at_age))
  ] <- fecundity_tv
  return(weight_at_age)
}
