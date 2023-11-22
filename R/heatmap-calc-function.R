#' Calculate values for a given range of years, for ages which are
#' numerically-named columns in the data frame
#' ([plot_weight_at_age_heatmap()])
#'
#' @param wa A weight-at-age data frame with a column `yr` for the years, and
#' numerically-named columns for ages
#' @param func A function to use on each age column to create a row of outputs
#' @param ... Absorb arguments meant for other functions
#'
#' @return A single row data frame containing columns for age,
#' represented as numbers. The number of age columns is the same as
#' the number in `wa`
heatmap_calc_function <- function(
    wa = NULL,
    func = mean,
    yr_col = "yr",
    ...){

  stopifnot(!is.null(wa))
  stopifnot(is.data.frame(wa))
  stopifnot(is.function(func))

  # Return a vector of the means-at-age of the requested years
  out_row <- wa |>
    select(-{{yr_col}}) |>
    apply(2, func)

  out_row

}