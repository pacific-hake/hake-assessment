#' Extract weight-at-age vector from SS3 input data
#'
#' @param model A model, created by [create_rds_file()]
#' @param yrs A vector of years to use in the calculation
#' @param yrs_mean If `TRUE`, calculate the at-age means from the `yrs`
#' weight-at-age values
#' @param flt The fleet number 1 = fishery, 2 = survey
#'
#' @returns A numeric named vector
#' @export
extract_weight_at_age_vector_from_ss_inputs <- \(model,
                                          yrs = 2020:2024,
                                          yrs_mean = TRUE,
                                          flt = 1){

  waa <- model$wtatage |>
    as_tibble() |>
    dplyr::filter(year %in% 2020:2024) |>
    dplyr::filter(fleet == flt) |>
    select(!seas & !sex & !bio_pattern & !birthseas & !fleet)  |>
    pivot_longer(cols = !year, names_to = "age") |>
    select(!year) |>
    split(~ age) |>
    map_dbl(~{mean(.x$value)})

  # They are now ordered by age characters, so we have to re-order numerically by age
  waa <- waa[order(as.numeric(names(waa)))]

  waa
}
