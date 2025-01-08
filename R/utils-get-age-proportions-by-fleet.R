#' Calculates the age proportions by year and returns a list with one
#' data frame for each fishery type (Freezer trawlers, Shoreside, and
#' Joint venture)
#'
#' @param d A data frame as returned by [gfdata::get_commercial_samples()]
#' @param raw_proportions Logical. If `TRUE`, return raw, unweighted age
#' proportions. If `FALSE`, return the age proportions weighted by sample
#' and catch weights
#'
#' @return A list of three data frames of age proportions by year
#'
#' @export
get_age_proportions_by_fleet <- function(d, raw_proportions = F){

  sectors <- c("ft", "ss", "jv")
  map(sectors, \(sector){

          switch(sector,
                 "ft" = {
                   df <- d |>
                     dplyr::filter(trip_sub_type_desc %in%
                              c("OBSERVED DOMESTIC",
                                "NON - OBSERVED DOMESTIC")) |>
                     dplyr::filter(vessel_id %in% freezer_trawlers$gfbio_id)
                 },
                 "ss" = {
                   df <- d |>
                     dplyr::filter(trip_sub_type_desc %in%
                             c("OBSERVED DOMESTIC",
                               "NON - OBSERVED DOMESTIC")) |>
                     dplyr::filter(!vessel_id %in% freezer_trawlers$gfbio_id)
                 },
                 "jv" = {
                   df <- d |>
                     dplyr::filter(trip_sub_type_desc == "OBSERVED J-V")
                 })
          df |>
            get_age_proportions(raw_proportions = raw_proportions)
        }) |>
    setNames(sectors)
}