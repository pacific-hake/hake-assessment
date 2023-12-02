#' Split up the Canadian samples data frame into fleet data frames
#'
#' @description
#' Split up the samples data frame into three data frames, one for Freezer
#' trawlers, one for Shoreside, and one for the Joint venture fishery
#'
#' @details
#' The following filtering is performed in this function for fleets:
#' 1) Freezer trawlers - Observed and Non-observed domestic trips, vessel
#'    is in the package data variable [freezer_trawlers]
#' 2) Shoreside - Observed and Non-observed domestic trips, vessel is not
#'    in the package data variable [freezer_trawlers]
#' 3) Joint venture - Observed J-V trips
#'
#' @param d A data frame as returned by [gfdata::get_commercial_samples()],
#' or the wrapper [canada_load_sample_data()]
#'
#' @return A list of three named data frames, one for each of the three
#' Canadian fisheries `ft`, `ss`, and `jv`
#'
#' @export
canada_get_fleet_samples <- function(d){

  fleets <- c("ft", "ss", "jv")

  map(fleets, \(fleet){

    switch(fleet,
           "ft" = {
             df <- d |>
               filter(trip_sub_type_desc %in%
                        c("OBSERVED DOMESTIC",
                          "NON - OBSERVED DOMESTIC")) |>
               filter(vessel_id %in% freezer_trawlers$gfbio_id)
           },
           "ss" = {
             df <- d |>
               filter(trip_sub_type_desc %in%
                        c("OBSERVED DOMESTIC",
                          "NON - OBSERVED DOMESTIC")) |>
               filter(!vessel_id %in% freezer_trawlers$gfbio_id)
           },
           "jv" = {
             df <- d |>
               filter(trip_sub_type_desc == "OBSERVED J-V")
           })
    df
  }) |>
    setNames(fleets)
}