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
#' or the wrapper [canada_load_sample_data()], or from
#' [canada_extract_depth_data_from_db()]
#' @param db_type The type of database that `d` came from. This is needed
#' to determine which Freezer trawler vessel IDs to use, `GFbioSQL` or `GFFOS`
#'
#' @return A list of three named data frames, one for each of the three
#' Canadian fisheries `ft`, `ss`, and `jv`
#'
#' @export
canada_get_fleet_samples <- function(d,
                                     db_type = c("gfbio",
                                                 "gffos")){

  fleets <- c("ft", "ss", "jv")

  db_type <- match.arg(db_type)
  if(db_type == "gfbio"){
    ft_ids <- freezer_trawlers$gfbio_id
  }else{
    ft_ids <- freezer_trawlers$fos_id
  }

  map(fleets, \(fleet){

    switch(fleet,
           "ft" = {
             df <- d
             if(db_type == "gfbio"){
               df <- df |>
                 dplyr::filter(trip_sub_type_desc %in%
                          c("OBSERVED DOMESTIC",
                            "NON - OBSERVED DOMESTIC"))
             }
             df <- df |>
               dplyr::filter(vessel_id %in% ft_ids)
           },
           "ss" = {
             df <- d
             if(db_type == "gfbio"){
               df <- df |>
                 dplyr::filter(trip_sub_type_desc %in%
                          c("OBSERVED DOMESTIC",
                            "NON - OBSERVED DOMESTIC"))
             }
             df <- df |>
               dplyr::filter(!vessel_id %in% ft_ids)
           },
           "jv" = {
             if(db_type == "gfbio"){
               df <- d |>
                 dplyr::filter(trip_sub_type_desc == "OBSERVED J-V")
             }else{
               # No FOS JV records will be returned, this includes depth
               # records
               df <- NULL
             }
           })
    df
  }) |>
    setNames(fleets)
}