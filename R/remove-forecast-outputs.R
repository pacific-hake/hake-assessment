#' Remove the `outputs` list item from the `catch_levels` lists in each year
#' in the `forecasts` list
#'
#' @details
#' The `outputs` list item for each catch level/year combination is set to
#' `NULL` which effectively removes it from the list. This is done because this
#' data frame can be over 87 million data values (if there are 8,000
#' posteriors and 726 columns as there are for the hake model)
#'
#' @param forecasts A list of years of forecasts, each of which is a list of
#' catch levels, each of which is a list of various values
#'
#' @return The forecasts list, with the `outputs` list items removed
#' @export
remove_forecast_outputs <- function(forecasts){

  map(forecasts, \(fore_yr){
    map(fore_yr, \(catch_level){
      catch_level$outputs <- NULL
      catch_level
    })
  })
}