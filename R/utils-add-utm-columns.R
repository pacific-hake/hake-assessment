#' Add UTM coordinates to a data frame (Copied from [gfplot] package)
#'
#' Add UTM (Universal Transverse Mercator) coordinates to a data frame. This is
#' useful since geostatistical modeling should generally be performed in an
#' equal-distance projection. You can do this yourself separately with the
#' [sf::st_as_sf()], [sf::st_transform()], and [sf::st_coordinates()] functions
#' in the \pkg{sf} package.
#'
#' @param d Data frame that contains longitude and latitude columns.
#' @param ll_names Longitude and latitude column names. **Note the order.**
#' @param ll_crs Input CRS value for `ll_names`.
#' @param utm_names Output column names for the UTM columns.
#' @param utm_crs Output CRS value for the UTM zone
#' @param units UTM units.
#'
#' @details
#' **Note that longitudes west of the prime meridian should be encoded
#' as running from -180 to 0 degrees.**
#'
#' You may wish to work in km's rather than the standard UTM meters so that the
#' range parameter estimate is not too small, which can cause computational
#' issues. This depends on the the scale of your data.
#'
#' @return
#' A copy of the input data frame with new columns for UTM coordinates.
#' @export
#'
#' @examplesIf require("sf", quietly = TRUE)
#' d <- data.frame(lat = c(52.1, 53.4), lon = c(-130.0, -131.4))
#' get_crs(d, c("lon", "lat"))
#' add_utm_columns(d, c("lon", "lat"))
add_utm_columns <- function(d,
                            ll_names = c("lon", "lat"),
                            ll_crs = 4326,
                            utm_names = c("X", "Y"),
                            utm_crs = 32609,
                            units = c("km", "m")){

  units <- match.arg(units)

  if(length(ll_names) != 2){
    cli_abort("`ll_names` must be a vector of length 2")
  }
  if(!all(ll_names %in% names(d))){
    cli_abort("Both names given in `ll_names` must appear as columns in `d`")
  }

  if(length(utm_names) != 2){
    cli_abort("`utm_names` must be a vector of length 2")
  }
  if(any(utm_names %in% names(d))){
    cli_abort(c("`utm_names` were found in `names(d)`.",
                "Remove them or choose different `utm_names`.")
    )
  }
  if(grepl("lat", ll_names[1]) ||
     grepl("lon", ll_names[2])){
    cli_warn(paste0("Make sure you didn't reverse the longitude and ",
                    "latitude in `ll_names`."))
  }

  x <- st_as_sf(d, crs = ll_crs, coords = ll_names) |>
    st_transform(utm_crs) |>
    st_coordinates() |>
    as_tibble() |>
    setNames(utm_names)

  if(units == "km"){
    x <- x |>
      mutate(across(utm_names, ~{.x <- .x / 1e3}))
  }

  d |>
    bind_cols(x)
}
