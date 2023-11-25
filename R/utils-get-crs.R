#' @rdname add_utm_columns
#' @export
get_crs <- function(dat, ll_names = c("lon", "lat")) {

  lon <- dat[[ll_names[1]]]
  lat <- dat[[ll_names[2]]]
  # https://gis.stackexchange.com/a/190209
  zones <- round((183 + lon) / 6, 0)
  one_zone <- length(unique(zones)) == 1L
  if(!one_zone) {
    warning("Multiple UTM zones detected.\n",
            "Proceeding with the most common value.\n",
            "You may wish to choose a different projection.")
  }
  check <- rev(sort(table(zones)))
  zone <- as.numeric(names(check)[[1]])

  lat_zones <- round((45 + lat) / 90, 0)
  if (length(unique(lat_zones)) > 1L) {
    warning("North and south latitudes detected.\n",
            "Proceeding with the most common value.\n",
            "You may wish to choose a different projection.")
    one_zone <- FALSE
  }
  check <- rev(sort(table(lat_zones)))
  lat_zone <- as.numeric(names(check)[[1]])

  crs_val <- 32700 - lat_zone * 100 + zone
  mess <- ifelse(one_zone, "Detected", "Proceeding with")
  message(mess, " UTM zone ", zone, ifelse(lat_zone == 1, "N", "S"),
          "; CRS = ", crs_val, ".")
  message("Visit ", paste0("https://epsg.io/", crs_val, " to verify."))

  crs_val
}