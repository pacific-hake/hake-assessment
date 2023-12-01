#' Fetch the sample data from the GFBIOSQL database
#' The data will be filtered to only include hake major and minor areas
#'
#' @param dr The directory to save the samples RDS file in
#' @param overwrite Logical. Overwrite the RDS file for sample data if
#' it exists
#' @param species_code The species code from the GFBio database, or
#' species name if you're unsure
#'
#' @export
canada_extract_sample_data <- function(dr = "/srv/hake/other/samples",
                                       overwrite = FALSE,
                                       species_code = 225){

  if(!dir.exists(dr)){
    stop("The directory `", dr, "` does not exist")
  }

  fn <- file.path(d, can_sample_data_rds_fn)

  if(overwrite || !file.exists(fn)){
    # Include BC offshore and St. of Juan de Fuca (Major 1, Minor 20)
    d <- get_commercial_samples(species_code) |>
      filter(major_stat_area_code %in% can_major_hake_areas |
               (major_stat_area_code == "01" & minor_stat_area_code == "20"))

    saveRDS(d, fn)
    message("The file:\n`", fn, "`\nwas written with new sample data\n")
  }else{
    message("The database query was not run because the file `", fn, "` ",
            "exists and `overwrite` is `FALSE`")
  }
}
