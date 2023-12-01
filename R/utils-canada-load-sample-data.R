#' Load the commercial sample data from a file generated
#' by [canada_extract_sample_data()]
#'
#' @details
#' Filtering is performed in this function for
#' 1) Areas - BC offshore areas + Strait of Juan de Fuca
#' 2) Fishing gear - Midwater trawl only
#'
#' @param dr The directory in which the sample RDS file resides
#'
#' @return Nothing, writes an RDS file
#' @export
canada_load_sample_data <- function(dr = "/srv/hake/other/samples"){

  if(!dir.exists(dr)){
    stop("The directory `", dr, "` does not exist")
  }

  fn <- file.path(dr, can_sample_data_rds_fn)

  if(file.exists(fn)){
    readRDS(fn) |>
      filter(major_stat_area_code %in% can_major_hake_areas |
               (major_stat_area_code == "01" & minor_stat_area_code == "20")) |>
      filter(gear_desc == "MIDWATER TRAWL")
  }else{
    stop("The file:\n`", fn, "`\ndoes not exist. Run ",
         "`canada_extract_sample_data()` to create it")
  }
}
