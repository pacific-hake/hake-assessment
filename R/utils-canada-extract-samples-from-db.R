#' Extract the Canadian biological sample data
#'
#' @description
#' Extract the Canadian biological sample data from the `GFBIOSQL` database
#' The data frame returned will be raw and unfiltered
#'
#' @param dr The directory to save the samples RDS file in
#' @param ret_df Logical. If `TRUE`, return the data frame from the function
#' and do not write a csv file. If `FALSE`, write the csv file and return
#' nothing
#' @param overwrite Logical. Overwrite the RDS file for sample data if
#' it exists
#' @param unsorted_only Only include unsorted samples. For hake in Canada,
#' this needs to be `FALSE` or some years will show no ages at all.
#' See [gfdata::get_commercial_samples()] for additional information
#' @param species_code The DFO Groundfish database species code;
#' default: 225 (hake)
#'
#' @return If `ret_df` is `TRUE`, return the sample data frame. If `ret_df`
#' is `FALSE`, return nothing, write an RDS file instead
#' @export
canada_extract_samples_from_db <- function(dr = "/srv/hake/other/samples",
                                           ret_df = FALSE,
                                           overwrite = FALSE,
                                           unsorted_only = FALSE,
                                           species_code = 225){

  msg_start <- "Extracting the sample data from the database, please wait ..."
  msg_end <- "Done extracting the sample data from the database.\n"

  if(ret_df){
    message(msg_start)
    d <- get_commercial_samples(species_code,
                                unsorted_only = unsorted_only)
    message(msg_end)
    return(d)
  }

  if(!dir.exists(dr)){
    stop("The directory `", dr, "` does not exist")
  }

  fn <- file.path(dr, can_sample_data_rds_fn)

  if(overwrite || !file.exists(fn)){
    message(msg_start)
    d <- get_commercial_samples(225,
                                unsorted_only = unsorted_only)
    message(msg_end)
    message("Writing samples data frame as RDS file:\n`", fn, "`\n")
    saveRDS(d, fn)
    message("Finished writing file:\n`", fn, "`\n")
  }else{
    message("The database query was not run because the file `", fn, "` ",
            "exists and `overwrite` is `FALSE`")
  }
}
