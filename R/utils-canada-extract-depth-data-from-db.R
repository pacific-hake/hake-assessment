#' Extract the Canadian depth and fishing event data
#'
#' @description
#' Extract the Canadian depth and fishing event data from the `GFBIOSQL`
#' database. The data frame returned will contain the `fishing_event_id`
#' column along  with six depth columns.
#'
#' @param dr The directory to save the depth RDS file in
#' @param ret_df Logical. If `TRUE`, return the data frame from the function
#' and do not write a csv file. If `FALSE`, write the csv file and return
#' nothing
#' @param overwrite Logical. Overwrite the RDS file for if it exists
#'
#' @return If `ret_df` is `TRUE`, return the depth data frame. If `ret_df`
#' is `FALSE`, return nothing, write an RDS file instead
#' @export
canada_extract_depth_data_from_db <- function(
    fn = file.path(can_sample_dr, can_depths_rds_fn),
    ret_df = FALSE,
    overwrite = FALSE){

  msg_start <- "Extracting the depth data from the database, please wait ..."
  msg_end <- "Done extracting the depth data from the database.\n"

  if((!overwrite && file.exists(fn)) && !ret_df){
    message("The database query was not run because the file `", fn, "` ",
            "exists and `overwrite` is `FALSE`")
    return(invisible())
  }

  message(msg_start)
  sql_fn <- here(sql_path, canada_depth_sql_fn)
  if(!file.exists(sql_fn)){
    stop("File `", sql_fn, "` does not exist")
  }
  sql <- readLines(sql_fn)

  fleets <- c("ft", "ss", "jv")

  d <- map_dfr(fleets, \(fleet){
    sql <- canada_sql_inject_fishery_filters(sql, type = fleet)
    run_sql("GFFOS", sql) |>
      as_tibble() |>
      mutate(fleet = fleet)
  })

  message(msg_end)

  if(ret_df){
    return(d)
  }

  saveRDS(d, fn)
  message("Finished writing file:\n`", fn, "`\n")

  d
}