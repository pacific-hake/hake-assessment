#' Create CSV files with catch by year and month for Canadian fleets
#'
#' @param lst A list of data frames as returned by
#' [canada_extract_fleet_catch()]
#' @param catch_scale A value to divide the catch by prior to writing to
#' the file
#'
#' @returns Nothing, creates three CSV files
#' @export
canada_create_fleet_catch_csv_files <- function(lst,
                                                catch_scale = 1000){

  if(length(lst) != 3){
    stop("The length of the input list `lst` does not equal 3")
  }
  if(!all(names(lst) %in% c("ft", "ss", "jv"))){
    stop("The names of the elements in the list `lst` are not correct. ",
         "They must be `ft`, `ss`, and `jv`")
  }

  fns <- here(data_tables_path, c(can_ft_catch_by_month_fn,
                                  can_ss_catch_by_month_fn,
                                  can_jv_catch_by_month_fn))
  if(!all(file.exists(fns))){
    stop("One or more of the csv data files does not exist. Files are:\n",
         paste(fns, collapse = "\n"))
  }

  walk2(lst,
        fns,
        \(df, fn){
          browser()
          d <- df |>
            group_by(year, month) |>
            summarize(catch = sum(landings) / catch_scale) |>
            ungroup() |>
            pivot_wider()
            dcast(year ~ month )
          d[is.na(d)] <- 0
          browser()
          write_csv(d, fn)
        })
}
