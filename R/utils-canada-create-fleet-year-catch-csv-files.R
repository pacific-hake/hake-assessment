#' Calculate the catch by year and fishery for Canadian
#' fisheries
#'
#' @param lst A list as returned by [canada_extract_fleet_catch()]
#' @param catch_scale A value to divide the catch by prior to writing to
#' the file
#' @param write_file If `TRUE`, write the output to the data file given by
#' the package data variable `can_catch_by_year_fn`. If `FALSE`, return the
#' data frame
#' @param digits The number of decimal places to include in the table output
#'
#' @return If `write_file` is `TRUE`, nothing. If `write_file` is `FALSE`,
#' a data frame containing the catch by year (rows) and fishery type (columns)
#' @export
canada_create_fleet_year_catch_csv_files <- function(lst,
                                                     catch_scale = 1000,
                                                     write_file = TRUE,
                                                     digits = 2){

  if(length(lst) != 3){
    stop("The length of the input list `lst` does not equal 3")
  }
  if(!all(names(lst) %in% c("ft", "ss", "jv"))){
    stop("The names of the elements in the list `lst` are not correct. ",
         "They must be `ft`, `ss`, and `jv`")
  }

  ct_lst <- map(lst, ~{
    .x |>
      group_by(year) |>
      summarize(landings = sum(landings)) |>
      ungroup()
  })

  ct <- ct_lst$ft |>
    full_join(ct_lst$ss, "year") |>
    full_join(ct_lst$jv, "year") |>
    setNames(c("year", "ft_weight", "ss_weight", "jv_weight"))
  ct[is.na(ct)] <- 0
  ct <- ct |>
    mutate(total_weight = ft_weight + ss_weight +jv_weight) |>
    mutate(across(-year, ~{round(.x / catch_scale, digits)}))

  if(!write_file){
    return(ct)
  }

  fn <- here(data_tables_path, can_catch_by_year_fn)
  write_csv(ct, fn)
  message("The file:\n`", fn, "`\nwas written with new catch data\n")
}