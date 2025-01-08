#' Extract the inside (Strait of Georgia) hake catch by year
#'
#' @param ... Arguments passed to [canada_load_catch_data()]
#'
#' @return A list of three data frames, one for the landed, one for the
#' discarded, and one for the sum of the two
#'
#' @export
extract_inside_catch <- function(...){

  j <- canada_load_catch_data(...)
  landed_df <- j$dmp_inside_df |>
    mutate(year = gsub("[a-zA-Z]+-[0-9]+-([0-9]{4})", "\\1", hail_out_date)) |>
    select(year, raw_wght_lbs_) |>
    transmute(year = year,
              wt = raw_wght_lbs_ * 0.453592) |>
    group_by(year) |>
    summarise(wt = sum(wt))

  disc_df <- j$logs_inside_df |>
    mutate(year = gsub("[a-zA-Z]+ +[0-9]+ +([0-9]{4})", "\\1", hail_out_date)) |>
    dplyr::filter(!is.na(released_wt)) |>
    dplyr::filter(released_wt > 0) |>
    select(year, released_wt) |>
    rename(wt = released_wt) |>
    group_by(year) |>
    summarise(wt = sum(wt))

  tot_df <- landed_df |>
    full_join(disc_df, by = "year") |>
    mutate(wt.y = ifelse(is.na(wt.y), 0, wt.y)) |>
    mutate(wt = wt.x + wt.y) |>
    select(-wt.x, -wt.y) |>
    mutate(wt = wt / 1e6)

  list(landed_df = landed_df,
       disc_df = disc_df,
       tot_df = tot_df)
}
