#' Create an age proportion CSV data from the sample data
#'
#' @details
#' A CSV file will be written, and the data frame will also be returned
#' invisibly. The data frame will contain the year, number of fish by year
#' in the data, and the number of samples (hauls/trips) in the data.
#' Shoreside uses trips as a sampling unit and the other two use hauls.
#'
#' @param d A data frame as returned by [gfdata::get_commercial_samples()]
#' @param type The fleet type to create the file for. This is needed so
#' the function knows what the filename should be for each fleet
#' @param min_date Earliest date to include
#' @param raw_counts Logical. If `TRUE`, return raw, unweighted age
#' proportions. If `FALSE`, return the age proportions weighted by sample
#' and catch weights
#' @param plus_grp Age plus group for maximum grouping
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#' @param weight_scale A value to divide the weights by
#' @param by_month Logical. If `TRUE`, return a data frame with a
#' `month` column in addition to a `year` column
#' @param digits The number of decimal places to round the values to
#'
#' @return The age proportion data frame, invisibly
#' @export
canada_create_age_proportions_csv <- function(
    d,
    type = c("ft", "ss", "jv"),
    min_date = as.Date("1972-01-01"),
    raw_counts = FALSE,
    plus_grp = 15,
    lw_tol = 0.1,
    lw_maxiter = 1e3,
    weight_scale = 1e3,
    by_month = FALSE,
    digits = 5){

  type <- match.arg(type)

  temporal_grouping <- if(by_month) c("year", "month") else "year"

  if(!"trip_start_date" %in% names(d)){
    d <- d |>
      rename(trip_start_date = trip_end_date)
  }
  d <- d |>
    dplyr::filter(!is.na(age)) |>
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) |>
    mutate(trip_start_date = as.Date(trip_start_date)) |>
    dplyr::filter(trip_start_date >= min_date)

  if(by_month){
    d <- d |>
      mutate(month = month(trip_start_date)) |>
      select(year, month, sample_id, length,
             weight, age, sample_weight, catch_weight)
  }else{
    d <- d |>
      select(year, sample_id, length, weight,
             age, sample_weight, catch_weight)
  }
  if(raw_counts){

    out <- d |>
      dplyr::filter(!is.na(age)) |>
      dplyr::filter(age > 0) |>
      group_by(year) |>
      mutate(num_at_age_year = n()) |>
      ungroup() |>
      group_by(year, age) |>
      mutate(num_at_age = n()) |>
      ungroup()|>
      select(year, age, num_at_age, num_at_age_year) |>
      distinct() |>
      select(year, age, num_at_age) |>
      arrange(year, age) |>
      complete(year, age = 1:plus_grp, fill = list(age_prop = 0)) |>
      pivot_wider(names_from = age, values_from = num_at_age) |>
      mutate(across(-year, ~{ifelse(is.na(.x), 0, .x)})) |>
      mutate(across(-year, ~{round(.x, digits)}))

    fn <- here(data_tables_path,
               switch(type,
                      "ft" = can_raw_ft_age_counts_fn,
                      "ss" = can_raw_ss_age_counts_fn,
                      "jv" = can_raw_jv_age_counts_fn))

    write_csv(out, fn)
    message("The file:\n`", fn, "`\nwas written with new age proportion ",
            "data\n")
    return(invisible(out))
  }

  all_yrs_lw <- fit_lw(d, lw_tol, lw_maxiter)

  ds <- d |>
    calc_lw_params("sample_id", lw_tol, lw_maxiter) |>
    calc_lw_params(temporal_grouping, lw_tol, lw_maxiter) |>
    rename(lw_alpha.x = lw_alpha,
           lw_beta.x = lw_beta) |>
    mutate(lw_alpha.y = all_yrs_lw[1],
           lw_beta.y = all_yrs_lw[2]) |>
    mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
           lw_beta = coalesce(lw_beta.x, lw_beta.y)) |>
    select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))

  # Calculate the weights from length for all missing weights,
  # using specimen-specific LW params
  ds <- ds |>
    dplyr::filter(!is.na(length)) |>
    mutate(weight = ifelse(is.na(weight),
                           lw_alpha * length ^ lw_beta,
                           weight))

  age_props <- ds |>
    group_by(sample_id) |>
    mutate(sample_weight = ifelse(is.na(sample_weight),
                                  sum(weight) / weight_scale,
                                  sample_weight)) |>
    mutate(sample_weight = ifelse(all(is.na(catch_weight)),
                                  1,
                                  sample_weight),
           catch_weight = ifelse(all(is.na(catch_weight)),
                                 1,
                                 catch_weight)) |>
    ungroup() |>
    group_by(sample_id, age) |>
    summarize(year = first(year),
              num_ages = n(),
              sample_weight = first(sample_weight),
              catch_weight = first(catch_weight)) |>
    ungroup() |>
    complete(sample_id, age) |>
    dplyr::filter(age > 0) |>
    group_by(sample_id) |>
    mutate(num_ages = ifelse(is.na(num_ages),
                             0,
                             num_ages),
           year = max(year, na.rm = TRUE),
           sample_weight = max(sample_weight, na.rm = TRUE),
           catch_weight = max(catch_weight, na.rm = TRUE)) |>
    mutate(num_ages_weighted = num_ages * catch_weight / sample_weight) |>
    ungroup() |>
    group_by(year, age) |>
    summarize(num_ages_weighted = sum(num_ages_weighted)) |>
    mutate(age_prop = num_ages_weighted / sum(num_ages_weighted)) |>
    ungroup() |>
    select(-num_ages_weighted) |>
    pivot_wider(names_from = age, values_from = age_prop) |>
    mutate(across(-year, ~{f(.x, digits)}))

  # Add number of fish and number of samples ----
  num_fish <- ds |>
    split(~year) |>
    map_dfr(~{
      .x |>
      group_by(year) |>
        summarize(num_fish = n()) |>
        ungroup()
    }) |>
    mutate(year = as.numeric(year)) |>
    mutate(num_fish = as.numeric(num_fish))

  num_samples <- ds |>
    split(~year) |>
    map_dbl(~{
      .x |>
        pull(sample_id) |>
        unique() |>
        length()
    })
  num_samples <- tibble(year = names(num_samples)) |>
    mutate(num_samples = num_samples) |>
    mutate(year = as.numeric(year))

  # Join `num_fish`, `num_samples`, and `age_props`
  out <- num_fish |>
    full_join(num_samples, by = "year") |>
    full_join(age_props, by = "year")

  # Write the csv files out ----
  fn <- here(data_tables_path,
             switch(type,
                    "ft" = can_ft_age_props_fn,
                    "ss" = can_ss_age_props_fn,
                    "jv" = can_jv_age_props_fn))

  write_csv(out, fn)
  message("The file:\n`", fn, "`\nwas written with new age proportion data\n")

  invisible(out)
}