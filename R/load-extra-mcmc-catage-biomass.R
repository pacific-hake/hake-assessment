#' Calculate the biomass-at-age in catch
#'
#' @rdname load_extra_mcmc_biomass
#' @param catage A catch-at-age data frame as created by
#' [load_extra_mcmc_atage()]
#' @param wtatage A weight-at-age data frame
#'
#' @return A data frame with the medians of the biomass-at-age in catch
#' @export
load_extra_mcmc_catage_biomass <- function(reps,
                                           catage,
                                           wtatage,
                                           verbose){

  if(verbose){
    message("Extracting Catch-at-age in biomass...")
  }
  year_column_string <- grep("year|yr",
                             colnames(wtatage),
                             value = TRUE,
                             ignore.case = TRUE)
  wtatage <- wtatage |>
    as_tibble() |>
    rename(yr = year_column_string[1])

  wt <- wtatage |>
    dplyr::filter(yr > 0)

  actual_start_yr <- min(catage$yr)
  start_yr_wtatage <- min(wt$yr)
  num_missing_yrs <- start_yr_wtatage - actual_start_yr

  if(num_missing_yrs > 0){
    # Need to fill in missing years without wt-at-age from actual_start_yr
    # to start_yr_wtatage
    row <- wt |>
      as_tibble() |>
      dplyr::filter(yr == min(yr), fleet == 2)

    if(nrow(row) > 1){
      stop("The biomass-at-age table failed to build. The `model$wtatage` ",
           "table had ", nrow(row), " rows match the filter for minimum ",
           "year and fleet == 2, when it should only be one row")
    }
    missing_yrs <- actual_start_yr:(start_yr_wtatage - 1)
    for(yr in rev(missing_yrs)){
      row$yr <- yr
      wt <- bind_rows(row, wt)
    }
  }
  ages <- grep("[0-9]+", names(wt), value = TRUE)
  wtatage <- wt |>
    dplyr::filter(fleet == 2) |>
    select(yr, all_of(ages))

  yrs <- sort(unique(catage$yr))
  # iters <- map(sort(unique(catage$iter)), ~{
  #   rep(.x, length(yrs))
  # })

  # Make a list of data frames, each with a unique iter value
  catage_lst <- catage |>
    split(~ iter)

  # Multiply catage data frame, from each posterior with wtatage
  # Note that the yr and iter columns are removed
  wtatage <- wtatage |>
    dplyr::filter(yr %in% unique(catage$yr))

  map(catage_lst, \(df){
    yr_iter <- df |>
      select(yr, iter)
    k <- as_tibble(select(df, -c(yr, iter)) *
                   as.vector(select(wtatage, -yr)))
    bind_cols(yr_iter, k)

  }) |>
    map_df(~{.x}) |>
    group_by(yr) |>
    summarize_all(median) |>
    select(-iter)
}
