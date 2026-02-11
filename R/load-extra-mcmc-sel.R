#' Load the selectivity outputs from the extra MCMC output
#'
#' @rdname load_extra_mcmc_biomass
#' @param end_yr End year in the model
#'
#' @export
load_extra_mcmc_sel <- function(reps,
                                progress_n,
                                verbose = TRUE,
                                start_yr = NULL,
                                end_yr = NULL,
                                type = c("fishery", "survey"),
                                ...){

  type <- match.arg(type)
  fleet <- ifelse(type == "fishery", 1, 2)

  if(verbose){
    message(paste0("Extracting ", type, " selectivities..."))
  }
  x <- load_extra_mcmc_get_chunk(reps, ...)
  ts <- extract_rep_table(reps_lst = x$lst,
                          header = x$header,
                          verbose = verbose,
                          ...) |>
    dplyr::filter(Fleet == fleet)

  names(ts) <- tolower(names(ts))

  ages <- grep("[0-9]+", names(ts), value = TRUE)

  ts <- ts |>
    select(yr, all_of(ages))

  if(!is.null(start_yr)){
    ts <- ts |>
      dplyr::filter(yr >= start_yr)
  }
  # Fill in missing years in the projection period
  all_but_last_row <- head(ts, nrow(ts) - 1)
  last_row <- tail(ts, 1)
  num_missing_yrs <- last_row$yr - all_but_last_row[nrow(all_but_last_row), "yr"] - 1
  num_missing_yrs <- as.numeric(num_missing_yrs)
  if(num_missing_yrs > 1){
    # Missing year(s) between the second to last row and the last row, fill them in
    for(i in 1:num_missing_yrs){
      new_row <- all_but_last_row[nrow(all_but_last_row),]
      new_row[1, "yr"] <- new_row[1, "yr"] + 1
      all_but_last_row <- all_but_last_row |>
        bind_rows(new_row)
    }
    all_but_last_row <- all_but_last_row |>
      bind_rows(last_row)
    ts <- all_but_last_row
  }
  if(!is.null(end_yr)){
    ts <- ts |>
      dplyr::filter(yr <= end_yr)
  }

  out <- list()
  out$sel <- ts
  out$sel_lo <- ts |>
    group_by(yr) |>
    summarize_all(~{
      quantile(.x, probs = probs[1])
    }) |>
    ungroup()

  out$sel_med <- ts |>
    group_by(yr) |>
    summarize_all(~{
      quantile(.x, probs = probs[2])
    }) |>
    ungroup()

  out$sel_hi <- ts |>
    group_by(yr) |>
    summarize_all(~{
      quantile(.x, probs = probs[3])
    }) |>
    ungroup()

  # out$sel_end_yr <- ts |>
  #   dplyr::filter(yr == ifelse(type == "survey",
  #                              end_yr,
  #                              end_yr - 1)) |>
  #   select(-yr)
  out$sel_end_yr <- ts |>
    dplyr::filter(yr == end_yr) |>
    select(-yr)

  if(nrow(out$sel_end_yr) == 1){
    out$sel_end_yr <- out$sel_end_yr |> bind_rows(out$sel_end_yr)
  }

  out
}