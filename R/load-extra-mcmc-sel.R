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

  out$sel_end_yr <- ts |>
    dplyr::filter(yr == ifelse(type == "survey",
                               end_yr,
                               end_yr - 1)) |>
    select(-yr)

    out
}