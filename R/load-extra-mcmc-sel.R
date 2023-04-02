#' Load the selectivity outputs from the extra MCMC output
#'
#' @rdname load_extra_mcmc_biomass
#' @param end_yr End year in the model
#'
#' @export
load_extra_mcmc_sel <- function(reps,
                                probs,
                                progress_n,
                                verbose = TRUE,
                                end_yr,
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
    filter(Fleet == fleet) |>
    rename(yr = Yr, iter = Iter)

  ages <- grep("[0-9]+", names(ts), value = TRUE)

  ts <- ts |>
    select(yr, iter, all_of(ages))

  out <- list()
  out$sel <- ts
  out$sel_lo <- ts |>
    mutate_at(vars(-yr), ~{
      quantile(.x, probs = probs[1])
    }) |>
    slice(1)

  out$sel_med <- ts |>
    mutate_at(vars(-yr), ~{
      quantile(.x, probs = probs[2])
    }) |>
    slice(1)

  out$sel_hi <- ts |>
    mutate_at(vars(-yr), ~{
      quantile(.x, probs = probs[3])
    }) |>
    slice(1)

  # End year (last year of catch) selectivities
  pat <- paste0(end_yr, "_", fleet, "Asel")
  y <- load_extra_mcmc_get_chunk(reps, beg_pat = pat, end_pat = pat)
  out$sel_endyr <- extract_rep_table(reps_lst = y$lst,
                                     header = x$header,
                                     verbose = verbose,
                                     ...) |>
    select(all_of(ages)) |>
    map_df(~{median(.x)})

    out
}