#' Load the vulnerability outputs from the extra MCMC output
#'
#' @rdname load_extra_mcmc_biomass
#'
#' @export
load_extra_mcmc_selwt <- function(reps,
                                  verbose = TRUE,
                                  head_beg_pat,
                                  head_end_pat,
                                  ...){

  if(verbose){
    message("Extracting vulnerable biomass...")
  }

  # Get header first, the same way as in `load_extra_mcmc_sel()`
  x <- load_extra_mcmc_get_chunk(reps,
                                 beg_pat = head_beg_pat,
                                 end_pat = head_end_pat)

  # This is just to get the header, x$lst is not used in this function
  ts <- extract_rep_table(reps_lst = x$lst,
                          header = x$header,
                          verbose = verbose,
                          ...)

  y <- load_extra_mcmc_get_chunk(reps, ...)
  if(is.na(y[1])){
    return(NA)
  }
  selwt <- extract_rep_table(reps_lst = y$lst,
                             header = x$header,
                             verbose = verbose,
                             ...) |>
    rename(yr = Yr, iter = Iter)

  ages <- grep("[0-9]+", names(selwt), value = TRUE)

  out <- list()
  out$selwt <- selwt |>
    select(yr, iter, all_of(ages))

  out$selwt_med <- map_df(out$selwt |>
                            select(-c(yr, iter)), ~{median(.x)})

  out
}