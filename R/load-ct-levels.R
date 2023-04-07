#' Load catch levels from SS forecast files. This is used to retrieve ending
#' values after running the [run_ct_levels()] function for default HR, SPR 100,
#' and stable catch
#'
#' @details
#' Assumes [run_ct_levels()] function has been run and the forecast
#' files are populated with 3 forecast years.
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param ct_levels_lst The catch levels list as returned by
#' `set_ct_levels()`
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of 3-element lists of vectors of 3 catch levels
#' corresponding to:
#' a) SPR-100%
#' b) Default harvest policy
#' c) Stable catch
#' Return object looks the same as the `ct_levels` object but with three more
#' elements
#' @export
load_ct_levels <- function(model,
                           ct_levels_lst = NULL,
                           ...){

  stopifnot(!is.null(ct_levels_lst))

  message("\nLoading catch levels from ", model$ct_levels_path)

  msgs <-
    c(paste0("Loading 'Default HR' catch level from ",
             model$default_hr_path),
      paste0("Loading 'SPR 100' catch level from ",
             model$spr_100_path),
      paste0("Loading 'Stable Catch' catch level from ",
             model$stable_catch_path))

  fns <- c(file.path(model$default_hr_path, model$forecast_fn),
           file.path(model$spr_100_path, model$forecast_fn),
           file.path(model$stable_catch_path, model$forecast_fn))

  cust_ct_levels <- map(1:3, \(ind){
    message(msgs[ind])
    fore <- SS_readforecast(fns[ind],
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$ForeCatch |>
      select(`Catch or F`) |>
      pull()
  })
  ct_levels <- ct_levels_lst$ct_levels

  # Replace the NA values for the custom catch levels with the values read in
  inds <- (length(ct_levels) - length(cust_ct_levels) + 1):length(ct_levels)
  ct_levels[inds] <- map(seq_along(ct_levels[inds]), ~{
    ct_levels[inds][[.x]][1] <- cust_ct_levels[.x]
    ct_levels[inds][[.x]]
  })

  list(ct_levels = ct_levels,
       ct_levels_vals = ct_levels_lst$ct_levels_vals)
}
