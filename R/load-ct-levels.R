#' Load catch levels from SS forecast files.
#'
#' @details
#' This is used to retrieve calculated values after running the
#' [run_ct_levels()] function for default HR, SPR 100, and stable catch
#' stream scenarios
#'
#' @details
#' Assumes [run_ct_levels()] function has been run and the forecast
#' files are populated with 3 forecast years.
#'
#' @param model_path The model directory name
#' @param inc_fi_and_stable_catch Logical. If `TRUE`, include the Fishing
#' intensity = 100% and the Stable Catch scenarios
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of 3-element lists of vectors of 3 catch levels
#' corresponding to:
#' 1. vector of values, 1 for each forecast year
#' 2. Pretty name of the catch stream scenario
#' 3. The directory name for the scenario
#'
#' Return object looks the same as the the list returned from
#' `set_ct_levels()` but with the NAs replaced for the catch levels that
#' needed to be run through algorithms to get the catch values (default-hr,
#' spr-100, stable-catch)
#'
#' @export
load_ct_levels <- function(model_path,
                           inc_fi_and_stable_catch = FALSE,
                           ...){

  if(!check_catch_levels(model_path,
                         inc_fi_and_stable_catch = inc_fi_and_stable_catch,
                         ...)){
    stop("The catch levels do not appear to have been run, have been run ",
         "incorrectly, or only been run partially. Re-run using ",
         "`run_catch_levels` before trying to attach forecasting")
  }


  #ct_levels_lst <- set_ct_levels(inc_fi_and_stable_catch = inc_fi_and_stable_catch)
  ct_levels_lst <- set_ct_levels(inc_fi_and_stable_catch = inc_fi_and_stable_catch)

  variable_stream_nms <- ordered_decision_table_paths
  if(!inc_fi_and_stable_catch){
    fi_and_stable_catch_inds <- grep("spr|stable", ordered_decision_table_paths)
    if(length(fi_and_stable_catch_inds)){
      variable_stream_nms <- variable_stream_nms[-fi_and_stable_catch_inds]
    }
  }
  ct_levels_fullpath <- file.path(model_path, ct_levels_path)
  drs <- list.files(ct_levels_fullpath)
  if(!any(map_lgl(drs, ~{.x %in% variable_stream_nms}))){
    stop("None of the calculated catch stream directories exist:\n\n",
         paste(variable_stream_nms, collapse = "\n"), "\n",
         "The directories you have are:\n\n",
         paste(drs, collapse = "\n"), "\n\n")
  }
  drs <- variable_stream_nms
  fore_fns <- file.path(ct_levels_fullpath, drs, forecast_fn)
  message("\nLoading catch levels from ", ct_levels_fullpath)

  msgs <- map_chr(fore_fns, \(fn){
    paste0("Loading catch level output from\n", fn)
  })

  cust_ct_levels <- map2(fore_fns, msgs, \(fn, msg){
    message(msg)
    if(!file.exists(fn)){
      stop("File `", fn, "` does not exist. Something must have gone wrong ",
           "with the run_ct_levels() process")
    }
    fore <- SS_readforecast(fn,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)

    fore$ForeCatch |>
      select(`catch_or_F`) |>
      pull()
  })
  ct_levels <- ct_levels_lst$ct_levels

  # Replace the NA values for the custom catch levels with the values read in
  # # They are assumed to be at the end

  # First, find the indices of the custom catch levels within the catch levels
  # list
  ct_dirnames <- ct_levels |> map_chr(~{.x[[3]]})
  ct_inds <- map_dbl(drs, ~{
    grep(.x, ct_dirnames)
  }) |>
    sort()
  if(length(ct_inds) != length(drs)){
    stop("One or more of the catch level names does not have a directory ",
         "name in the catch levels list that matches. See the package data ",
         "values for `default_hr_path`, `spr_100_path`, and ",
         "`stable_catch_path` and make sure those strings are contained ",
         "in the directory names in the file `",
         file.path(doc_path, forecast_descriptions_fn), "`")
  }

  ct_levels[ct_inds] <- map(seq_along(ct_levels[ct_inds]), ~{
    ct_levels[ct_inds][[.x]][1] <- cust_ct_levels[.x]
    ct_levels[ct_inds][[.x]]
  })

  list(ct_levels = ct_levels,
       ct_levels_vals = ct_levels_lst$ct_levels_vals)
}
