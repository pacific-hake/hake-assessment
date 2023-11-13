#' Run the model, iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS3 model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector of forecast years
#' @param ss_exe The name of executable to use or `NULL` to use the package
#' data variable [ss_executable]
#' @param keep_files Logical. If `TRUE`, keep all files in the directory,
#' if `FALSE` delete all files except for the filename contained in the
#' the `forecast_fn` variable. This is 'forecast.ss' by default for SS3
#' @param ... Absorbs arguments intended for other functions
#'
#' @return Nothing, the 'forecast.ss' file will have the catch numbers at the
#' end of the file under 'Catch_or_F'
#' @export
run_ct_levels_default_hr <- function(
    model,
    forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
    ss_exe = NULL,
    keep_files = FALSE,
    ...){

  ss_exe <- get_ss3_exe_name(ss_exe)

  pth <- here(model$path, ct_levels_path, default_hr_path)
  run_catch_levels_copy_input_files(model, pth)
  dest_derposts_fullpath_fn <- file.path(pth, derposts_fn)
  forecast_fullpath_fn <- file.path(pth, forecast_fn)

  default_hr_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in seq_along(forecast_yrs)){

    out <- read.table(dest_derposts_fullpath_fn, header = TRUE) |>
      as_tibble()
    fore_yr_label <- paste0("ForeCatch_", forecast_yrs[i])

    if(!fore_yr_label %in% names(out)){
      stop("The forecast column `", fore_yr_label, "` does not exist in the ",
           "derived posteriors file `", dest_derposts_fullpath_fn, "`")
    }
    fore_yr_label_sym <- sym(fore_yr_label)
    default_hr_catch[i] <- out |>
      pull(!!fore_yr_label_sym) |>
      median(na.rm = TRUE)
    fore <- SS_readforecast(forecast_fullpath_fn,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1:i])
    fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                 Seas = 1,
                                 Fleet = 1,
                                 Catch_or_F = default_hr_catch[1:i])
    SS_writeforecast(fore,
                     dir = pth,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(dest_derposts_fullpath_fn, force = TRUE)
    message("Default HR - for forecast year: ",
            forecast_yrs[i], " of ",
            tail(forecast_yrs, 1))

    # Make a modification to the starter file so the extra MCMC files are
    # not created
    modify_starter_mcmc_type(pth, 1)

    shell_command <- paste0("cd ", pth, " && ",
                            ss_exe, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
  }

  if(!keep_files){
    fns <- list.files(pth)
    fns <- fns[fns != forecast_fn]
    fns <- file.path(pth, fns)
    unlink(fns, force = TRUE)
  }

  invisible()
}
