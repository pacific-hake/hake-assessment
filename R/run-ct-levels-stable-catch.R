#' Run the model iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector of forecast years
#' @param ss_exe The name of executable to use or `NULL` to use the package
#' data variable [ss_executable]
#' @param keep_files Logical. If `TRUE`, keep all files in the directory,
#' if `FALSE` delete all files except for the filename contained in the
#' the `forecast_fn` variable. This is 'forecast.ss' by default for SS3
#' @param ... Absorbs other arguments intended for other functions
#'
#' @export
run_ct_levels_stable_catch <- function(
    model,
    forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
    ss_exe = NULL,
    keep_files = FALSE,
    ...){

  ss_exe <- get_ss3_exe_name(ss_exe)

  pth <- here(model$path, ct_levels_path, stable_catch_path)
  run_catch_levels_copy_input_files(model, pth)
  dest_derposts_fullpath_fn <- file.path(pth, derposts_fn)
  forecast_fullpath_fn <- file.path(pth, forecast_fn)
  # Make a modification to the starter file so the extra MCMC files are
  # not created
  modify_starter_mcmc_type(pth, 1)

  stable_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  iter <- 1
  repeat{
    out <- read.table(dest_derposts_fullpath_fn, header = TRUE) |>
      as_tibble()

    stable_catch <- map_dbl(forecast_yrs, ~{
      fore_yr_label <- paste0("ForeCatch_", .x)
      fore_yr_label_sym <- sym(fore_yr_label)

      out |>
        pull(!!fore_yr_label_sym) |>
        median(na.rm = TRUE)
    })

    message("Stable Catch: ")
    catch_diff <- abs(stable_catch[1] - stable_catch[2])
    message("Catch difference from forecast year 1 to 2: ",
            catch_diff,
            " < ", ct_levels_catch_tol, "? ",
            ifelse(catch_diff < ct_levels_catch_tol,
                   "Yes",
                   "No"))
    if(catch_diff < ct_levels_catch_tol){
      break
    }
    if(iter == ct_levels_max_iter){
      warning("The maximum number of iterations (", ct_levels_max_iter,
              ") was reached. The catch difference in the last iteration was ",
              catch_diff)
      break
    }
    fore <- SS_readforecast(forecast_fullpath_fn,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1])
    fore$ForeCatch <- data.frame(
      Year = forecast_yrs[1],
      Seas = 1,
      Fleet = 1,
      Catch_or_F = (stable_catch[1] + stable_catch[2]) / 2)
    SS_writeforecast(fore,
                     dir = pth,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(dest_derposts_fullpath_fn, force = TRUE)

    shell_command <- paste0("cd ", pth, " && ",
                            ss_executable, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
    iter <- iter + 1
  }
  fore <- SS_readforecast(forecast_fullpath_fn,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  fore$Ncatch <- length(forecast_yrs[1:length(forecast_yrs)])
  fore$ForeCatch <- data.frame(
    Year = forecast_yrs[1:length(forecast_yrs)],
    Seas = 1,
    Fleet = 1,
    Catch_or_F = stable_catch[seq_along(forecast_yrs)])
  SS_writeforecast(fore,
                   dir = pth,
                   overwrite = TRUE,
                   verbose = FALSE)
  unlink(dest_derposts_fullpath_fn, force = TRUE)
  shell_command <- paste0("cd ", pth, " && ",
                          ss_executable, " -mceval")
  system_(shell_command, wait = FALSE, intern = !show_ss_output)

  if(!keep_files){
    fns <- list.files(pth)
    fns <- fns[fns != forecast_fn]
    fns <- file.path(pth, fns)
    unlink(fns, force = TRUE)
  }

  invisible()
}
