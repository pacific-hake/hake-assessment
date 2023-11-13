#' Run catch-levels, forecasting, and retrospectives in one function
#'
#' @details
#' This is a wrapper function for calling [run_ct_levels()],
#' [run_forecasts()], and [run_retrospectives()] functions.
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param model_path The directory the models are located in
#' @param forecast_yrs A vector of forecast years
#' @param run_forecasts Logical. If `TRUE` run forecasting
#' @param run_retrospectives Logical. If `TRUE` run retrospectives
#' @param run_catch_levels Logical. If `TRUE` run catch levels estimation.
#' If this is `FALSE` and `run_forecasts` is `TRUE` but the directory
#' that is supposed to hold the catch-levels output ([ct_levels_path]) is
#' missing, then the catch levels will run anyway, because those value
#' are needed for forecasting
#' @param ... Arguments passed to the [run_ct_levels()], [run_forecasts()],
#' and [run_retrospectives()] functions
#'
#' @return [base::invisible()]
#' @export
run_forecasts_retrospectives <- function(
    model = NULL,
    model_path = NULL,
    forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
    run_catch_levels = FALSE,
    run_forecasts = FALSE,
    run_retrospectives = FALSE,
    ...){

  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
    }
    model <- load_ss_files(model_path, ...)
  }else{
    if(is.null(model_path)){
      model_path <- model$path
    }else{
      if(model$path != model_path){
        stop("You provided both `model` and `model_path` and `model$path` ",
             "does not math `model_path`")
      }
    }
  }

  if(!dir.exists(model_path)){
    stop("The ", model_path, " directory does not exist")
  }

  mcmc_fullpath <- file.path(model_path, mcmc_path)
  if(!dir.exists(mcmc_fullpath)){
    stop("The ", mcmc_fullpath, " directory does not exist")
  }

  if(run_catch_levels && !run_forecasts){
    # You can add arguments from the run_ct_* functions here which are:
    # ss_exe and keep_files. If you don not supply it here, `ss_exe` will
    # automatically be selected as the value of the package data variable
    # [ss_executable] found in `data-raw/pd-ss-filenames.R`
    # keep_files tells the run_ct_* routines to keep all the files or just
    # keep the forecast.ss file (the default choice)
    run_ct_levels(model, ...)
  }

  if(run_forecasts){
    run_ct_levels(model, ...)
    run_forecasts(model, ...)
  }

  if(run_retrospectives){
    # Extra-mcmc is automatically run but can be turned off with
    # `retro_mcmc = FALSE`
    # These calls are split up because  `num_chains`, which is passed through
    # the ellipsis all the way down to `run_adnuts()` is assumed to be 16
    # If it is 16, then 4 `yrs` running in parallel will be 64CPUs

    ncores <- availableCores()
    run_retrospectives(model, yrs = 1:4, ...)
    run_retrospectives(model, yrs = 5:7, ...)
    run_retrospectives(model, yrs = 8:10, ...)
  }

  invisible()
}
