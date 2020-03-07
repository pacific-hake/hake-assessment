#' Build RDS file for a list of models by running forecasts, retrospectives, and extra-mcmc
#' where applicable
#'
#' @param model_dirs A vector of model directory names (not full paths)
#' @param run_forecasts Logical. TRUE to run forecasts on the models (when an mcmc directory is present)
#' @param run_retrospectives Logical. TRUE to run retrospectives on the models (when an mcmc directory is present)
#' @param run_extra_mcmc Logical. TRUE to run extra-mcmc routine on the models (when an mcmc directory is present)
#' @param catch_levels A list of lists for the forecast catch levels to run forecasts for. See
#' forecast-catch-levels.R
#'
#' @return Nothing
#' @export
#' @importFrom purrr map
#'
#' @examples
#' build(c("base", "nuts"), TRUE, TRUE, TRUE, catch_levels)
build <- function(model_dirs,
                  run_forecasts = FALSE,
                  run_retrospectives = FALSE,
                  run_extra_mcmc = FALSE,
                  catch_levels = NULL,
                  ...){

  map(model_dirs, ~{
    if(FALSE){
    # if(run_forecasts |
    #    run_retrospectives |
    #    run_extra_mcmc){
      stopifnot(!is.null(catch_levels))
      run(model_dir = .x,
          run_forecasts = run_forecasts,
          run_retrospectives = run_retrospectives,
          run_extra_mcmc = run_extra_mcmc,
          catch_levels = catch_levels)

    }
    create_rds_file(model_dir = .x, ...)
  }, ...)
  message("\nCompleted build.\n")
  invisible()
}

#' Create an rdata file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL, ...){

  stopifnot(!is.null(model_dir))

  model_fullpath <- file.path(models_path, model_dir)
  if(!dir.exists(model_fullpath)){
    stop("Error - the directory ", model_fullpath, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_fullpath, paste0(model_dir, ".rds"))
  if(file.exists(rds_file)){
    unlink(rds_file, force = TRUE)
  }

  message("Creating a new RDS file in ", model_fullpath, "\n")

  # If this point is reached, no RData file exists so it has to be built from scratch
  model <- load_ss_files(model_fullpath, ...)

  # Load forecasts. If none are found or there is a problem, model$forecasts will be NA
  if(dir.exists(file.path(model_fullpath, forecasts_path))){
    catch_levels_fullpath <- file.path(model_fullpath, catch_levels_path)
    model$catch.levels <- fetch_catch_levels(catch_levels_fullpath, catch_levels)
    model$catch.default.policy <- model$catch.levels[[catch.default.policy.ind]][[1]]
    model$forecasts <- fetch_forecasts(model_fullpath, model$catch.levels, ...)
    model$risks <- calc_risk(forecast_outputs = model$forecasts,
                             catch_levels = model$catch.levels,
                             forecast_yrs)
  }else{
    model$catch.levels <- NA
    model$catch.default.policy <- NA
    model$forecasts <- NA
    model$risks <- NA
  }

  # Load retrospectives. If none are found or there is a problem, model$retros will be NA
  model$retropath <- file.path(model_fullpath, retrospectives_path)
  if(dir.exists(model$retropath)){
    model$retros <- fetch_retrospectives(model$retropath,
                                         retrospective_yrs)
  }else{
    model$retros <- NA
  }

  # Try loading extra mcmc output. If none are found or there is a problem, model$extra.mcmc will be NA
  model$extra.mcmc.path <- file.path(model_fullpath, extra_mcmc_path)
  if(dir.exists(model$extra.mcmc.path)){
    model$extra.mcmc <- fetch_extra_mcmc(model)
  }else{
    model$extra.mcmc <- NA
  }

  saveRDS(model, file = rds_file)
  invisible()
}

#' Run extra models for forecasting, retrospectives, and extra MCMC (one report file per posterior)
#'
#' @details This is a wrapper function for calling [run_catch_levels()], [run_forecasts()],
#' [run_retrospectives()], and [run_extra_mcmc()] functions.
#'
#' @param model_dirs The name of the directories the models reside in
#' @param ... Passed to the subroutines
#'
#' @return [base::invisible()]
#' @export
run <- function(model_dir = NULL,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                run_extra_mcmc = FALSE,
                ...){

  stopifnot(!is.null(model_dir))
  model_dir <- file.path(models_path, model_dir)

  if(!dir.exists(model_dir)){
    stop("The ", model_dir, " directory does not exist",
         call. = FALSE)
  }
  if(dir.exists(file.path(model_dir, "mcmc"))){
    if(run_forecasts){
      #run_catch_levels(model_dir, ...)
      run_forecasts(model_dir, ...)
    }
    if(run_retrospectives){
      run_retrospectives(model_dir, ...)
    }
    if(run_extra_mcmc){
      run_extra_mcmc(model_dir, ...)
    }
  }
}
