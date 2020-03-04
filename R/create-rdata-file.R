#' Create an rdata file to hold the model's data and outputs.
#'
#' @param models_path Directory name for all models location
#' @param model_name Directory name of model to be loaded
#' @param ovwrt_rdata Logical. Overwrite the RData file if it exists
#'
#' @return [base::invisible()]
#' @export
create_rdata_file <- function(models_path = "models",
                              model_name = NULL,
                              ovwrt_rdata = FALSE,
                              catch_levels_path,
                              forecasts_path,
                              retrospectives_path,
                              extra_mcmc_path,
                              ...){

  stopifnot(!is.null(models_path),
            !is.null(model_name))

  model_path <- file.path(models_path, model_name)
  if(!dir.exists(model_path)){
    stop("Error - the directory ", model_path, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RData file will have the same name as the directory it is in
  rdata_file <- file.path(model_path, paste0(model_name, ".RData"))
  if(file.exists(rdata_file)){
    if(ovwrt_rdata){
      message("RData file found in ", model_path, ". Deleting...")
      unlink(rdata_file, force = TRUE)
    }else{
      message("RData file found in ", model_path, ". Keeping it...\n")
      return(invisible())
    }
  }else{
    message("No RData file found in ", model_path)
  }

  message("Creating a new RData file in ", model_path, "\n")

  # If this point is reached, no RData file exists so it has to be built from scratch
  model <- load_ss_files(model_path, ...)

  # Load forecasts. If none are found or there is a problem, model$forecasts will be NA
  if(dir.exists(file.path(model_path, forecasts_path))){
    catch_levels_path <- file.path(model_path, catch_levels_path)
    model$catch.levels <- fetch_catch_levels(catch_levels_path, ...)
    model$catch.default.policy <- model$catch.levels[[catch.default.policy.ind]][[1]]
    model$forecasts <- fetch_forecasts(model_path, forecasts_path, ...)
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
  model$retropath <- file.path(model$path, retrospectives_path)
  if(dir.exists(model$retropath)){
    model$retros <- fetch_retrospectives(model$retropath,
                                         retrospective_yrs)
  }else{
    model$retros <- NA
  }

  # # Try loading extra mcmc output. If none are found or there is a problem, model$extra.mcmc will be NA

  model$extra.mcmc.path <- file.path(model_path, extra_mcmc_path)
  if(dir.exists(model$extra.mcmc.path)){
    model$extra.mcmc <- fetch_extra_mcmc(model)
  }else{
    model$extra.mcmc <- NA
  }

  save(model, file = rdata_file)
  invisible()
}

#' Run extra models for forecasting, retrospectives, and extra MCMC (one report file per posterior)
#'
#' @details This is a wrapper function for calling [run_catch_levels()], [run_forecasts()],
#' [run_retrospectives()], and [run_extra_mcmc()] functions.
#'
#' @param models_path The path where the models directories reside
#' @param model_name The name of the directory the model resides in
#' @param ... Passed to the subroutines
#'
#' @return [base::invisible()]
#' @export
run <- function(models_path = NULL,
                model_name = NULL,
                ...){

  stopifnot(!is.null(models_path),
            !is.null(model_name))
  model_path <- file.path(models_path, model_name)

  if(!dir.exists(model_path)){
    stop("Error - the directory ", model_path, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }
  if(!dir.exists(file.path(model_path, "mcmc"))){
    return(NULL)
  }
  model <- load_ss_files(model_path, ...)

  run_catch_levels(model, ...)
  run_forecasts(model, ...)
  run_retrospectives(model, ...)
  run_extra_mcmc(model, ...)
  invisible()
}
