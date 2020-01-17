#' Create an rdata file to hold the model's data and outputs.
#' 
#' @details If an RData file exists, and overwrite is FALSE, return immediately.
#' If no RData file exists, the model will be loaded from outputs into an R list
#' and saved as an RData file in the correct directory.
#' When this function exits, an RData file will be located in the
#' directory given by model.name.
#' Assumes the files model-setup.r, retrospective-setup.r, and forecast-catch-levels.r
#' have been sourced (for default values of args).
#'
#' @param models.dir Directory name for all models location
#' @param model.name Directory name of model to be loaded
#' @param ovwrt.rdata Overwrite the RData file if it exists?
#' @param run.catch.levels Run the catch levels determination for the Default HR, SPR 100
#' and Stable catch cases.
#' @param run.fore Run forecasting metrics for this model? *This will overwrite any already run*
#' @param fore.yrs Vector of years to run forecasting
#' @param forecast.probs Vector of quantile values for forecasting
#' @param forecast.catch.levels List of catch levels to run forecasting for if run.fore = TRUE
#' @param run.retros Run retrospectives for this model? *This will overwrite any already run*
#' @param my.retro.yrs Vector of integers (positives) to run retrospectives for if run.retros = TRUE
#' @param run.extra.mcmc Run extra mcmc output (a report file for each of the mcmc samples)
#' @param key.posteriors Vector of key posteriors used to create key posteriors file
#' @param key.posteriors.fn Key posteriors file name
#' @param nuisance.posteriors.fn Nuisance posteriors file name 
#' @param ss.version Version of SS used in this assessment
#' @param exe.file.name SS executable file name
#' @param starter.file.name SS starter file name
#' @param forecast.file.name SS forecast file name
#' @param weight.at.age.file.name SS weight-at-age file name
#'
#' @return [base::invisible()]
#' @export
# create.rdata.file <- function(models.dir = "models",
#                               model.name = NA,
#                               ovwrt.rdata = FALSE,
#                               run.catch.levels = FALSE,
#                               run.fore = FALSE,
#                               fore.yrs = NA,
#                               forecast.probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
#                               forecast.catch.levels = NA,
#                               run.retros = FALSE,
#                               my.retro.yrs = NA,
#                               run.extra.mcmc = FALSE,
#                               key.posteriors = c("NatM", "SR_LN", "SR_BH_steep", "Q_extraSD"),
#                               key.posteriors.fn = "keyposteriors.csv",
#                               nuisance.posteriors.fn = "nuisanceposteriors.csv",
#                               ss.version = "3.30.14.08",
#                               exe.file.name = "ss.exe",
#                               starter.file.name = "starter.ss",
#                               forecast.file.name = "forecast.ss",
#                               weight.at.age.file.name = "wtatage.ss",
#                               ...){
create.rdata.file <- function(models.dir = "models",
                              model.name = NULL,
                              ovwrt.rdata = FALSE,
                              run.catch.levels = FALSE,
                              run.fore = FALSE,
                              run.retros = FALSE,
                              run.extra.mcmc = FALSE,
                              ...){
  
  stopifnot(!is.null(models.dir),
            !is.null(model.name))

  model.dir <- file.path(models.dir, model.name)
  if(!dir.exists(model.dir)){
    stop("Error - the directory ", model.dir, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }
  
  # The RData file will have the same name as the directory it is in
  rdata.file <- file.path(model.dir, paste0(model.name, ".RData"))
  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      message("RData file found in ", model.dir, ". Deleting...")
      unlink(rdata.file, force = TRUE)
    }else{
      message("RData file found in ", model.dir, ". Keeping it...")
      return(invisible())
    }
  }else{
    message("No RData file found in ", model.dir, ". Creating one now...")
  }
  if(!ovwrt.rdata){
    if(run.catch.levels){
      stop("Error - You have asked to run catch level determination, but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.", call. = FALSE)
    }
    if(run.fore){
      stop("Error - You have asked to run forecasting, but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.", call. = FALSE)
    }
    if(run.retros){
      stop("Error - You have asked to run retrospectives, but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.", call. = FALSE)
    }
    if(run.extra.mcmc){
      stop("Error - You have asked to run the extra mcmc output, but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.", call. = FALSE)
    }
  }
  
  # If this point is reached, no RData file exists so it has to be built from scratch
  model <- load.ss.files(model.dir, ...)

  model$retropath <- file.path(model$path, "retrospectives")
  if(run.retros){
    # run.retrospectives(model,
    #                    yrs = my.retro.yrs,
    #                    exe.file.name = exe.file.name,
    #                    starter.file.name = starter.file.name,
    #                    forecast.file.name = forecast.file.name,
    #                    weight.at.age.file.name = weight.at.age.file.name)
    run.retrospectives(model = NA,
                       yrs = 1:15,
                       remove.blocks = FALSE,
                       extras = "-nox",
                       exe.file.name = "ss.exe",
                       starter.file.name = "starter.ss",
                       forecast.file.name = "forecast.ss",
                       weight.at.age.file.name = "wtatage.ss",)
    
  }
  if(dir.exists(model$mcmcpath)){
    if(run.catch.levels){
      calc.catch.levels(model,
                        forecast.yrs,
                        catch.levels,
                        catch.levels.path = "catch-levels",
                        default.hr.path = "default-hr",
                        stable.catch.path = "stable-catch",
                        spr.100.path = "spr-100")
      
    }
    if(run.fore){
      # run.forecasts(model,
      #               fore.yrs,
      #               forecast.probs,
      #               forecast.catch.levels)
      run.forecasts(model, ...)
    }
    if(run.extra.mcmc){
      model$extra.mcmc.path = file.path(model$path, "extra-mcmc")
      run.extra.mcmc.models(model)
    }
  }
  
  # Load forecasts.  If none are found or there is a problem, model$forecasts will be NA
  if(dir.exists(file.path(model$mcmcpath, "forecasts"))){
    model$catch.levels <- fetch.catch.levels(model,
                                             forecast.catch.levels)
    model$catch.default.policy <- model$catch.levels[[catch.default.policy.ind]][[1]]
    model$forecasts <- fetch.forecasts(model$mcmcpath,
                                       fore.yrs,
                                       model$catch.levels,
                                       fore.probs = forecast.probs)
    model$risks <- calc.risk(model$forecasts,
                             model$catch.levels,
                             fore.yrs)
  }else{
    model$catch.levels <- NA
    model$catch.default.policy <- NA
    model$forecasts <- NA
    model$risks <- NA
  }

  # Load retrospectives. If none are found or there is a problem, model$retros will be NA
  model$retropath <- file.path(model$path, "retrospectives")
  if(dir.exists(model$retropath)){
    model$retros <- fetch.retros(model$retropath,
                                 my.retro.yrs)
  }else{
    model$retros <- NA
  }

  # Try loading extra mcmc output. If none are found or there is a problem, model$extra.mcmc will be NA
  model$extra.mcmc.path <- file.path(model$path, "extra-mcmc")
  if(dir.exists(model$extra.mcmc.path)){
    model$extra.mcmc <- fetch.extra.mcmc(model)
  }else{
    model$extra.mcmc <- NA
  }

  save(model, file = rdata.file)
  invisible()
}

#' Run extra models for forecasting, retrospectives, and extra MCMC (one report file per posterior)
#'
#' @details This is a wrapper function for calling other run_*() functions. The catch-levels part must
#' have been successfully run before the forecasting can commence.
#' 
#' @param model_path The directory the model resides in
#' @param run_catch_levels Logical. Run the catch levels determination
#' @param run_forecasts Logical. Run the forercasts
#' @param run_retrospectives Logical. Run the retrospectives
#' @param run_extra_mcmc Logical. Run the extra MCMC routines
#' @param ... Passed to the subroutines
#'
#' @return [base::invisible()]
#' @export
run <- function(model_path = NULL,
                run_catch_levels = FALSE,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                run_extra_mcmc = FALSE,
                ...){
  
  if(!dir.exists(model_path)){
    stop("Error - the directory ", model_path, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }
  model <- load.ss.files(model_path, ...)

  if(run_catch_levels){
    run_catch_levels(model, ...)
  }
  if(run_forecasts){
    run_catch_levels(model, ...)
    run_forecasts(model, ...)
  }
  # 
  # if(run_retros){
  #   run.retrospectives(path = model.dir, yrs = 1:2, ...)
  #                      remove.blocks = FALSE,
  #                      extras = "-nox",
  #                      exe.file.name = "ss.exe",
  #                      starter.file.name = "starter.ss",
  #                      forecast.file.name = "forecast.ss",
  #                      weight.at.age.file.name = "wtatage.ss")
  #   
  # }
  # if(dir.exists(model$mcmcpath)){
  #   if(run.fore){
  #     run.forecasts(model, ...)
  #   }
  #   if(run.extra.mcmc){
  #     model$extra.mcmc.path <- file.path(model$path, "extra-mcmc")
  #     run.extra.mcmc.models(model)
  #   }
  # }
  
}