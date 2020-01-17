#' Fetch catch levels from SS forecast files. This is used to retrieve ending values
#' after running the [run_catch_levels()] function for default HR, SPR 100, and stable catch
#' 
#' @details Assumes [run_catch_levels()] function has been run and the forecast files
#' are populated with 3 forecast years 
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param catch_levels_path The path for the catch-levels output
#' @param spr_100_path The path for the spr-100 output
#' @param default_hr_path The path for the default hr output
#' @param stable_catch_path The path for the stable-catch output
#'
#' @return A list of 3-element lists of vectors of 3 catch levels corresponding to:
#' a) SPR-100%
#' b) Default harvest policy
#' c) Stable catch
#' Return object looks the same as the `catch_levels`` object but with three more elements
#' @export
fetch_catch_levels <- function(model_path,
                               catch_levels_path,
                               spr_100_path,
                               default_hr_path,
                               stable_catch_path,
                               catch_levels,
                               ...){
  
  catch_levels_path <- file.path(model_path, catch_levels_path)
  default_hr_path <- file.path(catch_levels_path, default_hr_path)
  spr_100_path <- file.path(catch_levels_path, spr_100_path)
  stable_catch_path <- file.path(catch_levels_path, stable_catch_path)

  forecast_file <- file.path(spr_100_path, "forecast.ss")
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  
  ind <- length(catch_levels) - 2
  catch_levels[[ind]][[1]] <- fore$ForeCatch$Catch_or_F
  
  forecast_file <- file.path(default_hr_path, "forecast.ss")
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  ind <- ind + 1
  catch_levels[[ind]][[1]] <- fore$ForeCatch$Catch_or_F
  
  forecast_file <- file.path(stable_catch_path, "forecast.ss")
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  ind <- ind + 1
  catch_levels[[ind]][[1]] <- fore$ForeCatch$Catch_or_F
  
  catch_levels
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast_yrs A vector of forecast years
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param default_hr_path The path for the default hr output
#' @param ss_executable The name of the SS executable
#'
#' @export
run_catch_levels_default_hr <- function(model,
                                        forecast_yrs,
                                        catch_levels,
                                        default_hr_path = "default-hr",
                                        ss_executable = NULL,
                                        ...){
  
  stopifnot(!is.null(ss_executable))
  
  mcmc_path <- model$mcmcpath
  unlink(file.path(default_hr_path, "*"), recursive = TRUE)
  dir.create(default_hr_path, showWarnings = FALSE)

  file.copy(file.path(mcmc_path,
                      list.files(mcmc_path)),
            file.path(default_hr_path,
                      list.files(mcmc_path)),
            copy.mode = TRUE)
  
  forecast_file <- file.path(default_hr_path, "forecast.ss")
  default_hr_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    out <- read.table(file.path(default_hr_path,
                                "derived_posteriors.sso"),
                      header = TRUE)
    default_hr_catch[i] <- median(as.numeric(out[paste0("ForeCatch_",
                                                        forecast_yrs[i])][[1]]))
    fore <- SS_readforecast(forecast_file,
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
                     dir = default_hr_path,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(default_hr_path, "derived_posteriors.sso"),
           force = TRUE)
    shell_command <- paste0("cd ", default_hr_path, " & ", ss_executable, " -mceval")
    shell(shell_command)
  }
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast_yrs A vector of forecast years
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param catch_levels_spr_tol The tolerance in the spr away from 1.
#' @param catch_levels_catch_tol The tolerance in tonnes. The iterations will stop if the difference between the
#' projected biomass between the first and second years is less than this
#' @param catch_levels_max_iter The maximum number of iterations to do. If this is reached, then no catch value could 
#' be found within the tolerance
#' @param spr_100_path The path for the spr-100 output
#' @param catch_levels_path The path for the catch-levels output
#' @param ss_executable The name of the SS executable
#'
#' @export
run_catch_levels_spr_100 <- function(model,
                                     forecast_yrs,
                                     catch_levels,
                                     catch_levels_spr_tol = 0.0001,
                                     catch_levels_catch_tol = 100,
                                     catch_levels_max_iter = 20,
                                     catch_levels_path = "catch-levels",
                                     spr_100_path = "spr-100",
                                     ss_executable = NULL,
                                     ...){
  
  stopifnot(!is.null(ss_executable))
  
  mcmc_path <- model$mcmcpath
  unlink(file.path(spr_100_path, "*"), recursive = TRUE)
  dir.create(spr_100_path, showWarnings = FALSE)
  
  file.copy(file.path(mcmc_path,
                      list.files(mcmc_path)),
            file.path(spr_100_path,
                      list.files(mcmc_path)),
            copy.mode = TRUE)
  
  forecast_file <- file.path(spr_100_path, "forecast.ss")
  spr_100_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    fore <- SS_readforecast(forecast_file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1:i])
    upper <- spr_100_catch[i] <- median(model$mcmc[paste0("ForeCatch_",
                                                          forecast_yrs[i])][[1]])
    lower <- 0
    iter <- 1
    repeat{
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = spr_100_catch[1:i])
      SS_writeforecast(fore,
                       dir = spr_100_path,
                       overwrite = TRUE,
                       verbose = FALSE)
      unlink(file.path(spr_100_path, "derived_posteriors.sso"),
             force = TRUE)
      shell_command <- paste0("cd ", spr_100_path, " & ", ss_executable, " -mceval")
      shell(shell_command)
      out <- read.table(file.path(spr_100_path,
                                  "derived_posteriors.sso"),
                        header = TRUE)
      spr <- median(as.numeric(out[paste0("SPRratio_", forecast_yrs[i])][[1]]))
      if(abs(spr - 1) < catch_levels_spr_tol |
         abs(upper - lower) < catch_levels_catch_tol){
        ## Sometimes, upper and lower can end up close to equal,
        ##  but the tolerance is still not met. In this case, assume
        ##  the catch creates an SPR of 100% even though it is slightly off.
        break
      }
      if(iter == catch_levels_max_iter){
        warning("The maximum number of iterations (", catch_levels_max_iter,") was reached for forecast year ", forecast_yrs[i],
                ". The SPR difference in the last iteration was ", spr - 1)
        break
      }
      
      if(spr - 1 > 0){
        upper <- spr_100_catch[i]
        spr_100_catch[i] <- (lower + spr_100_catch[i]) / 2.0
      }else{
        lower <- spr_100_catch[i]
        spr_100_catch[i] <- (upper + spr_100_catch[i]) / 2.0
      }
      iter <- iter + 1
    }
  }
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast_yrs A vector of forecast years
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param catch_levels_catch_tol The tolerance in tonnes. The iterations will stop if the difference between the
#' projected biomass between the first and second years is less than this
#' @param catch_levels_max_iter The maximum number of iterations to do. If this is reached, then no catch value could 
#' be found within the tolerance
#' @param catch_levels_path The path for the catch-levels output
#' @param stable_catch_path The path for the stable-catch output
#' @param ss_executable The name of the SS executable
#'
#' @export
run_catch_levels_stable_catch <- function(model,
                                          forecast_yrs,
                                          catch_levels,
                                          catch_levels_catch_tol = 100,
                                          catch_levels_max_iter = 20,
                                          catch_levels_path = "catch-levels",
                                          stable_catch_path = "stable-catch",
                                          ss_executable = NULL,
                                          ...){

  stopifnot(!is.null(ss_executable))
  
  mcmc_path <- model$mcmcpath
  unlink(file.path(stable_catch_path, "*"), recursive = TRUE)
  dir.create(stable_catch_path, showWarnings = FALSE)
  
  file.copy(file.path(mcmc_path,
                      list.files(mcmc_path)),
            file.path(stable_catch_path,
                      list.files(mcmc_path)),
            copy.mode = TRUE)
  
  forecast_file <- file.path(stable_catch_path, "forecast.ss")
  stable_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  out <- read.table(file.path(stable_catch_path,
                              "derived_posteriors.sso"),
                    header = TRUE)
  iter <- 1
  repeat{
    out <- read.table(file.path(stable_catch_path,
                                "derived_posteriors.sso"),
                      header = TRUE)
    stable_catch[1] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[1])][[1]]))
    stable_catch[2] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[2])][[1]]))
    stable_catch[3] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[3])][[1]]))
    if(abs(stable_catch[1] - stable_catch[2]) < catch_levels_catch_tol){
      break
    }
    if(iter == catch_levels_max_iter){
      warning("The maximum number of iterations (", catch_levels_max_iter,") was reached. The catch difference in the last iteration was ",
              abs(stable_catch[1] - stable_catch[2]))
      break
    }
    fore <- SS_readforecast(forecast_file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1])
    fore$ForeCatch <- data.frame(Year = forecast_yrs[1],
                                 Seas = 1,
                                 Fleet = 1,
                                 Catch_or_F = (stable_catch[1] + stable_catch[2]) / 2)
    SS_writeforecast(fore,
                     dir = stable_catch_path,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(stable_catch_path, "derived_posteriors.sso"),
           force = TRUE)
    shell_command <- paste0("cd ", stable_catch_path, " & ", ss_executable, " -mceval")
    shell(shell_command)
    iter <- iter + 1
  }
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  fore$Ncatch <- length(forecast_yrs[1:length(forecast_yrs)])
  fore$ForeCatch <- data.frame(Year = forecast_yrs[1:length(forecast_yrs)],
                               Seas = 1,
                               Fleet = 1,
                               Catch_or_F = stable_catch[1:length(forecast_yrs)])
  SS_writeforecast(fore,
                   dir = stable_catch_path,
                   overwrite = TRUE,
                   verbose = FALSE)
  unlink(file.path(stable_catch_path, "derived_posteriors.sso"),
         force = TRUE)
  shell_command <- paste0("cd ", stable_catch_path, " & ", ss_executable, " -mceval")
  shell(shell_command)
}

#' A wrapper to run the catch levels determination routines
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param ovr_hr Logical. Overwrite the default_hr case
#' @param ovr_spr Logical. Overwrite the spr_100 case 
#' @param ovr_stable Logical. Overwrite the stable_catch case
#' @param ... Passed to the run.*() functions for each case
#'
#' @return
#' @export
#'
#' @examples
run_catch_levels <- function(model,
                             catch_levels_path,
                             catch_levels_ovr_hr = FALSE,
                             catch_levels_ovr_spr = FALSE,
                             catch_levels_ovr_stable = FALSE,
                             default_hr_path,
                             spr_100_path,
                             stable_catch_path,
                             ...){

  model_path <- model$path
  catch_levels_path <- file.path(model_path, catch_levels_path)
  if(catch_levels_ovr_hr & catch_levels_ovr_spr & catch_levels_ovr_stable){
     unlink(file.path(catch_levels_path, "*"), recursive = TRUE) 
  }
  dir.create(catch_levels_path, showWarnings = FALSE)
  default_hr_path <- file.path(catch_levels_path, default_hr_path)
  spr_100_path <- file.path(catch_levels_path, spr_100_path)
  stable_catch_path <- file.path(catch_levels_path, stable_catch_path)
  
  if(catch_levels_ovr_hr | !dir.exists(default_hr_path)){
    run_catch_levels_default_hr(model,
                                default_hr_path = default_hr_path,
                                ...)
  }
  if(catch_levels_ovr_spr | !dir.exists(spr_100_path)){
    run_catch_levels_spr_100(model,
                             spr_100_path = spr_100_path,
                             ...)
  }
  if(catch_levels_ovr_stable | !dir.exists(stable_catch_path)){
    run_catch_levels_stable_catch(model,
                                  stable_catch_path = stable_catch_path,
                                  ...)
  }
}
