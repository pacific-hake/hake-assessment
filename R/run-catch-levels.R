#' Fetch catch levels from SS forecast files. This is used to retrieve ending values
#' after running the [run_catch_levels()] function for default HR, SPR 100, and stable catch
#'
#' @details Assumes [run_catch_levels()] function has been run and the forecast files
#' are populated with 3 forecast years
#'
#' @param model The SS model output as loaded by [load_ss_files()]
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
#' Return object looks the same as the `catch_levels` object but with three more elements
#' @export
fetch_catch_levels <- function(catch_levels_path,
                               catch_levels = NULL){

  stopifnot(!is.null(catch_levels))

  message("\nLoading catch level data from ", catch_levels_path)

  spr_100_path <- file.path(catch_levels_path, spr_100_path)
  message("Loading 'SPR 100' catch level data from ", spr_100_path)
  forecast_file <- file.path(spr_100_path, forecast_file_name)
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)

  ind <- length(catch_levels) - 2
  colCatch <- grep("Catch.or.F", colnames(fore$ForeCatch), perl = TRUE)
  if (length(colCatch) != 1) {
    stop("The column 'Catch or F' was not found in the forecast file Catch matrix.")
  }

  catch_levels[[ind]][[1]] <- fore$ForeCatch[, colCatch]

  default_hr_path <- file.path(catch_levels_path, default_hr_path)
  message("Loading 'Default HR' catch level data from ", default_hr_path)
  forecast_file <- file.path(default_hr_path, "forecast.ss")
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  ind <- ind + 1
  catch_levels[[ind]][[1]] <- fore$ForeCatch[, colCatch]

  stable_catch_path <- file.path(catch_levels_path, stable_catch_path)
  message("Loading 'Stable Catch' catch level data from ", stable_catch_path)
  forecast_file <- file.path(stable_catch_path, "forecast.ss")
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  ind <- ind + 1
  catch_levels[[ind]][[1]] <- fore$ForeCatch[, colCatch]
  message("Successfully loaded catch level data.")

  catch_levels
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load_ss_files()]
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param ...
#'
#' @export
run_catch_levels_default_hr <- function(model,
                                        default_hr_path,
                                        catch_levels,
                                        ...){

  mcmc_path <- model$mcmcpath
  file.copy(file.path(mcmc_path,
                      list.files(mcmc_path)),
            file.path(default_hr_path,
                      list.files(mcmc_path)),
            copy.mode = TRUE)

  forecast_file <- file.path(default_hr_path, forecast_file_name)
  default_hr_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    out <- read.table(file.path(default_hr_path,
                                derposts_file_name),
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
    unlink(file.path(default_hr_path, derposts_file_name),
           force = TRUE)
    message("Default HR - for forecast year: ", forecast_yrs[i], " of ", tail(forecast_yrs, 1))

    shell_command <- paste0("cd ", default_hr_path, " & ", ss_executable, " -mceval")
    shell(shell_command, wait = TRUE, intern = !show_ss_output)
  }
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load_ss_files()]
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#'
#' @export
run_catch_levels_spr_100 <- function(model,
                                     spr_100_path,
                                     catch_levels,
                                     ...){

  mcmc_path <- model$mcmcpath
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
      unlink(file.path(spr_100_path, derposts_file_name),
             force = TRUE)
      shell_command <- paste0("cd ", spr_100_path, " & ", ss_executable, " -mceval")
      shell(shell_command, wait = TRUE, intern = !show_ss_output)
      out <- read.table(file.path(spr_100_path,
                                  derposts_file_name),
                        header = TRUE)
      spr <- median(as.numeric(out[paste0("SPRratio_", forecast_yrs[i])][[1]]))
      message("SPR 100, for forecast year: ", forecast_yrs[i], " of ", tail(forecast_yrs, 1))
      message("SPR difference from 1: ", abs(spr - 1), " < ", catch_levels_spr_tol, " ? ",
              ifelse(abs(spr - 1) < catch_levels_spr_tol, "Yes", "No"))
      message("Upper catch: ", upper, " - Lower catch: ", lower,
              ". Difference: ", abs(upper - lower), " < ", catch_levels_catch_tol, " ? ",
              ifelse(abs(upper - lower) < catch_levels_catch_tol, "Yes", "No"))
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
#' @param model The SS model output as loaded by [load_ss_files()]
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#'
#' @export
run_catch_levels_stable_catch <- function(model,
                                          stable_catch_path,
                                          catch_levels,
                                          ...){

  mcmc_path <- model$mcmcpath
  file.copy(file.path(mcmc_path,
                      list.files(mcmc_path)),
            file.path(stable_catch_path,
                      list.files(mcmc_path)),
            copy.mode = TRUE)

  forecast_file <- file.path(stable_catch_path, forecast_file_name)
  stable_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  out <- read.table(file.path(stable_catch_path,
                              derposts_file_name),
                    header = TRUE)
  iter <- 1
  repeat{
    out <- read.table(file.path(stable_catch_path,
                                derposts_file_name),
                      header = TRUE)
    stable_catch[1] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[1])][[1]]))
    stable_catch[2] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[2])][[1]]))
    stable_catch[3] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[3])][[1]]))
    message("Stable Catch: ")
    message("Catch difference from forecast year 1 to 2: ", abs(stable_catch[1] - stable_catch[2]),
            " < ", catch_levels_catch_tol, " ? ",
            ifelse(abs(stable_catch[1] - stable_catch[2]) < catch_levels_catch_tol, "Yes", "No"))
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
    unlink(file.path(stable_catch_path, derposts_file_name),
           force = TRUE)
    shell_command <- paste0("cd ", stable_catch_path, " & ", ss_executable, " -mceval")
    shell(shell_command, wait = TRUE, intern = !show_ss_output)
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
  unlink(file.path(stable_catch_path, derposts_file_name),
         force = TRUE)
  shell_command <- paste0("cd ", stable_catch_path, " & ", ss_executable, " -mceval")
  shell(shell_command, wait = FALSE, intern = !show_ss_output)
}

#' A wrapper to run the catch levels determination routines
#'
#' @param model_path The model directory name
#' @param ... Passed to [run_catch_levels_default_hr()], [run_catch_levels_spr_100()],
#' and [run_catch_levels_stable_catch()]
#'
#' @return [base::invisible()]
#' @export
run_catch_levels <- function(model_path, ...){

  model <- load_ss_files(model_path)

  catch_levels_path <- file.path(model_path, catch_levels_path)
  dir.create(catch_levels_path, showWarnings = FALSE)
  unlink(file.path(catch_levels_path, "*"), recursive = TRUE)
  default_hr_path <- file.path(catch_levels_path, default_hr_path)
  dir.create(default_hr_path, showWarnings = FALSE)
  spr_100_path <- file.path(catch_levels_path, spr_100_path)
  dir.create(spr_100_path, showWarnings = FALSE)
  stable_catch_path <- file.path(catch_levels_path, stable_catch_path)
  dir.create(stable_catch_path, showWarnings = FALSE)

  plan("multisession")
  future_map(1:3, ~{
    if(.x == 1){
      run_catch_levels_default_hr(model,
                                  default_hr_path,
                                  ...)
    }else if(.x == 2){
      run_catch_levels_spr_100(model,
                               spr_100_path,
                               ...)
    }else{
      run_catch_levels_stable_catch(model,
                                    stable_catch_path,
                                    ...)
    }
  },
  ...)
  plan()
}
