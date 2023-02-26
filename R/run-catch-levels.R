#' Fetch catch levels from SS forecast files. This is used to retrieve ending values
#' after running the [run_catch_levels()] function for default HR, SPR 100, and stable catch
#'
#' @details Assumes [run_catch_levels()] function has been run and the forecast files
#' are populated with 3 forecast years
#'
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param catch_levels_path The path for the catch-levels output
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of 3-element lists of vectors of 3 catch levels corresponding to:
#' a) SPR-100%
#' b) Default harvest policy
#' c) Stable catch
#' Return object looks the same as the `catch_levels` object but with three more elements
#' @export
fetch_catch_levels <- function(catch_levels_path,
                               catch_levels = NULL,
                               ...){
  stopifnot(!is.null(catch_levels))

  message("\nLoading catch levels from ", catch_levels_path)

  spr_100_path <- file.path(catch_levels_path, spr_100_path)
  default_hr_path <- file.path(catch_levels_path, default_hr_path)
  stable_catch_path <- file.path(catch_levels_path, stable_catch_path)

  plan("multisession")
  cust_catch_levels <- future_map(1:3, ~{
    if(.x == 1){
      message("Loading 'SPR 100' catch level from ", spr_100_path)
      forecast_file <- file.path(spr_100_path, forecast_file_name)
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)

      tryCatch({
        col_catch <- fore$ForeCatch %>% select(`Catch or F`)
      }, error = function(e){
        stop("The column 'Catch or F' was not found in the forecast file Catch matrix.")
      })
      col_catch
    }else if(.x == 2){
      message("Loading 'Default HR' catch level from ", default_hr_path)
      forecast_file <- file.path(default_hr_path, "forecast.ss")
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      tryCatch({
        col_catch <- fore$ForeCatch %>% select(`Catch or F`)
      }, error = function(e){
        stop("The column 'Catch or F' was not found in the forecast file Catch matrix.")
      })
      col_catch
    }else{
      message("Loading 'Stable Catch' catch level from ", stable_catch_path)
      forecast_file <- file.path(stable_catch_path, "forecast.ss")
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      tryCatch({
        col_catch <- fore$ForeCatch %>% select(`Catch or F`)
      }, error = function(e){
        stop("The column 'Catch or F' was not found in the forecast file Catch matrix.")
      })
      message("Finished loading catch levels")
      col_catch
    }
  })
  plan()
  # Replace the NA values for the custom catch levels with the values read in
  inds <- (length(catch_levels) - length(cust_catch_levels) + 1):length(catch_levels)
  map2(inds, 1:length(cust_catch_levels), ~{
    catch_levels[[.x]][[1]] <<- cust_catch_levels[[.y]] %>% pull()
  }, furrr_options(globals = c("pull")))
  catch_levels
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param default_hr_path The path in which the default hr forecast resides
#' @param forecast_yrs A vector of forecast years
#' @param ... Absorbs arguments intended for other functions
#'
#' @export
run_catch_levels_default_hr <- function(model,
                                        default_hr_path,
                                        forecast_yrs,
                                        ...){

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(default_hr_path,
                      files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists, model$extra_mcmc_path, model$mcmc_path),
                      derposts_file_name),
            file.path(default_hr_path,
                      derposts_file_name))

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

    # Make a modification to the starter file so the extra MCMC files are not created
    modify_starter_mcmc_type(default_hr_path, 1)

    shell_command <- paste0("cd ", default_hr_path, " && ", ss_executable, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
  }
}

#' Run the model iteratively zoning in on a catch value that reduces the SPR to 1, within the tolerance given
#' (`catch_levels_spr_tol`)
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param spr_100_path Path where the SPR model files are found
#' @param forecast_yrs A vector if the years to forecast for
#' @param catch_levels_spr_tol The tolerance to be within 1 for the SPR
#' @param catch_levels_catch_tol Catch tolerance. If the upper and lower catch in the algorithm are within this,
#' it is assumed that the SPR is close enough
#' @param catch_levels_max_iter The maximum number of iterations to do
#' @param ... Not used
#' @export
run_catch_levels_spr_100 <- function(model,
                                     spr_100_path,
                                     forecast_yrs,
                                     catch_levels_spr_tol,
                                     catch_levels_catch_tol,
                                     catch_levels_max_iter,
                                     ...){

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(spr_100_path,
                      files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists, model$extra_mcmc_path, model$mcmc_path),
                      derposts_file_name),
            file.path(spr_100_path,
                      derposts_file_name))

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

      # Make a modification to the starter file so the extra MCMC files are not created
      modify_starter_mcmc_type(spr_100_path, 1)

      shell_command <- paste0("cd ", spr_100_path, " && ", ss_executable, " -mceval")
      system_(shell_command, wait = TRUE, intern = !show_ss_output)
      out <- read.table(file.path(spr_100_path,
                                  derposts_file_name),
                        header = TRUE)
      spr <- median(as.numeric(out[paste0("SPRratio_", forecast_yrs[i])][[1]]))
      message("SPR 100, for forecast year: ", forecast_yrs[i], " of ", tail(forecast_yrs, 1))
      message("SPR difference from 1: ", abs(spr - 1), " < ", catch_levels_spr_tol, " ? ",
              ifelse(abs(spr - 1) < catch_levels_spr_tol, "Yes", "No"))
      message("Upper catch: ", upper, " - Lower catch: ", lower,
              ". Difference: ", abs(upper - lower), " < ", catch_levels_catch_tol, " ? ",
              ifelse(abs(upper - lower) < catch_levels_catch_tol, "Yes\n", "No\n"))

      if(abs(spr - 1) < catch_levels_spr_tol |
         abs(upper - lower) < catch_levels_catch_tol){
        # Sometimes, upper and lower can end up close to equal,
        #  but the tolerance is still not met. In this case, assume
        #  the catch creates an SPR of 100% even though it is slightly off.
        break
      }
      if(iter == catch_levels_max_iter){
        warning("The maximum number of iterations (", catch_levels_max_iter,") was reached for forecast year ", forecast_yrs[i],
                ". The SPR difference in the last iteration was ", spr - 1, "\n")
        break
      }

      if(spr - 1 > 0){
        upper <- spr_100_catch[i]
        lower <- (upper + lower) / 2.0
        spr_100_catch[i] <- lower
        message("spr greater than 1, upper set to ", upper, ", lower set to ", lower,"\n")
      }else if(spr - 1 < 0){
        lower <- spr_100_catch[i]
        upper <- upper * 1.5
        spr_100_catch[i] <- upper
        message("spr less than 1, upper set to ", upper, ", lower set to ", lower,"\n")
      }else{
        message("spr exactly equal to 1, breaking\n")
        break
      }
      iter <- iter + 1
    }
  }
}

#' Run the model iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param stable_catch_path The path of the stable catch scenario
#' @param forecast_yrs A vector of forecast years
#' @param catch_levels_catch_tol The tolerance for stopping based on catch
#' difference
#' @param catch_levels_max_iter The maximum number of iterations
#' @param ... Absorbs other arguments intended for other functions
#'
#' @export
run_catch_levels_stable_catch <- function(model,
                                          stable_catch_path,
                                          forecast_yrs,
                                          catch_levels_catch_tol,
                                          catch_levels_max_iter,
                                          ...){

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(stable_catch_path,
                      files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists, model$extra_mcmc_path, model$mcmc_path),
                      derposts_file_name),
            file.path(stable_catch_path,
                      derposts_file_name))

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
    stable_catch[4] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast_yrs[4])][[1]]))

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

    # Make a modification to the starter file so the extra MCMC files are not created
    modify_starter_mcmc_type(stable_catch_path, 1)

    shell_command <- paste0("cd ", stable_catch_path, " && ", ss_executable, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
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
  shell_command <- paste0("cd ", stable_catch_path, " && ", ss_executable, " -mceval")
  system_(shell_command, wait = FALSE, intern = !show_ss_output)
}

#' A wrapper to run the catch levels determination routines
#'
#' @param model_path The model directory name
#' @param catch_levels_path The catch levels list, which is a list of lists
#' of length 3
#' @param ... Absorbs arguments intended for other functions
#'
#' @return [base::invisible()]
#' @export
run_catch_levels <- function(model_path,
                             catch_levels_path = catch_levels_path,
                             ...){

  model <- load_ss_files(model_path)

  catch_levels_path <- file.path(model_path, catch_levels_path)
  dir.create(catch_levels_path, showWarnings = FALSE)
  unlink(file.path(catch_levels_path, "*"), recursive = TRUE)

  plan("multisession")
  future_map(1:3, function(x = .x,
                           catch_levels_path = catch_levels_path,
                           default_hr_path = default_hr_path,
                           spr_100_path = spr_100_path,
                           stable_catch_path = stable_catch_path,
                           forecast_yrs = forecast_yrs,
                           catch_levels_spr_tol = catch_levels_spr_tol,
                           catch_levels_catch_tol = catch_levels_catch_tol,
                           catch_levels_max_iter = catch_levels_max_iter){
    if(x == 1){
      model <- load_ss_files(model_path)
      default_hr_path <- file.path(catch_levels_path, default_hr_path)
      dir.create(default_hr_path, showWarnings = FALSE)
      run_catch_levels_default_hr(model,
                                  default_hr_path,
                                  forecast_yrs)
    }else if(x == 2){
      model <- load_ss_files(model_path)
      spr_100_path <- file.path(catch_levels_path, spr_100_path)
      dir.create(spr_100_path, showWarnings = FALSE)
      run_catch_levels_spr_100(model,
                               spr_100_path,
                               forecast_yrs,
                               catch_levels_spr_tol,
                               catch_levels_catch_tol,
                               catch_levels_max_iter)
    }else{
      model <- load_ss_files(model_path)
      stable_catch_path <- file.path(catch_levels_path, stable_catch_path)
      dir.create(stable_catch_path, showWarnings = FALSE)
      run_catch_levels_stable_catch(model,
                                    stable_catch_path,
                                    forecast_yrs,
                                    catch_levels_catch_tol,
                                    catch_levels_max_iter)
    }
  },
  catch_levels_path = catch_levels_path,
  default_hr_path = default_hr_path,
  spr_100_path = spr_100_path,
  stable_catch_path = stable_catch_path,
  forecast_yrs = forecast_yrs,
  catch_levels_spr_tol = catch_levels_spr_tol,
  catch_levels_catch_tol = catch_levels_catch_tol,
  catch_levels_max_iter = catch_levels_max_iter,
  .options = furrr_options(globals = c(f = f,
                                       load_ss_files = load_ss_files,
                                       SS_output = SS_output,
                                       fix.posteriors = fix.posteriors,
                                       posts_file_name = posts_file_name,
                                       derposts_file_name = derposts_file_name,
                                       create.key.nuisance_posteriors_files = create.key.nuisance_posteriors_files,
                                       calc.mcmc = calc.mcmc,
                                       get_os = get_os,
                                       strip.columns = strip.columns,
                                       run_catch_levels_default_hr = run_catch_levels_default_hr,
                                       run_catch_levels_spr_100 = run_catch_levels_spr_100,
                                       run_catch_levels_stable_catch = run_catch_levels_stable_catch,
                                       latex_bold = latex_bold,
                                       forecast_file_name = forecast_file_name,
                                       ss_executable = ss_executable,
                                       show_ss_output = show_ss_output,
                                       starter_file_name = starter_file_name,
                                       modify_starter_mcmc_type = modify_starter_mcmc_type,
                                       system_ = system_)))
  plan()
}
