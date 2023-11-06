#' Run the model iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param stable_catch_path The path of the stable catch scenario
#' @param forecast_yrs A vector of forecast years
#' @param ct_levels_catch_tol The tolerance for stopping based on catch
#' difference
#' @param ct_levels_max_iter The maximum number of iterations
#' @param ... Absorbs other arguments intended for other functions
#'
#' @export
run_ct_levels_stable_catch <- function(model,
                                       stable_catch_path,
                                       forecast_yrs,
                                       ct_levels_catch_tol,
                                       ct_levels_max_iter,
                                       ...){

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(stable_catch_path,
                      files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists,
                             model$extra_mcmc_path,
                             model$mcmc_path),
                      derposts_fn),
            file.path(stable_catch_path,
                      derposts_fn))

  forecast_file <- file.path(stable_catch_path, forecast_fn)
  stable_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  out <- read.table(file.path(stable_catch_path,
                              derposts_fn),
                    header = TRUE)
  iter <- 1
  repeat{
    out <- read.table(file.path(stable_catch_path,
                                derposts_fn),
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
    catch_diff <- abs(stable_catch[1] - stable_catch[2])
    message("Catch difference from forecast year 1 to 2: ",
            catch_diff,
            " < ", ct_levels_catch_tol, " ? ",
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
    fore <- SS_readforecast(forecast_file,
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
                     dir = stable_catch_path,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(stable_catch_path, derposts_fn),
           force = TRUE)

    # Make a modification to the starter file so the extra MCMC files are
    # not created
    modify_starter_mcmc_type(stable_catch_path, 1)

    shell_command <- paste0("cd ", stable_catch_path, " && ",
                            ss_executable, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
    iter <- iter + 1
  }
  fore <- SS_readforecast(forecast_file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  fore$Ncatch <- length(forecast_yrs[1:length(forecast_yrs)])
  fore$ForeCatch <- data.frame(
    Year = forecast_yrs[1:length(forecast_yrs)],
    Seas = 1,
    Fleet = 1,
    Catch_or_F = stable_catch[1:length(forecast_yrs)])
  SS_writeforecast(fore,
                   dir = stable_catch_path,
                   overwrite = TRUE,
                   verbose = FALSE)
  unlink(file.path(stable_catch_path, derposts_fn),
         force = TRUE)
  shell_command <- paste0("cd ", stable_catch_path, " && ",
                          ss_executable, " -mceval")
  system_(shell_command, wait = FALSE, intern = !show_ss_output)
}
