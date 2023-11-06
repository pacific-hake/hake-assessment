#' Run the model iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector of forecast years
#' @param ... Absorbs arguments intended for other functions
#'
#' @export
run_ct_levels_default_hr <- function(model,
                                     forecast_yrs,
                                     ...){

  pth <- here::here(model$path, ct_levels_path, default_hr_path)

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(pth, files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists,
                             model$extra_mcmc_path,
                             model$mcmc_path),
                      derposts_fn),
            file.path(pth, derposts_fn))

  forecast_file <- file.path(pth, forecast_fn)
  default_hr_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    out <- read.table(file.path(pth, derposts_fn), header = TRUE)
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
                     dir = pth,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(pth, derposts_fn), force = TRUE)
    message("Default HR - for forecast year: ",
            forecast_yrs[i], " of ",
            tail(forecast_yrs, 1))

    # Make a modification to the starter file so the extra MCMC files are
    # not created
    modify_starter_mcmc_type(pth, 1)

    shell_command <- paste0("cd ", pth, " && ",
                            ss_executable, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
  }
}
