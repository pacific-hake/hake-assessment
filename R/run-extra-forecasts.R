#' Run extra forecasting for the model supplied
#'
#' @details
#' You must modify the catch-levels list inside this function prior
#' to running. Start withthe directory number after what is already
#' in the base model `forecasts` directory.
#'
#' @details If there is no mcmc component to the model, an error will be
#' given and the program will be stopped
#'
#' @param model The SS model output as loaded by [load_ss_files()]
#'
#' @return [base::invisible()]
#' @export
run_extra_forecasts <- function(
    model_path,
    forecast_yrs,
    catch_levels =
      list(list(rep(470000, length(forecast_yrs)), "470,000 t", "16-470000"),
           list(rep(510000, length(forecast_yrs)), "510,000 t", "17-510000"),
           list(rep(600000, length(forecast_yrs)), "600,000 t", "18-600000"),
           list(rep(650000, length(forecast_yrs)), "650,000 t", "19-650000")),
    ...){

  model <- load_ss_files(model_path, ...)
  forecasts_path <- file.path(model_path, forecasts_path)
  dir.create(forecasts_path, showWarnings = FALSE)

  message("Running forecasts for model located in ", model_path, "\n")
  plan("multisession")
  map(forecast_yrs, ~{
    # In this outer loop .x is the forecast year
    fore_path <- file.path(forecasts_path, paste0("forecast-year-", .x))
    dir.create(fore_path, showWarnings = FALSE)
    future_map2(catch_levels, .x, ~{
      # In this inner loop .y is the forecast year and .x is the list element of catch_levels
      name <- .x[[3]]
      catch_ind <- which(forecast_yrs == .y)
      new_forecast_dir <- file.path(fore_path, name)
      dir.create(new_forecast_dir, showWarnings = FALSE)

      # Copy all model files into this new forecast directory
      file.copy(list.files(model$mcmc_path, full.names = TRUE),
                file.path(new_forecast_dir, list.files(model$mcmc_path)), copy.mode = TRUE)

      # Make a modification to the starter file so the extra MCMC files are not created
      modify_starter_mcmc_type(new_forecast_dir, 1)

      # Insert fixed catches into forecast file
      forecast_file <- file.path(new_forecast_dir, forecast_file_name)
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      fore$Ncatch <- length(forecast_yrs[1:catch_ind])
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:catch_ind],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = .x[[1]][1:catch_ind])

      SS_writeforecast(fore, dir = new_forecast_dir, overwrite = TRUE, verbose = FALSE)

      # Evaluate the model using mceval option of ADMB, and retrieve the output
      unlink(file.path(new_forecast_dir, derposts_file_name), force = TRUE)
      unlink(file.path(new_forecast_dir, posts_file_name), force = TRUE)
      unlink(file.path(new_forecast_dir, "sso"), force = TRUE)
      shell_command <- paste0("cd ", new_forecast_dir, " && ", ss_executable, " -mceval")
      system_(shell_command, wait = TRUE, intern = !show_ss_output)
    })
  })
  plan()
  message("Finished running forecasts for model located in ", model$path, "\n")
}
