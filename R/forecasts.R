#' Run forecasting for the model supplied
#'
#' @details If there is no mcmc component to the model, an error will be given and the program will be stopped
#'
#' @param model The SS model output as loaded by [load_ss_files()]
#' @param forecast_yrs A vector of years to forecast
#' @param forecast_probs A vector of quantiles
#' @param catch_levels The catch levels list as defined in forecast-catch-levels.R
#' @param ss_executable SS executable file name
#'
#' @return [base::invisible()]
#' @export
run_forecasts <- function(model,
                          catch_levels_path,
                          run_forecasts,
                          forecast_yrs,
                          forecast_probs,
                          ss_executable,
                          forecasts_path,
                          ...){

  model_path <- model$path
  mcmc_path <- model$mcmcpath
  forecasts_path <- file.path(model_path, "forecasts")
  if(!run_forecasts & dir.exists(forecasts_path)){
    return(invisible())
  }
  dir.create(forecasts_path, showWarnings = FALSE)
  unlink(file.path(forecasts_path, "*"), recursive = TRUE)

  catch_levels_path <- file.path(model_path, catch_levels_path)

  # Calculate and add on model-custom catch levels

  catch_levels <- fetch_catch_levels(catch_levels_path, ...)

  # Extract the catch level names from the list into a vector
  catch_levels_names <- sapply(catch_levels, "[[", 3)
  # Make the catch level values a matrix where the columns represent the cases in catch.names
  catch_levels <- sapply(catch_levels, "[[", 1)

  message("Running forecasts for model located in ", model_path, "...\n")
  dir.create(forecasts_path, showWarnings = FALSE)

  for(i in 1:length(forecast_yrs)){
    fore_path <- file.path(forecasts_path, paste0("forecast-year-", forecast_yrs[i]))
    dir.create(fore_path, showWarnings = FALSE)
    for(level_ind in 1:ncol(catch_levels)){
      # Create a new sub-directory for each catch projection
      name <- catch_levels_names[level_ind]
      new_forecast_dir <- file.path(fore_path, name)
      dir.create(new_forecast_dir, showWarnings = FALSE)

      # Copy all model files into this new forecast directory
      file.copy(file.path(mcmc_path, list.files(mcmc_path)),
                file.path(new_forecast_dir, list.files(mcmc_path)), copy.mode = TRUE)

      # Insert fixed catches into forecast file (depending on i)
      forecast_file <- file.path(new_forecast_dir, "forecast.ss")
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      fore$Ncatch <- length(forecast_yrs[1:i])
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = catch_levels[,level_ind][1:i])

      SS_writeforecast(fore, dir = new_forecast_dir, overwrite = TRUE, verbose = FALSE)

      # Evaluate the model using mceval option of ADMB, and retrieve the output
      unlink(file.path(new_forecast_dir, "derived_posteriors.sso"), force = TRUE)
      unlink(file.path(new_forecast_dir, "posteriors.sso"), force = TRUE)
      shell_command <- paste0("cd ", new_forecast_dir, " & ", ss_executable, " -mceval")
      shell(shell_command)
    }
  }
  message("Finished running forecasts for model located in ", model$path, "...\n")
  invisible()
}

#' Fetch the output from previously-run forecasting using [run_forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem loading the forecasts, return NA
#'
#' @param model_path Path of the model
#' @param forecasts_path Path of the forecasts
#' @param catch_levels The table of catch levels with same structure as found in forecast-catch-levels.R
#' @param forecast_yrs The forecast years
#' @param ...
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
fetch_forecasts <- function(model_path,
                            forecasts_path,
                            catch_levels,
                            forecast_yrs,
                            ...){

  # Extract the catch level names from the list into a vector
  catch_levels_names <- sapply(catch_levels, "[[", 3)

  # outputs.list <- vector(mode = "list", length = length(catch.levels))
  outputs_list <- vector(mode = "list", length = length(forecast_yrs))
  for(i in 1:length(forecast_yrs)){
    outputs_list[[i]] <- vector(mode = "list", length = length(catch_levels))
  }
  forecasts_path <- file.path(model_path, forecasts_path)
  if(!dir.exists(forecasts_path)){
    return(NA)
  }
  message("\nLoading forecast data from ", forecasts_path)

  # Get the directory listing and choose the last one for loading
  dir_listing <- dir(forecasts_path)

  for(i in 1:length(forecast_yrs)){
    fore_path <- file.path(forecasts_path, paste0("forecast-year-", forecast_yrs[i]))
    # fore.path <- file.path(forecasts.path, dir.listing[length(dir.listing)])
    # Get the directory listing of the last year's forecasts directory and make sure
    #  it matches what the catch levels are.
    dir_listing <- dir(fore_path)
    if(!identical(catch_levels_names, dir_listing)){
      stop("There is a discrepancy between what you have set ",
           "for the catch.levels names \n and what appears in the forecasts directory '",
           fore_path,"'. \n Check the names in both and try again.\n\n", call. = FALSE)
    }
    for(level_ind in 1:length(catch_levels_names)){
      fore_level_path <- file.path(fore_path, catch_levels_names[level_ind])
      message("Loading forecast data from ", fore_level_path)

      mcmc_out <- SSgetMCMC(dir = fore_level_path, writecsv = FALSE, verbose = FALSE)
      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary
      sb <- mcmc_out[,grep("Bratio_",names(mcmc_out))]
      spr <- mcmc_out[,grep("SPRratio_",names(mcmc_out))]

      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      names(sb) <- gsub("Bratio_", "",names(sb))
      names(spr) <- gsub("SPRratio_", "",names(spr))

      # Now, filter out the projected years only
      sb_proj_cols <- sb[,names(sb) %in% forecast_yrs]
      spr_proj_cols <- spr[,names(spr) %in% forecast_yrs]

      outputs_list[[i]][[level_ind]]$biomass <- t(apply(sb_proj_cols, 2, quantile, probs = forecast_probs))
      outputs_list[[i]][[level_ind]]$spr <- t(apply(spr_proj_cols, 2, quantile, probs = forecast_probs))
      outputs_list[[i]][[level_ind]]$mcmccalcs <- calc.mcmc(mcmc_out)
      outputs_list[[i]][[level_ind]]$outputs <- mcmc_out
      names(outputs_list[[i]]) <- catch_levels_names
    }
  }
  names(outputs_list) <- forecast_yrs
  message("Finished loading forecast data")
  outputs_list
}

#' Calculate the probablities of being under several reference points from one forecast year to the next
#'
#' @param forecast_outputs A list as output by [fetch_forecasts()]
#'
#' @return A list of length 1 less than the number of forecast years. Each element
#' is a data.frame of catch levels holding the probabilities. For example, list element 1 will hold the
#'  probabilities for each catch.level of being under several reference points for the first two years
#'  in the forecast_yrs vector. If forecast.outputs is NA, NA will be returned, otherwise the risk.list
#'  will be returned
#' @export
calc_risk <- function(forecast_outputs = NA, ...){

  stopifnot(!is.na(forecast_outputs))

  # Make the catch level values a matrix where the columns represent the cases in catch_names
  catch_levels <- sapply(catch_levels, "[[", 1)

  if(is.na(forecast_outputs)[1]){
    return(NA)
  }

  metric <- function(case_ind, x, yr, yr_ind){
    out <- NULL
    out[1] <- catch_levels[yr_ind, case_ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] < x[, paste0("SSB_", yr)]) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1]) / nrow(x) * 100.0
    ## DFO values
    out[8] <- sum(x[, paste0("SSB_", yr)] > x[, "SSB_MSY"]) / nrow(x) * 100.0
    out[9] <- sum(x[, paste0("SSB_", yr)] > 0.4 * x[, "SSB_MSY"]) / nrow(x) * 100.0
    out[10] <- sum(x[, paste0("SSB_", yr)] > 0.8 * x[, "SSB_MSY"]) / nrow(x) * 100.0
    names(out) <- c(paste0("ForeCatch_", yr),
                    paste0("SSB_", yr + 1, "<SSB_", yr),
                    paste0("Bratio_", yr + 1, "<0.40"),
                    paste0("Bratio_", yr + 1, "<0.25"),
                    paste0("Bratio_", yr + 1, "<0.10"),
                    paste0("SPRratio_", yr, ">1.00"),
                    paste0("ForeCatch_", yr + 1, "<ForeCatch_", yr),
                    ## DFO values
                    paste0("SSB_", yr, ">SSB_MSY"),
                    paste0("SSB_", yr, ">0.4SSB_MSY"),
                    paste0("SSB_", yr, ">0.8SSB_MSY"))

    out
  }
  risk_list <- vector(mode = "list", length = length(forecast_yrs) - 1)
  for(yr in 1:(length(forecast_yrs) - 1)){
    # outputs is a list of one data frame per case, for the current year yr
    outputs <- lapply(forecast_outputs[[yr]], "[[", "outputs")
    # This call calculates the metrics for each element in the list (each catch case)
    #  and binds them together into a data frame. If there was a problem,
    #  (e.g. a bridge model is set up for forecasting) it will be set to NA.
    risk_list[[yr]] <- tryCatch({
      do.call("rbind",
              lapply(1:length(outputs),
                     function(ind, yr, yr_ind){
                       metric(ind, outputs[[ind]], yr, yr_ind)
                     },
                     yr = forecast_yrs[yr],
                     yr_ind = yr))
    }, error = function(e){
      NA
    })
  }
  names(risk_list) <- names(forecast_outputs[1:(length(forecast_outputs) - 1)])

  risk_list
}
