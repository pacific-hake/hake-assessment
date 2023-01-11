#' Run forecasting for the model supplied
#'
#' @details If there is no mcmc component to the model, an error will be given and the program will be stopped
#'
#' @param model The SS model output as loaded by [load_ss_files()]
#'
#' @return [base::invisible()]
#' @export
run_forecasts <- function(model_path,
                          catch_levels_path,
                          ...){

  model <- load_ss_files(model_path, ...)
  forecasts_path <- file.path(model_path, forecasts_path)
  dir.create(forecasts_path, showWarnings = FALSE)
  unlink(file.path(forecasts_path, "*"), recursive = TRUE)

  catch_levels_path <- file.path(model_path, catch_levels_path)

  # Calculate and add on model-custom catch levels
  catch_levels <- fetch_catch_levels(catch_levels_path, ...)

  message("Running forecasts for model located in ", model_path, "\n")
  dir.create(forecasts_path, showWarnings = FALSE)
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

#' Fetch the output from previously-run forecasting using [run_forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem loading the forecasts, return NA
#'
#' @param model_path Path of the model
#' @param forecasts_path Path of the forecasts
#' @param my_catch_levels The table of catch levels with same structure as found in forecast-catch-levels.R
#' @param forecast_yrs The forecast years
#' @param ...
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
fetch_forecasts <- function(model_path,
                            my_catch_levels,
                            ...){

  # Extract the catch level names from the list into a vector
  catch_levels_names <- map_chr(my_catch_levels, ~{.x[[3]]})

  forecasts_path <- file.path(model_path, forecasts_path)
  if(!dir.exists(forecasts_path)){
    return(NA)
  }
  message("\nLoading forecast data from ", forecasts_path)

  # Get the directory listing and choose the last one for loading
  dir_listing <- dir(forecasts_path)

  plan("multisession")
  lst <- map(forecast_yrs, ~{
    fore_path <- file.path(forecasts_path, paste0("forecast-year-", .x))
    # Get the directory listing of the last year's forecasts directory and make sure
    #  it matches what the catch levels are.
    dir_listing <- dir(fore_path)
    if(!identical(catch_levels_names, dir_listing)){
      stop("There is a discrepancy between what you have set ",
           "for the catch_levels_names \n and what appears in the forecasts directory '",
           fore_path,"'. \n Check the names in both and try again.\n\n", call. = FALSE)
    }
    lvls_lst <- future_map2(my_catch_levels, .x, ~{
      fore_level_path <- file.path(fore_path, .x[[3]])
      message("Loading forecast data from ", fore_level_path)

      mcmc_out <- SSgetMCMC(dir = fore_level_path, writecsv = FALSE, verbose = FALSE)
      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary

      sb <- mcmc_out %>% select(grep("Bratio_", names(.)))
      spr <- mcmc_out %>% select(grep("SPRratio_", names(.)))

      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      names(sb) <- gsub("Bratio_", "", names(sb))
      names(spr) <- gsub("SPRratio_", "", names(spr))

      # Now, filter out the projected years only
      sb_proj_cols <- sb %>% select(one_of(as.character(forecast_yrs)))
      spr_proj_cols <- spr %>% select(one_of(as.character(forecast_yrs)))
      sb_proj_cols <- na.omit(sb_proj_cols)
      spr_proj_cols <- na.omit(spr_proj_cols)
      list(biomass = t(apply(sb_proj_cols, 2, quantile, probs = forecast_probs, na.rm = TRUE)),
           spr = t(apply(spr_proj_cols, 2, quantile, probs = forecast_probs, na.rm = TRUE)),
           mcmccalcs = calc.mcmc(mcmc_out),
           outputs = mcmc_out)
    }, .options = furrr_options(globals = c("f",
                                            "calc.mcmc",
                                            "forecast_yrs",
                                            "forecast_probs",
                                            "latex.bold",
                                            "select",
                                            "strip.columns",
                                            "SSgetMCMC")))
    names(lvls_lst) <- catch_levels_names
    lvls_lst
  })
  plan()
  names(lst) <- forecast_yrs
  message("Finished loading forecasts")
  lst
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
calc_risk <- function(forecast_outputs = NA,
                      catch_levels,
                      ...){

  stopifnot(!is.na(forecast_outputs))

  # Make the catch level values a matrix where the columns represent the cases in catch_names
  catch_levels <- sapply(catch_levels, "[[", 1)

  if(is.na(forecast_outputs)[1]){
    return(NA)
  }

  metric <- function(case_ind, x, yr, yr_ind){
    out <- NULL
    out[1] <- catch_levels[yr_ind, case_ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] < x[, paste0("SSB_", yr)], na.rm = TRUE) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40, na.rm = TRUE) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25, na.rm = TRUE) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10, na.rm = TRUE) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00, na.rm = TRUE) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1], na.rm = TRUE) / nrow(x) * 100.0
    ## DFO values
    out[8] <- sum(x[, paste0("SSB_", yr)] > x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[9] <- sum(x[, paste0("SSB_", yr)] > 0.4 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[10] <- sum(x[, paste0("SSB_", yr)] > 0.8 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
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
    risk_list[[yr]] <-
       do.call("rbind",
              lapply(1:length(outputs),
                     function(ind, yr, yr_ind){
                       metric(ind, outputs[[ind]], yr, yr_ind)
                     },
                     yr = forecast_yrs[yr],
                     yr_ind = yr))
  }
  names(risk_list) <- names(forecast_outputs[1:(length(forecast_outputs) - 1)])

  risk_list
}
