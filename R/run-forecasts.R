#' Run forecasting for a model in parallel when possible
#'
#' @param model The SS3 model output as loaded by [create_rds_file()]
#' @param model_path The directory containing the model
#' @param forecast_yrs A vector of forecast years
#' @param ss_exe The name of the SS3 executable. If run standalone,
#' this will be [ss_executable]. If run from the context of of the [bookdown]
#' document, this will be set as a YAML key/tag
#' @param ... Arguments passed to [fetch_ct_levels()]
#'
#' @details
#' The function will check to see if you can run the forecasting in parallel.
#' If you are on Windows, or calling the code from an Rstudio console,
#' you will be asked if you want to continue in sequential mode. To run in
#' parallel (multicore) mode, you must be on a Mac or Linux machine, and NOT
#' using Rstudio. You must also call this function from Rscript or the
#' terminal R program. You can also use a bash script containing
#' `Rscript - e "R code here"`. See the scripts located in the `bash-scripts`
#' directory
#'
#' @return [base::invisible()]
#' @export
run_forecasts <- function(model = NULL,
                          model_path = NULL,
                          forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                          ss_exe = NULL,
                          ...){

  # First, check if the computer/terminal combination of capable of running
  # the code in parallel. If only sequential is possible, ask if caller wants
  # to continue, because it takes a long time to run sequentially.
  supports_multicore <- supportsMulticore()
  if(!supports_multicore){
    message(paste0("`run_forecasts`: ", parallelism_warning))
    if(interactive()){
      continue_ans <- readline("Do you want to continue (y/n)? ")
      continue_ans <- substr(tolower(continue_ans), 1, 1)
      if(continue_ans == "n"){
        stop_quietly("Halted program at user request, without any changes ",
                     "to\ndirectories/files and without any forecasting runs ",
                     "done\n")
      }
    }
  }

  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
    }
    message("Loading model output into R list for model located at:\n",
            model_path)
    model <- load_ss_files(model_path, ...)
  }else{
    if(is.null(model_path)){
      model_path <- model$path
    }else{
      if(model$path != model_path){
        stop("You provided both `model` and `model_path` and `model$path` ",
             "does not math `model_path`")
      }
    }
  }

  if(!check_catch_levels(model_path)){
    message("`run_forecasts()`: The `", file.path(model_path, ct_levels_path),
            "` directory does not appear to be in the correct format. ",
            "Either one of the subdirectories does not exist, or the `",
            forecast_fn, "` files inside the subdirectories do not ",
            "contain the right number of forecast years.\n\nRunning ",
            "`run_ct_levels()` before forecasting.")
    run_ct_levels(model,
                  forecast_yrs = forecast_yrs,
                  ss_exe = ss_exe,
                  ... )
  }

  if(is.null(model$mcmc_path)){
    stop("`model$mcmc_path` is `NULL`")
  }
  if(!dir.exists(model$mcmc_path)){
    stop("Directory `", model$mcmc_path, "` does not exist")
  }

  pth <- file.path(model_path, forecasts_path)
  dir.create(pth, showWarnings = FALSE)
  file_chmod(pth, output_permissions)
  unlink(file.path(pth, "*"), recursive = TRUE)

  ct_levels_fullpath <- file.path(model_path, ct_levels_path)

  # Calculate and add on model-custom catch levels
  ct_levels <- load_ct_levels(model_path, ...)

  message("Running forecasts for model located in ", model_path, "\n")

  walk(forecast_yrs, \(yr){

    fore_path <- file.path(pth, paste0(forecasts_prepend, yr))
    dir.create(fore_path, showWarnings = FALSE)
    file_chmod(fore_path, output_permissions)

    if(supports_multicore){
      # One worker for each catch stream
      plan("multicore", workers = length(ct_levels$ct_levels))
    }else{
      plan("sequential")
    }

    future_walk(ct_levels$ct_levels, \(ct_level, year = yr){

      nm <- ct_level[[3]]
      catch_ind <- which(forecast_yrs == year)
      new_forecast_dir <- file.path(fore_path, nm)
      dir.create(new_forecast_dir, showWarnings = FALSE)
      file_chmod(new_forecast_dir, output_permissions)

      # Copy all model files into this new forecast directory
      # Using setdiff is the only guaranteed way you can disallow directories
      # in the file list.
      src_fns_nodirs <- setdiff(list.files(model$mcmc_path,
                                           full.names = FALSE),
                                list.dirs(model$mcmc_path,
                                          full.names = FALSE))
      src_fns <- file.path(model$mcmc_path, src_fns_nodirs)
      dest_fns <- file.path(new_forecast_dir, src_fns_nodirs)

      copy_flag <- file.copy(src_fns,
                             dest_fns,
                             overwrite = TRUE,
                             copy.mode = TRUE)

      if(!all(copy_flag)){
        stop("At least one MCMC file failed to copy from directory\n`",
             model$mcmc_path, "` to directory\n`",
             new_forecast_dir, "`.\nThe file(s) not copied are:\n",
             paste(src_fns[!copy_flag], collapse = "\n"))
      }

      # Make a modification to the starter file so the extra MCMC files
      #  are not created
      modify_starter_mcmc_type(new_forecast_dir, 1)

      # Insert fixed catches into forecast file
      forecast_file <- file.path(new_forecast_dir, forecast_fn)
      fore <- SS_readforecast(forecast_file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      fore$Ncatch <- length(forecast_yrs[1:catch_ind])
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:catch_ind],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = ct_level[[1]][1:catch_ind])

      SS_writeforecast(fore,
                       dir = new_forecast_dir,
                       overwrite = TRUE,
                       verbose = FALSE)

      # Evaluate the model using -mceval option of ADMB, and retrieve the output
      unlink(file.path(new_forecast_dir, derposts_fn), force = TRUE)
      unlink(file.path(new_forecast_dir, posts_fn), force = TRUE)
      unlink(file.path(new_forecast_dir, "sso"), force = TRUE)
      shell_command <- paste0("cd ", new_forecast_dir, " && ",
                              ss_executable, " -mceval")
      system_(shell_command, wait = TRUE, intern = !show_ss_output)
    })
  })

  message("Finished running forecasts for model located in ", model$path, "\n")
}
