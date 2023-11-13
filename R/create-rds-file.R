#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_path Directory name of model to be loaded
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param overwrite Logical. If `TRUE`, overwrite the file if it exists
#' @param keep_index_fit_posts Logical. If `TRUE`, keep the `index_fit_posts`
#' data in the model's list. This contains the survey fit output for every
#' posterior`and is quite large. Typically kept for base model only. Needed
#' for the [plot_survey_fit_mcmc()] plot to work.
#' @param ... Arguments to pass to [load_ss_files()]
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model = NULL,
                            model_path = NULL,
                            keep_index_fit_posts = FALSE,
                            verbose = TRUE,
                            overwrite = TRUE,
                            ...){

  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
    }
    if(verbose){
      message("`create_rds_file()`: Loading SS3 model input and output ",
              "files in:\n`", model_path, "`\n")
    }
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

  if(length(grep("\\/$", model_path))){
    # Remove trailing slashes
    model_path <- gsub("\\/+$", "", model_path)
  }

  if(!dir.exists(model_path)){
    stop("Model directory `", model_path, "` does not exist")
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_path, paste0(basename(model_path), ".rds"))
  if(file.exists(rds_file)){
   if(!overwrite){
     stop_quietly("The RDS file `", rds_file,"` exists but you did not ",
                  "set `overwrite = TRUE`, so nothing was done by the ",
                  "`create_rds_file()` function")
   }
  }

  if(verbose){
    message("`create_rds_file()`: Creating a new RDS file from SS3 ",
            "model output.")
  }

  # Add values to be used in the document that require the MCMC data frame to
  # calculate, because the MCMC data frame will not be stored in the RDS file
  # due to its size
  model$mcmcvals <- load_mcmc_vals(model,
                                   model$dat$endyr + 1)

  # Add prior and posterior extractions/calculations/formatting
  model$parameter_priors <- get_prior_data(model, ...)
  model$parameter_posts <- get_posterior_data(model, ...)

  # Try loading extra mcmc output. If none are found or there is a problem,
  # model$extra_mcmc will be NA
  if(verbose){
    message("`create_rds_file()`: Loading extra MCMC output for model in:\n",
            "`", model$extra_mcmc_path, "`\n")
  }
  # Add values extracted from the extra MCMC output which includes index
  # estimates, catchability estimates, and at-age data frames
  model$extra_mcmc <- load_extra_mcmc(model,
                                      verbose = verbose,
                                      ...)

  # Load forecasts. If none are found or there is a problem,
  # `model$forecasts` will be `NA`
  model$catch_levels <- NA
  model$catch_default_policy <- NA

  ct_levels_fullpath <- file.path(model_path, ct_levels_path)
  if(dir.exists(ct_levels_fullpath)){
    # Load in the missing `default HR`, `SPR 100`, and `stable catch` values
    # by passing `ct_levels` which has those values as NA. `load_ct_levels()`
    # populates them with the values from the catch levels runs which were
    # run using reduction search type algorithms and have their results
    # located in forecast files in their respective run directories
    ct_levels_lst <- load_ct_levels(model_path, ...)
    model$ct_levels <- ct_levels_lst$ct_levels
    model$ct_levels_vals <- ct_levels_lst$ct_levels_vals

    default_policy_ind <- ct_levels_lst$ct_levels_vals$ct_default_policy_ind
    model$ct_default_policy <-
      model$ct_levels[[default_policy_ind]][[1]]
  }

  model$forecasts <- NA
  model$risks <- NA
  if(dir.exists(forecasts_path) && dir.exists(ct_levels_path)){
    model$forecasts <- load_forecasts(model, ...)
    model$risks <- calc_risk(model, ...)
    # Remove "outputs" from forecasts lists after calculating risk
    model$forecasts <- remove_forecast_outputs(model$forecasts)
  }

  # Load retrospectives. If none are found or there is a problem,
  # `model$retros` will be set to `NA`
  model$retros <- load_retrospectives(model, ...)

  # Pre-make plots (optional) ----
  model$plots <- plot_during_loading(model)

  # Remove `extra_mcmc$index_fit_posts`, (set to `NULL`) because it is
  # large. It is needed for the call to `plot_during_loading()` above so
  # DO NOT move it up in the function
  if(!keep_index_fit_posts){
    model$extra_mcmc$index_fit_posts <- NULL
  }

  # These are too large and after the calculations above in `load_mcmc_vals()`
  # and `load_parameter_priors()`, they are not needed any longer
  #model$mcmc <- NULL
  #model$parameters <- NULL

  saveRDS(model, file = rds_file)
  if(file.exists(rds_file)){
    dt <- now() - file.info(rds_file)$mtime
    message("RDS file `", rds_file, "` was created ",
            f(dt[[1]], 2), " ", units(dt), " ago\n\n")
  }else{
    stop("File was not created during the `saveRDS()` call")
  }

  invisible()
}

