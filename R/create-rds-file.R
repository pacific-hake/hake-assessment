#' Create an rds file to hold the model's data and outputs.
#'
#' @details
#' This function does not load retrospectives and forecasts.
#' To do that you must run this first, then run other functions
#' to load those things and attach them to the model list
#'
#' @param model_path Directory name of model to be loaded
#' @param keep_index_fit_posts Logical. If `TRUE`, keep the `index_fit_posts`
#' data in the model's list. This contains the survey fit output for every
#' posterior`and is quite large. Typically kept for base model only. Needed
#' for the [plot_survey_fit_mcmc()] plot to work.
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param overwrite Logical. If `TRUE`, overwrite the RDS file if it exists
#' @param ... Arguments to pass to [load_ss_files()]
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_path = NULL,
                            keep_index_fit_posts = TRUE,
                            verbose = TRUE,
                            overwrite = FALSE,
                            ...){

  if(is.null(model_path)){
    stop("`model_path` must be supplied")
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
                   "provide the argument `overwrite = TRUE`, so nothing ",
                   "was done by the `create_rds_file()` function")
    }
  }

  if(verbose){
    message("`create_rds_file()`: Loading SS3 model input and output ",
            "files in:\n`", model_path, "`\n")
  }

  model <- load_ss_files(model_path, ...)

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

  # Add values extracted from the extra MCMC output which includes index
  # estimates, catchability estimates, and at-age data frames
  model$extra_mcmc <- load_extra_mcmc(model,
                                      verbose = verbose,
                                      ...)

  model$mcmcparams <- load_parameter_priors(model)

  # Remove `extra_mcmc$index_fit_posts`, (set to `NULL`) because it is
  # large. It is needed for the call to `plot_during_loading()` above so
  # DO NOT move it up in the function
  if(!keep_index_fit_posts){
    model$extra_mcmc$index_fit_posts <- NULL
  }

  # TODO: Test removal of these (NULL-ify) to see if they are needed for
  # retrospective models
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
    stop("File creation failed for file `", rds_file ,
         "` during the `saveRDS()` call")
  }

  invisible()
}