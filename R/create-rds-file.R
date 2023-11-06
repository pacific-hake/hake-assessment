#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
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
create_rds_file <- function(
    model_dir = NULL,
    keep_index_fit_posts = FALSE,
    ct_levels_lst = set_ct_levels(),
    forecasts_path = file.path(model_dir, forecasts_path),
    ct_levels_path = file.path(model_dir, catch-levels_path),
    retrospectives_path = file.path(model_dir, retrospectives_path),
    default_hr_path = file.path(ct_levels_path, default_hr_path),
    stable_catch_path = file.path(ct_levels_path, stable_catch_path),
    spr_100_path = file.path(ct_levels_path, spr_100_path),
    verbose = TRUE,
    overwrite = TRUE,
    ...){

  stopifnot(!is.null(model_dir))

  if(length(grep("\\/$", model_dir))){
    # Remove trailing slashes
    model_dir <- gsub("\\/+$", "", model_dir)
  }
  if(!dir.exists(model_dir)){
    stop("Directory `", model_dir, "` does not exist",
         call. = FALSE)
  }

  if(is.null(ct_levels_lst$ct_levels)){
    stop("`ct_levels_lstct_levels` is `NULL` but requires a value. It is a ",
         "list of lists of vectors of length 3. Use ",
         "`hake::set_ct_levels()` to set this",
         call. = FALSE)
  }
  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(file.exists(rds_file)){
   if(overwrite){
     unlink(rds_file, force = TRUE)
   }else{
     stop_quietly("The RDS file `", rds_file, "` exists and you did not set ",
                  "`overwrite = TRUE`")
   }
  }

  if(verbose){
    message("Creating a new RDS file from SS3 model output...")
    message("Loading SS3 model input and output files in:\n",
            "`", model_dir, "`\n")
  }
  model <- load_ss_files(model_dir, ...)
  if(verbose){
    message("SS3 input and output files loaded successfully from ",
            "model output in: ",
            "`", model_dir, "`\n")
  }
  # Try loading extra mcmc output. If none are found or there is a problem,
  # model$extra_mcmc will be NA
  if(verbose){
    message("Loading extra MCMC output for model in:\n",
            "`", model$extra_mcmc_path, "`\n")
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

  # Set all important forecast directories here
  model$forecasts_path <- forecasts_path
  model$ct_levels_path <- ct_levels_path
  model$retrospectives_path <- retrospectives_path
  model$default_hr_path <- default_hr_path
  model$stable_catch_path <- stable_catch_path
  model$spr_100_path <- spr_100_path

  # Load forecasts. If none are found or there is a problem,
  # `model$forecasts` will be `NA`
  model$catch_levels <- NA
  model$catch_default_policy <- NA
  if(dir.exists(model$ct_levels_path)){
    # Load in the missing `default HR`, `SPR 100`, and `stable catch` values
    # by passing `ct_levels` which has those values as NA. `load_ct_levels()`
    # populates them with the values from the catch levels runs which were
    # run using reduction search type algorithms and have their results
    # located in forecast files in their respective run directories
    ct_levels_lst <- load_ct_levels(model,
                                    ct_levels_lst = ct_levels_lst,
                                    ...)

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
    stop("File was not created during the `saveRDS()` call",
         call. = FALSE)
  }

  invisible()
}

