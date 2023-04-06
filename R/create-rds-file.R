#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#' @param probs A vector of 3 values, the lower CI, median, and upper CI
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param overwrite Logical. If `TRUE`, overwrite the file if it exists
#' @param ... Arguments to pass to [load_ss_files()]
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(
    model_dir = NULL,
    probs = c(0.025, 0.5, 0.975),
    forecast_probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
    ct_levels_lst = set_ct_levels(4),
    forecasts_path = file.path(model_dir, "forecasts"),
    ct_levels_path = file.path(model_dir, "catch-levels"),
    retrospectives_path = file.path(model_dir, "retrospectives"),
    default_hr_path = file.path(ct_levels_path, "default-hr"),
    stable_catch_path = file.path(ct_levels_path, "stable-catch"),
    spr_100_path = file.path(ct_levels_path, "spr-100"),
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

  if(length(probs) != 3){
    stop("`probs` must be a vector of three numeric values",
         call. = FALSE)
  }
  if(probs[2] != 0.5){
    stop("The second element of `probs` must be 0.5",
         call. = FALSE)
  }
  if(is.unsorted(probs)){
    stop("The elements of `probs` must be numerically increasing",
         call. = FALSE)
  }

  if(is.null(ct_levels_lst$ct_levels)){
    stop("`ct_levels_lstct_levels` is `NULL` but requires a value. It is a ",
         "list of lists of vectors of length 3. Use `set_ct_levels()` to ",
         "set this",
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
  model$extra_mcmc <- load_extra_mcmc(model,
                                      probs = probs,
                                      verbose = verbose,
                                      ...)

  # Set all important forecast directories here
  model$forecast_fn <- "forecast.ss"
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
    model$ct_levels <-
      load_ct_levels(model,
                     ct_levels = ct_levels_lst$ct_levels,
                     ...)
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
  # `model$retros` will be NA
  model$retros <- load_retrospectives(model$retrospectives_path,
                                      probs = probs,
                                      ...)

  saveRDS(model, file = rds_file)
  if(file.exists(rds_file)){
    dt <- now() - file.info(rds_file)$mtime
    message("RDS file `", rds_file, "` was created ",
            f(dt[[1]], 2), " ", units(dt), " ago\n")
  }else{
    stop("File was not created during the `saveRDS()` call",
         call. = FALSE)
  }

  invisible()
}

