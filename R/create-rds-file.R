#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#' @param ... Arguments to pass to [load_extra_mcmc()]
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL,
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

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(file.exists(rds_file) && !overwrite){
    stop_quietly("The RDS file `", rds_file, "` exists and you did not set ",
         "`overwrite = TRUE`")
  }

  if(verbose){
    message("\nCreating a new RDS file from SS3 model output...\n")
    message("Loading SS3 model input and output files in:\n",
            "`", model_dir, "`\n")
  }
  tic("Load SS3 files")
  model <- load_ss_files(model_dir, ...)
  toc()
  if(verbose){
    message("SS3 input and output files loaded successfully from ",
            "model output in:\n`",
            model_dir, "`\n")
  }
  # Try loading extra mcmc output. If none are found or there is a problem,
  # model$extra.mcmc will be NA
  # `small` is an argument needed, passed through `...`
  tic("Load extra MCMC")
  model$extra.mcmc <- load_extra_mcmc(model, verbose = verbose, ...)
  toc()

  # Load forecasts. If none are found or there is a problem, model$forecasts will be NA
  if(dir.exists(file.path(model_dir, forecasts_path))){
    catch_levels_fullpath <- file.path(model_dir, catch_levels_path)
    model$catch.levels <- fetch_catch_levels(catch_levels_fullpath, catch_levels)
    model$catch.default.policy <- model$catch.levels[[catch.default.policy.ind]][[1]]
    model$forecasts <- fetch_forecasts(model_dir, model$catch.levels, ...)
    model$risks <- calc_risk(forecast_outputs = model$forecasts,
                             catch_levels = model$catch.levels,
                             forecast_yrs)
  }else{
    model$catch.levels <- NA
    model$catch.default.policy <- NA
    model$forecasts <- NA
    model$risks <- NA
  }

  # Load retrospectives. If none are found or there is a problem, model$retros will be NA
  tic("Load retrospectives")
  model$retropath <- file.path(model_dir, retrospectives_path)
  if(dir.exists(model$retropath)){
    model$retros <- fetch_retrospectives(model$retropath,
                                         retrospective_yrs)
  }else{
    model$retros <- NA
  }
  toc()

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

