#' Create an rds file to hold the model's full MCMC samples for annual numbers at age.
#'
#' @details
#' Simplified from [create_rds_file()] by Andy for highest density interval
#'   manuscript, for which need to look at the full MCMC values for numbers at
#'   age for each year. Save the rds file with name as used in
#'   [create_rds_file()] but with with `natage` appended.
#' This function does not load retrospectives and forecasts.
#' To do that you must run this first, then run other functions
#' to load those things and attach them to the model list
#'
#' @param model_path Directory name of model to be loaded
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param overwrite Logical. If `TRUE`, overwrite the RDS file if it exists
#' @param progress_n Report every time this many files are processed. Consider
#' how many posteriors there are, this should be a fairly large proportion of
#' that (around 1/8th) or there will be too much output and it will run slow
#' @param first Load this many of the files. If a non-positive number, load
#' them all. Used for debugging purposes to cut down the size of the
#' lists used
#' @param ... Arguments to pass to [load_ss_files()]
#'
#' @return [base::invisible()]
#' @examples
#' \dontrun{
#' # create_rds_natage("/srv/hake/models/2024/02-version/01-base-models/01-base/")
#' }
#' @export
create_rds_natage <- function(model_path = NULL,
                              verbose = TRUE,
                              overwrite = FALSE,
                              progress_n = 500,
                              first = 0,
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

  # The RDS file will have the same name as the directory it is in,
  rds_file <- file.path(model_path, paste0(basename(model_path), "-natage.rds"))
  if(file.exists(rds_file)){
    if(!overwrite){
      stop_quietly("The RDS file `", rds_file,"` exists but you did not ",
                   "provide the argument `overwrite = TRUE`, so nothing ",
                   "was done by the `create_rds_file()` function")
    }
  }

  if(verbose){
    message("`create_rds_natage()`: Loading SS3 model input and output ",
            "files in:\n`", model_path, "`\n")
  }

  model <- load_ss_files(model_path, ...)

  if(verbose){
    message("`create_rds_natage()`: Creating a new natage RDS file from SS3 ",
            "model output.")
  }

  # Can delete any commented parts once ths function works.

  # Add values to be used in the document that require the MCMC data frame to
  # calculate, because the MCMC data frame will not be stored in the RDS file
  # due to its size
#  model$mcmcvals <- load_mcmc_vals(model,
#                                   model$dat$endyr + 1)

  # Add prior and posterior extractions/calculations/formatting
#  model$parameter_priors <- get_prior_data(model, ...)
#  model$parameter_posts <- get_posterior_data(model, ...)

  # Add values extracted from the extra MCMC output which includes index
  # estimates, catchability estimates, and at-age data frames
#  model$extra_mcmc <- load_extra_mcmc(model,
#                                      verbose = verbose,
#                                      ...)

  # For create_rds_natage(), just take the relevant parts from
  # `load_extra_mcmc()` and put here, since only want to deal with numbers at
  # age.
  # load_extra_mcmc <- function(model, ...)

  if(is.null(model$extra_mcmc_path) || is.na(model$extra_mcmc_path)){
    if(verbose){
      message("`extra_mcmc_path` is NA or NULL, so not attempting to load ",
              "extra mcmc for the model in:\n`",
              model$path, "`\n")
    }
    return(NA)
  }

  if(!dir.exists(model$extra_mcmc_path)){
    if(verbose){
      message("The `", model$extra_mcmc_path,
              "` directory does not exist, so extra mcmc files were not ",
              "loaded for model located in:\n", model$path, "\n\n")
    }
    return(NA)
  }

  if(first <= 0){
    first <- model$nposts
    if(is.null(first)){
      first <- model$mcmc |> nrow()
    }
  }

  # These are for debugging, so don't have keep reloading (see below).
  if(!exists("reps") || (exists("reps") && length(reps) != first)){
    if(verbose){
      message("Loading Report files")
    }
    reps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = regex_extra_mcmc_report,
        progress_n = progress_n,
        verbose = verbose,
        first = first)
  }

  if(!exists("compreps") || (exists("compreps") && length(compreps) != first)){
    if(verbose){
      message("Loading CompReport files")
    }
    compreps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = regex_extra_mcmc_compreport,
        progress_n = progress_n,
        verbose = verbose,
        first = first)
  }
  # For debugging only, uncomment these lines to save these as global.
  # If they exist, the loading above will not happen
  reps <<- reps
  compreps <<- compreps

  extra_mcmc <- list()

  # Numbers-at-age ------------------------------------------------------------
  natage_lst <- load_extra_mcmc_atage(
    reps = reps,
    verbose = verbose,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    txt = "numbers-at-age",
    beg_pat = "^NUMBERS_AT_AGE report",
    end_pat = "^BIOMASS_AT_AGE",
    scale = 1e3,
    progress_n = progress_n,
    ...)

  natage_mcmc <- natage_lst$atage

#  extra_mcmc$natage_med <- natage_lst$med

  # This was for some selectivity calculations in load_extra_mcmc().
#  natage <- natage_lst$atage |>
#    dplyr::filter(yr == model$endyr + 1) |>
#    select(-c(yr, iter))

#  model$mcmcparams <- load_parameter_priors(model)

  # Remove `extra_mcmc$index_fit_posts`, (set to `NULL`) because it is
  # large. It is needed for the call to `plot_during_loading()` above so
  # DO NOT move it up in the function
#  if(!keep_index_fit_posts){
#    model$extra_mcmc$index_fit_posts <- NULL
#  }

  # TODO: Test removal of these (NULL-ify) to see if they are needed for
  # retrospective models
  # These are too large and after the calculations above in `load_mcmc_vals()`
  # and `load_parameter_priors()`, they are not needed any longer
  #model$mcmc <- NULL
  #model$parameters <- NULL

  saveRDS(natage_mcmc, file = rds_file)
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
