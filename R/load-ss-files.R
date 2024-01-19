#' Load all the SS files for output and input, and return the model object
#'
#' @details If MCMC directory is present, load that and perform calculations
#' for mcmc parameters.
#'
#' @param model_path The directory that the model is in
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#' @param ... Passed to `calc_mcmc()`
#'
#' @return A model object representing the output from the SS model
#' @export
load_ss_files <- function(model_path = NA,
                          printstats = FALSE,
                          ...){

  if(!dir.exists(model_path)){
    stop("Directory `", model_path, "` does not exist")
  }

  # Load Report.ss file for model limits etc
  model <- tryCatch({
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              wtfile = weight_at_age_fn)
  }, error = function(e){
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE,
              wtfile = weight_at_age_fn)
  })

  # Load the data file and control file for the model
  # Get the file whose name contains "_data.ss" and "_control.ss"
  # If there is not exactly one of each, stop with error.
  model_path_listing <- tolower(dir(model_path))
  dat_fn_ind <- grep(data_fn, model_path_listing)
  ctl_fn_ind <- grep(control_fn, model_path_listing)
  par_fn_ind <- grep(par_fn, model_path_listing)
  if(!length(dat_fn_ind)){
    stop("The data file `", file.path(model_path, data_fn), "` is missing")
  }
  if(!length(ctl_fn_ind)){
    stop("The control file `", file.path(model_path, control_fn),
         "` is missing")

  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", model_path, ", there is more than one control ",
         "file. A control file is any file with a name ending in ",
         "`_control.ss`")
  }
  model$path <- model_path
  model$dat_file <- file.path(model_path, data_fn)
  model$ctl_file <- file.path(model_path, control_fn)
  model$par_file <- file.path(model_path, par_fn)
  model$dat <- SS_readdat(model$dat_file, verbose = FALSE)
  model$ctl <- SS_readctl(model$ctl_file, verbose = FALSE)
  model$par <- readLines(model$par_file)

  # Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  # Set the mcmc and extra mcmc paths and record their existence
  model$mcmc_path <- file.path(model_path, mcmc_path)
  model$mcmc_exists <- dir.exists(model$mcmc_path)
  model$extra_mcmc_path <- file.path(model$mcmc_path, sso_path)
  model$extra_mcmc_exists <- dir.exists(model$extra_mcmc_path)
  # Save the posterior names from the mcmc output. This is necessary for the
  # function `plot_mcmc_param_stats()`
  posteriors_dir <- ifelse(model$extra_mcmc_exists,
                           model$extra_mcmc_path,
                           model$mcmc_path)

  # If it has an mcmc or mcmc/sso sub-directory, load that as well
  if(dir.exists(posteriors_dir)){
    posteriors_fn <- file.path(posteriors_dir, posts_fn)
    derposteriors_fn <- file.path(posteriors_dir, derposts_fn)
    # Read in the column headers only
    tmp <- readLines(posteriors_fn, n = 1)
    tmp <- str_split(tmp, "[:space:]")[[1]]
    # Remove Empty string, `Iter` and `Objective_function` as they are not
    # parameters
    model$post_names <- tmp[!tmp %in% c("", "Iter", "Objective_function")]
    fix_posteriors(posteriors_fn)
    fix_posteriors(derposteriors_fn)
    model$mcmc <- SSgetMCMC(dir = posteriors_dir,
                            writecsv = FALSE,
                            verbose = FALSE)
    # replace any SPB with SSB
    names(model$mcmc) <- gsub(pattern = "SPB",
                              replacement = "SSB",
                              names(model$mcmc))

    # Get the key and nuisance posteriors
    key_nuisance <- get_key_nuisance_posts(model)
    model$key_posts <- key_nuisance$key_posts
    model$nuisance_posts <- key_nuisance$nuisance_posts

    # Do the mcmc calculations, e.g. quantiles for SB, SSB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc_mcmc(model$mcmc, ...)

    # The MCMC object itself is huge and should not be included in the list,
    # as the loading of it in the document is prohibitive. It was used to
    # populate `mcmccalcs` but is no longer needed. Save the number of
    # posteriors first though
    model$nposts <- nrow(model$mcmc)

  }

  model
}
