#' Remove multiple header/datarows found in posteriors files to leave just one
#'
#' @param dir Directory where the files reside
#'
#' @return If the file contains only one header/datarow (i.e is of correct format)
#' then return a 1-row dataframe of the contents
#' @export
fix.posteriors <- function(dir){

  do.it <- function(file){
    posts <- read.table(file.path(dir, file),
                        header = TRUE,
                        fill = TRUE,
                        stringsAsFactors = FALSE)
    if(all(grepl("^[[:digit:]]", posts[,1]))){
      return(posts)
    }
    write.table(posts[1:(grep("\\D+", posts[,1])[1] - 1),],
                file.path(dir, file),
                quote = FALSE,
                row.names = FALSE)
  }
  do.it(posts_file_name)
  do.it(derposts_file_name)
}

#' Load all the SS files for output and input, and return the model object
#'
#' @details If MCMC directory is present, load that and perform calculations for mcmc parameters.
#'
#' @param model_path The directory that the model is in
#' @param key_posts Vector of key posteriors used to create key posteriors file
#' @param key_posts_fn Name of the key posteriors file
#' @param nuisance_posts_fn Name of the nuisance posteriors file
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A model object representing the output from the SS model
#' @export
load_ss_files <- function(model_path = NA,
                          key_posts = c("NatM",
                                        "SR_LN",
                                        "SR_BH_steep",
                                        "Q_extraSD",
                                        "ln.EffN_mult._1",
                                        "ln.EffN_mult._2"),
                          key_posts_fn = "keyposteriors.csv",
                          nuisance_posts_fn = "nuisanceposteriors.csv",
                          printstats = FALSE,
                          ...){

  stopifnot(!is.na(model_path))

  # Load MPD results
  model <- tryCatch({
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              wtfile = "wtatage.ss")
  }, error = function(e){
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE,
              wtfile = "wtatage.ss")
  })

  # Load the data file and control file for the model
  # Get the file whose name contains "_data.ss" and "_control.ss"
  # If there is not exactly one of each, stop with error.
  model_path_listing <- tolower(dir(model_path))
  dat_fn_ind <- grep("_data.ss", model_path_listing)
  ctl_fn_ind <- grep("_control.ss", model_path_listing)
  par_fn_ind <- grep("ss.par", model_path_listing)
  if(!length(dat_fn_ind)){
    stop("Error in model ", model_path,
         ", there is no data file. A data file is any file whose name end with _data.ss.\n",
         call. = FALSE)
  }
  if(length(dat_fn_ind) > 1){
    stop("Error in model ", model_path,
         ", there is more than one data file. A data file is any file whose name ends with _data.ss.\n\n",
         call. = FALSE)

  }
  if(!length(ctl_fn_ind)){
    stop("Error in model ", model_path,
         ", there is no control file. A control file is any file whose name ends with _control.ss.\n\n",
         call. = FALSE)

  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", model_path,
         ", there is more than one control file. A control file is any file whose name ends with _control.ss.\n\n",
         call. = FALSE)
  }
  model$path <- model_path
  model$dat_file <- file.path(model_path, model_path_listing[dat_fn_ind])
  model$ctl_file <- file.path(model_path, model_path_listing[ctl_fn_ind])
  model$par_file <- file.path(model_path, model_path_listing[par_fn_ind])
  model$dat <- SS_readdat(model$dat_file, verbose = FALSE)
  model$ctl <- readLines(model$ctl_file)
  model$ctl <- gsub("\t", " ", model$ctl)

  # model$par <- readLines(par_fn)
  # Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  # Set the mcmc and extra mcmc paths and record their existence
  model$mcmc_path <- file.path(model_path, "mcmc")
  model$mcmc_exists <- dir.exists(model$mcmc_path)
  model$extra_mcmc_path <- file.path(model$mcmc_path, "sso")
  model$extra_mcmc_exists <- dir.exists(model$extra_mcmc_path)
  # Save the posterior names from the mcmc output. This is necessary for the function `plot_mcmc_param_stats()`
  posteriors_dir <- ifelse(model$extra_mcmc_exists, model$extra_mcmc_path, model$mcmc_path)

  # If it has an mcmc sub-directory, load that as well
  if(dir.exists(posteriors_dir)){
    tmp <- readLines(file.path(posteriors_dir, "posteriors.sso"), n = 1)
    tmp <- str_split(tmp, "[:space:]")[[1]]
    # Remove Empty string, Iter and Objective_function as they are not parameters
    model$post_names <- tmp[!tmp %in% c("", "Iter", "Objective_function")]
    fix.posteriors(posteriors_dir)
    model$mcmc <- SSgetMCMC(dir = posteriors_dir,
                            writecsv = FALSE,
                            verbose = FALSE)
    # replace any SPB with SSB
    names(model$mcmc) <- gsub(pattern="SPB", replacement="SSB", names(model$mcmc))
    create.key.nuisance_posteriors_files(model,
                                         key_posts,
                                         key_posts_fn,
                                         nuisance_posts_fn)
    # Do the mcmc calculations, e.g. quantiles for SB, SSB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc_mcmc(model$mcmc)

  }
  model
}

create.key.nuisance_posteriors_files <- function(model,
                                                 posterior_regex,
                                                 key_post_file,
                                                 nuisance_post_file){
  ## Creates the two files for key and nuisance posteriors
  if(model$extra_mcmc_exists){
    key_file <- here::here(model$extra_mcmc_path, key_post_file)
    nuisance_file <- here::here(model$extra_mcmc_path, nuisance_post_file)
  }else{
    key_file <- here::here(model$mcmc_path, key_post_file)
    nuisance_file <- here::here(model$mcmc_path, nuisance_post_file)
  }

  mc <- model$mcmc
  mc_names <- names(mc)
  mcmc_grep <- unique(grep(paste(posterior_regex, collapse="|"), mc_names))
  mcmc_names <- mc_names[mcmc_grep]
  keys <- mc[, mcmc_grep]
  nuisances <- mc[, -mcmc_grep]
  write.csv(keys, key_file, row.names = FALSE)
  write.csv(nuisances, nuisance_file, row.names = FALSE)
}
