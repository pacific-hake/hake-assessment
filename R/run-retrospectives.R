#' Runs retrospectives for the given model and for the vector of years given
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param model_path The path of the model run
#' @param retro_mcmc If `TRUE`, run the ADNUTS MCMC in the *mcmc*
#' subdirectory for each retrospective in addition to the MLE run
#' @param retro_yrs A vector of number of years value prior to current end
#' year in the model to run retrospectives for. Example if this is c(1, 5)
#' then two retrospectives will be run, one with 1 year of data removed and
#' one with 5 years of data removed
#' @param ss_exe The name of executable to use or `NULL` to use the package
#' data variable [ss_executable]
#' @param ... Arguments passed to [load_ss_files()] and [run_adnuts()]
#'
#' @details
#' This will:
#' 1. Create a directory with the name found in the package data  variable
#'    [retrospectives_path] directory in the same directory that the model
#'    resides in
#' 2. Create subdirectories for each retrospective year
#' 3. Copy all SS3 model input files into each directory
#' 4. Run a retrospective for each year found in `retro_yrs`
#'
#' @return [base::invisible()]
#' @export
run_retrospectives <- function(model = NULL,
                               model_path = NULL,
                               retro_yrs = retrospective_yrs,
                               num_chains = 1,
                               retro_mcmc = TRUE,
                               ss_exe = NULL,
                               ...){

  # Check core availability ----
  # Allow 90% core usage
  num_cores <- floor(availableCores() * 0.9)
  total_requested_cores <- num_chains * length(retro_yrs)
  if(total_requested_cores  >= num_cores){
    stop("Your system has ", num_cores," and you have asked for ", num_chains,
         " chains for ", length(retro_yrs), " years which equates to ",
         total_requested_cores, " cores. You must reduce the number of ",
         "chains, years, or both")
  }

  ss_exe <- get_ss3_exe_name(ss_exe)

  # Ensure `model` and `model_path` have values ----
  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
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

  # Setup retrospective path names ----
  retro_pth <- file.path(model_path, retropectives_path)
  dir.create(retro_pth, showWarnings = FALSE)
  file_chmod(retro_pth, output_permissions)
  retro_subdirs <- file.path(retro_pth,
                             paste0(retrospectives_prepend,
                                    pad_num(retro_yrs, 2)))
  src_fns <- file.path(model_path, ss_input_files)

  # Create directories and copy input files into them ----
  walk(retro_subdirs, \(retro_subdir){
    dir.create(retro_subdir, showWarnings = FALSE)
    file_chmod(retro_subdir, output_permissions)
    unlink(file.path(retro_subdir, "*"), force = TRUE)
    dest_fns <- file.path(retro_subdir, ss_input_files)
    copy_flags <- file.copy(src_fns, dest_fns)
    if(!all(copy_flags)){
      stop("Problem copying SS3 input files from directory `", model_path,
           " to retrospective subdirectory `", retro_subdir, "`")
    }
  })

  # Main run loop ----
  walk2(
    retro_yrs,
    retro_subdirs,
    \(retro_yr, retro_subdir, ...){

      message("Running retrospective for year = ", retro_yr)
      starter_file <- file.path(retro_subdir, starter_fn)
      starter <- SS_readstarter(starter_file, verbose = FALSE)
      starter$retro_yr <- -retro_yr
      starter$init_values_src <- 0
      SS_writestarter(starter,
                      dir = retro_subdir,
                      verbose = FALSE,
                      overwrite = TRUE)

      dat <- SS_readdat(file.path(retro_subdir, starter$datfile),
                        verbose = FALSE,
                        version = model$SS_versionshort)
      ctl <- SS_readctl(file.path(retro_subdir, starter$ctlfile),
                        verbose = FALSE,
                        use_datlist = TRUE,
                        datlist = dat,
                        version = model$SS_versionshort)

      ctl$MainRdevYrLast <- ctl$MainRdevYrLast - retro_yr
      ctl$last_yr_fullbias_adj <- ctl$MainRdevYrLast - 1
      ctl$first_recent_yr_nobias_adj <- ctl$MainRdevYrLast

      asp <- ctl$age_selex_parms$dev_maxyr
      asp <- ifelse(asp > dat$endyr - retro_yr, dat$endyr - retro_yr, asp)
      ctl$age_selex_parms$dev_maxyr <- asp

      chk <- ctl$age_selex_parms |>
        dplyr::filter(dev_minyr != 0) |>
        select(dev_minyr, dev_maxyr) |>
        mutate(diff = dev_maxyr - dev_minyr) |>
        pull(diff)
      if(length(chk) > 0){
        if(any(chk < 1)){
          stop("The retrospective, ",
               basename(retro_subdir),
               ", has time-varying selectivity outside the data years.")
        }
      }
      SS_writectl(ctl,
                  outfile = file.path(retro_subdir, starter$ctlfile),
                  version = model$SS_versionshort,
                  overwrite = TRUE,
                  verbose = FALSE)

      covar_file <- file.path(retro_subdir, covar_fn)
      unlink(covar_file, force = TRUE)

      if(retro_mcmc){
        # Call to ADNUTS ----
        run_adnuts(path = retro_subdir,
                   num_chains = num_chains,
                   ...)
      }else{
        command <- paste0("cd ", retro_subdir, " && ",
                          ss_exe, " -nox")
        system_(command, wait = FALSE, intern = !show_ss_output)
      }
      d_new_fn <- file.path(retro_subdir, data_new_ssnew_fn)
      if(file.exists(d_new_fn)){
        d_new <- readLines(d_new_fn)
        df_for_meanbody <- grep("DF_for_meanbodysize", d_new)
        if(length(df_for_meanbody)){
          d_new[df_for_meanbody] <- paste0("#_COND_",
                                           d_new[df_for_meanbody])
          writeLines(d_new, con = file.path(retro_subdir,
                                            data_ssnew_fn))
          file_chmod(file.path(retro_subdir,
                               data_ssnew_fn), output_permissions)
          writeLines(d_new, con = file.path(retro_subdir,
                                            data_new_ssnew_fn))
          file_chmod(file.path(retro_subdir,
                               data_new_ssnew_fn), output_permissions)
          message("Done running retrospective for year = ", retro_yr)
        }
      }
    }, ...)
}
