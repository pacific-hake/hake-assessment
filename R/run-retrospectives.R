#' Runs retrospectives for the given model and for the vector of years given
#'
#' @param model_path The path of the model run
#' @param remove_blocks If `TRUE`, remove block designs from control file prior to running
#' @param retro_mcmc If `TRUE`, run the ADNUTS MCMC in the *mcmc* subdirectory for each
#' retrospective in addition to the MLE run
#' @param continue If TRUE, attempt to continue the runs from where it left off
#' (in case of unwanted computer shutdown)
#' @param retrospective_yrs The years (e.g. 1:6) to run so each of these numbers means that many
#' years of data removed from the model
#' @param n_cores The number of CPUs to use if running `run_adnuts()`
#' @param ... Arguments passed to `load_ss_files()`
#'
#' @details This will create a *retrospectives* directory in the same directory as the model resides,
#' create a directory for each retrospective year, copy all model files into each directory,
#' run the retrospectives, and make a list of the [r4ss::SS_output()] call to each
#' Warning - This function will completely delete all previous retrospectives that have been run without notice.
#'
#' @return [base::invisible()]
#' @export
#'
#' @examples
run_retrospectives <- function(model_path,
                               remove_blocks = FALSE,
                               retro_mcmc = TRUE,
                               retro_n_final = 8000,
                               retro_warmup_final = 250,
                               retro_continue = TRUE,
                               retrospective_yrs = NA,
                               n_cores = NA,
                               ...){

  stopifnot(!is.na(retrospective_yrs))

  model <- load_ss_files(model_path, ...)
  retro_path <- file.path(model_path, "retrospectives")
  done <- rep(FALSE, length(retrospective_yrs))
  if(!dir.exists(retro_path) | !retro_continue){
    dir.create(retro_path, showWarnings = FALSE)
    unlink(file.path(retro_path, "*"), recursive = TRUE)
  }

  # Copy all required model files into the retrospective directory
  files_to_copy <- c(file.path(model_path, c(#ifelse(get_os() == "windows", paste0(ss_executable, ".exe"), ss_executable),
                                             starter_file_name,
                                             forecast_file_name,
                                             weight_at_age_file_name)),
                     model$ctl_file,
                     model$dat_file)


  # Create a directory for each retrospective, copy files, and run retro
  purrr::map(retrospective_yrs, ~{
    retro_subdir <- file.path(retro_path, paste0("retro-", pad.num(.x, 2)))
    if(dir.exists(retro_subdir)){
      message(" The directory ", retro_subdir, " already exists. Not running the retrospective for that scenario")
    }else{
      dir.create(retro_subdir, showWarnings = FALSE)
      file.copy(files_to_copy, retro_subdir)
      starter_file <- file.path(retro_subdir, starter_file_name)
      starter <- SS_readstarter(starter_file, verbose = FALSE)
      starter$retro_yr <- -.x
      starter$init_values_src <- 0
      SS_writestarter(starter,
                      dir = retro_subdir,
                      verbose = FALSE,
                      overwrite = TRUE)

      dat <- SS_readdat(file.path(retro_subdir, starter$datfile),
                        verbose = FALSE,
                        version = model$SS_versionshort)
      ctl <- r4ss::SS_readctl(file.path(retro_subdir, starter$ctlfile),
                        verbose = FALSE,
                        use_datlist = TRUE,
                        datlist = dat,
                        version = model$SS_versionshort)

      ctl$MainRdevYrLast <- ctl$MainRdevYrLast - .x
      ctl$last_yr_fullbias_adj <- ctl$MainRdevYrLast - 1
      ctl$first_recent_yr_nobias_adj <- ctl$MainRdevYrLast

      asp <- ctl$age_selex_parms$dev_maxyr
      asp <- ifelse(asp > dat$endyr - .x, dat$endyr - .x, asp)
      ctl$age_selex_parms$dev_maxyr <- asp

      chk <- ctl$age_selex_parms %>%
        dplyr::filter(dev_minyr !=0) %>%
        select(dev_minyr, dev_maxyr) %>%
        mutate(diff = dev_maxyr - dev_minyr) %>%
        pull(diff)
      if(length(chk) > 0){
        if(any(chk < 1)){
          stop("The retrospective, ", basename(retro_subdir),
               ", has time-varying selectivity outside the data years.",
               call. = FALSE)
        }
      }
      SS_writectl(ctl,
                  outfile = file.path(retro_subdir, starter$ctlfile),
                  version = model$SS_versionshort,
                  overwrite = TRUE,
                  verbose = FALSE)
      if(remove_blocks){
        ctl_file <- file.path(retro_subdir, model$ctl_file)
        ctl <- readLines(ctl_file)
        ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
        ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
        unlink(ctl_file)
        writeLines(ctl, ctl_file)
      }
      covar_file <- file.path(retro_subdir, "covar.sso")
      if(file.exists(covar_file)){
        unlink(covar_file)
      }
      if(retro_mcmc){
        # Make a modification to the starter file so the extra MCMC files are not created
        modify_starter_mcmc_type(retro_subdir, 1)

        run_adnuts(retro_subdir,
                   n_final = retro_n_final,
                   warmup_final = retro_warmup_final,
                   n_cores = n_cores)
      }else{
        shell_command <- paste0("cd ", retro_subdir, " && ", ss_executable, " -nox")
        system_(shell_command, wait = FALSE, intern = !show_ss_output)
      }
      data_new <- readLines(file.path(retro_subdir, "data.ss_new"))
      df_for_meanbody <- grep("DF_for_meanbodysize", data_new)
      if(length(df_for_meanbody)){
        data_new[df_for_meanbody] <- paste0("#_COND_", data_new[df_for_meanbody])
        writeLines(data_new, con = file.path(retro_subdir, "data.ss_new"))
      }
    }
  })
}

#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param retro_path The path in which the retrospective directories reside
#' @param retrospective_yrs A vector of years for the retrospectives
#' @return The list of retrospective outputs
#' @export
fetch_retrospectives <- function(retro_path,
                                 retrospective_yrs,
                                 ...){

  message("\nLoading retrospectives from ", retro_path)
  retros_list <- map(retrospective_yrs, function(x = .x, ...){
    retro_sub <- paste0("retro-", pad.num(x, 2))
    retro_dir <- file.path(retro_path, retro_sub)
    message("Loading from ", retro_dir)
    load_ss_files(retro_dir, ...)
  }, ...)
  message("Finished loading retrospectives")
  retros_list
}
