#' Runs retrospectives for the given model and for the vector of years given
#'
#' @param model_path The path of the model run
#' @param remove_blocks If `TRUE`, remove block designs from control file prior to running
#' @param retro_mcmc If `TRUE`, run the ADNUTS MCMC in the *mcmc* subdirectory for each
#' retrospective in addition to the MLE run
#' @param num_samples Same as in [run_adnuts()]
#' @param num_warmup_samples Same as in [run_adnuts()]
#' @param num_chains Same as in [run_adnuts()]
#' @param retrospective_yrs The years (e.g. 1:6) to run so each of these numbers means that many
#' years of data removed from the model
#' @param ... Arguments passed to [load_ss_files()]
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
                               retrospective_yrs = NA,
                               remove_blocks = FALSE,
                               retro_mcmc = TRUE,
                               run_extra_mcmc = TRUE,
                               fn_exe = ifelse(exists("ss_executable"),
                                               ss_executable,
                                               "ss3"),
                               ...){

  stopifnot(!is.na(retrospective_yrs))

  model <- load_ss_files(model_path, ...)
  retro_path <- file.path(model_path, "retrospectives")
  dir.create(retro_path, showWarnings = FALSE)

  # Copy all required model files into the retrospective directory
  files_to_copy <- c(file.path(model_path,
                               c(starter_file_name,
                                 forecast_file_name,
                                 weight_at_age_file_name)),
                     model$ctl_file,
                     model$dat_file)

  walk(retrospective_yrs, function(.x, ...){

    .x <- abs(.x)

    retro_subdir <- file.path(retro_path,
                              paste0("retro-",
                                     pad.num(.x, 2)))
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
    ctl <- SS_readctl(file.path(retro_subdir, starter$ctlfile),
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

    chk <- ctl$age_selex_parms |>
      filter(dev_minyr !=0) |>
      select(dev_minyr, dev_maxyr) |>
      mutate(diff = dev_maxyr - dev_minyr) |>
      pull(diff)
    if(length(chk) > 0){
      if(any(chk < 1)){
        stop("The retrospective, ",
             basename(retro_subdir),
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
      ctl[grep("block designs", ctl)] <-
        "0 # Number of block designs for time varying parameters"
      ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
      unlink(ctl_file)
      writeLines(ctl, ctl_file)
    }
    covar_file <- file.path(retro_subdir, "covar.sso")
    if(file.exists(covar_file)){
      unlink(covar_file)
    }
    if(retro_mcmc){
      run_adnuts(retro_subdir,
                 run_extra_mcmc = run_extra_mcmc,
                 ...)
    }else{
      command <- paste0("cd ", retro_subdir, " && ", fn_exe, " -nox")
      system_(command, wait = FALSE, intern = !show_ss_outpu)
    }
    data_new <- readLines(file.path(retro_subdir, "data_echo.ss_new"))
    df_for_meanbody <- grep("DF_for_meanbodysize", data_new)
    if(length(df_for_meanbody)){
      data_new[df_for_meanbody] <- paste0("#_COND_",
                                          data_new[df_for_meanbody])
      writeLines(data_new, con = file.path(retro_subdir,
                                           "data.ss_new"))
      writeLines(data_new, con = file.path(retro_subdir,
                                           "data_echo.ss_new"))
    }
  }, ...)
}
