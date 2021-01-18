#' Runs retrospectives for the given model and for the vector of years given
#'
#' @details This will create a *retrospectives* directory in the same directory as the model resides,
#' create a directory for each restrospective year, copy all model files into each directory,
#' run the retrospectives, and make a list of the [r4ss::SS_output()] call to each
#' Warning - This function will completely delete all previous retrospectives that have been run without notice.
#'
#' @param model
#' @param yrs A vector of years to subtract from the model's data to run on.
#' @param remove.blocks
#' @param extras Extra switches for the command line.
#' @param exe.file.name
#' @param starter.file.name
#' @param forecast.file.name
#' @param weight.at.age.file.name
#'
#' @return [base::invisible()]
#' @export
#'
#' @examples
run_retrospectives <- function(model_path,
                               remove_blocks = FALSE,
                               ...){

  model <- load_ss_files(model_path, ...)
  retro_path <- file.path(model_path, "retrospectives")
  dir.create(retro_path, showWarnings = FALSE)
  unlink(file.path(retro_path, "*"), recursive = TRUE)

  # Create a list for the retrospective output to be saved to
  retros_list <- list()

  # Copy all required model files into the retrospective directory
  files_to_copy <- c(file.path(model_path, c(ss_executable,
                                             starter_file_name,
                                             forecast_file_name,
                                             weight_at_age_file_name)),
                     model$ctl.file,
                     model$dat.file)

  # Create a directory for each retrospective, copy files, and run retro
  plan("multisession")
  future_map(retrospective_yrs, ~{
    retro_subdir <- file.path(retro_path, paste0("retro-", pad.num(.x, 2)))
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
    asp <- ctl$age_selex_parms$dev_maxyr
    asp <- ifelse(asp > dat$endyr - .x, dat$endyr - .x, asp)
    ctl$age_selex_parms$dev_maxyr <- asp

    chk <- ctl$age_selex_parms %>%
      filter(dev_minyr !=0) %>%
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
      ctl_file <- file.path(retro_subdir, model$ctl.file)
      ctl <- readLines(ctl.file)
      ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
      ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
      unlink(ctl_file)
      writeLines(ctl, ctl_file)
    }
    covar_file <- file.path(retro_subdir, "covar.sso")
    if(file.exists(covar_file)){
      unlink(covar_file)
    }
    shell_command <- paste0("cd ", retro_subdir, " & ", ss_executable, " -nox")
    shell(shell_command, wait = FALSE, intern = !show_ss_output)
    data_new <- readLines(file.path(retro_subdir, "data.ss_new"))
    df_for_meanbody <- grep("DF_for_meanbodysize", data_new)
    if(length(df_for_meanbody)){
      data_new[df_for_meanbody] <- paste0("#_COND_", data_new[df_for_meanbody])
      writeLines(data_new, con = file.path(retro_subdir, "data.ss_new"))
    }
  })
  plan()
}

#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param retro.path The path in which the retrospective directories reside
#' @param retro.yrs A vector of years for the retrospectives
#' @param printstats  Print info on each model loaded via [r4ss::SS_output()]
#'
#' @return
#' @export
fetch_retrospectives <- function(retro_path,
                                 retrospective_yrs,
                                 ...){

  message("\nLoading retrospectives from ", retro_path)
  retros_list <- map(retrospective_yrs, ~{
    retro_dir <- file.path(retro_path, paste0("retro-", pad.num(.x, 2)))
    message("Loading from ", retro_dir)
    SS_output(dir = retro_dir,
              verbose = FALSE,
              printstats = FALSE,
              covar = FALSE)
  })
  message("Finished loading retrospectives")
  retros_list
}
