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
run.retrospectives <- function(model_path = NULL,
                               yrs = NULL,
                               remove_blocks = FALSE,
                               extras = "-nox",
                               exe_file_name = "ss.exe",
                               starter_file_name = "starter.ss",
                               forecast_file_name = "forecast.ss",
                               weight_at_age_file_name = "wtatage.ss",
                               ...){

  stopifnot(!is.null(model_path),
            !is.null(yrs))
  
  retro_path <- file.path(model_path, "retrospectives")
  delete.dirs(sub.dir = retro_path)
  dir.create(retro_path, showWarnings = FALSE)
  
  # Create a list for the retrospective output to be saved to
  retros_list <- list()
  
  # Copy all required model files into the retrospective directory
  model <- load.ss.files(model_path, ...)
  files_to_copy <- c(file.path(model_path, c(exe.file.name,
                                             starter.file.name,
                                             forecast.file.name,
                                             weight.at.age.file.name)),
                     model$ctl.file,
                     model$dat.file)
                     
  # Create a directory for each retrospective, copy files, and run retro
  for(retro in 1:length(yrs)){
    retro_subdir <- file.path(retro_path, paste0("retro-", pad.num(yrs[retro], 2)))
    dir.create(retro_subdir, showWarnings = FALSE)
    file.copy(files_to_copy, retro_subdir)
    
    starter_file <- file.path(retro_subdir, starter_file_name)
    starter <- SS_readstarter(starter_file, verbose = FALSE)
    starter$retro_yr <- -yrs[retro]
    starter$init_values_src <- 0
    SS_writestarter(starter, dir = retro_subdir, verbose = FALSE, overwrite = TRUE)
    if(remove.blocks){
      ctl_file <- file.path(retro_subdir, model$ctl.file)
      ctl <- readLines(ctl.file)
      ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
      ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
      unlink(ctl_file)
      writeLines(ctl, ctl_file)
    }
    covar.file <- file.path(retro_subdir, "covar.sso")
    if(file.exists(covar_file)){
      unlink(covar_file)
    }
    shell.command <- paste0("cd ", retro_subdir, " & ", exe.file.name, " extras")
    shell(shell.command)
  }
  invisible()
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
fetch.retros <- function(retro.path = NA,
                         retro.yrs = NA,
                         verbose = FALSE,
                         printstats = FALSE){

  retros.paths <- file.path(retro.path, paste0("retro-", pad.num(retro.yrs, 2)))
  if(is.na(retro.path) | !all(dir.exists(retros.paths))){
    return(NA)
  }
  if(all(dir.exists(retros.paths))){
    message("Loading retrospectives...\n")
    retros.list <- list()
    for(retro in 1:length(retro.yrs)){
      retro.dir <- file.path(retro.path, paste0("retro-", pad.num(retro.yrs[retro], 2)))
      retros.list[[retro]] <- SS_output(dir = retro.dir,
                                        verbose = verbose,
                                        printstats = printstats,
                                        covar = FALSE)
    }
    message("Retrospectives loaded for ", retro.path, ".")
  }else{
    message("Not all retrospective directories exist in ", retro.path , "Look at retrospective-setup.r and your directories ",
            "to make sure they are both the same or set run.retros = TRUE.")
  }
  retros.list
}
