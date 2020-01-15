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
run.retrospectives <- function(model = NA,
                               yrs = 1:15,
                               remove.blocks = FALSE,
                               extras = "-nox",
                               exe.file.name = "ss.exe",
                               starter.file.name = "starter.ss",
                               forecast.file.name = "forecast.ss",
                               weight.at.age.file.name = "wtatage.ss"){

  stopifnot(!is.na(model))
  
  if(is.na(model$retropath)){
    return(invisible())
  }
  
  retros.dir <- model$retropath
  dir.create(retros.dir, showWarnings = FALSE)
  
  # Create a list for the retros' output to be saved to
  retros.list <- list()
  
  # Create a directory for each retrospective, copy files, and run retro
  for(retro in 1:length(yrs)){
    retro.dir <- file.path(retros.dir, paste0("retro-", pad.num(yrs[retro], 2)))
    dir.create(retro.dir, showWarnings = FALSE)
    
    # Copy all required model files into the retrospective directory
    files.to.copy <- c(file.path(model$path, c(exe.file.name,
                                               starter.file.name,
                                               forecast.file.name,
                                               weight.at.age.file.name)),
                       model$ctl.file,
                       model$dat.file)
    file.copy(files.to.copy, retro.dir)
    
    starter.file <- file.path(retro.dir, starter.file.name)
    starter <- SS_readstarter(starter.file, verbose = verbose)
    starter$retro_yr <- -yrs[retro]
    starter$init_values_src <- 0
    SS_writestarter(starter, dir = retro.dir, verbose = verbose, overwrite = TRUE)
    if(remove.blocks){
      ctl.file <- file.path(retro.dir, model$ctl.file)
      ctl <- readLines(ctl.file)
      ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
      ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
      unlink(ctl.file)
      writeLines(ctl, ctl.file)
    }
    covar.file <- file.path(retro.dir, "covar.sso")
    if(file.exists(covar.file)){
      unlink(covar.file)
    }
    shell.command <- paste0("cd ", retro.dir, " & ", exe.file.name, " extras")
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
