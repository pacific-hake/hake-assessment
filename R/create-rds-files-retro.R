#' Create the RDS files for all retrospective runs
#'
#' @param model_path Directory name of model to be loaded
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param ... Arguments to pass to [create_rds_file()]
#'
#' @return [base::invisible()]
#' @export
create_rds_files_retro <- function(model_path,
                                   verbose = TRUE,
                                   ...){

  retro_fullpath <- file.path(model_path, retropectives_path)
  if(!dir.exists(retro_fullpath)){
    if(verbose){
      stop("The retrospectives directory `", retro_fullpath, "` does not ",
           "exist")
    }
  }

  fns <- dir(retro_fullpath)
  pat <- paste0("^", retrospectives_prepend, "([0-9]+)$")
  retro_paths <- grep(pat, fns, value = TRUE)
  num_fns_match <- length(retro_paths)

  if(!num_fns_match){
    stop("There were no subdirectories in the `", retro_fullpath,
         "`\ndirectory that matched the pattern ", pat)
  }
  retro_paths <- file.path(model_path, retropectives_path, retro_paths)
  if(verbose){
    message("\nAttempting to create RDS files for retrospective models in ",
            "the\n`", retro_fullpath, "`\ndirectory\n")
  }

  if(supportsMulticore()){
    message("Creating the retrospective RDS files in parallel")
    plan("multicore", workers = length(retro_paths))
  }else{
    message(paste0("`create_rds_files_retro()`: ", parallelism_warning))
    if(interactive()){
      message("\nContinue in sequential mode? (choose a number)")
      ans <- menu(c("Yes", "No"))
      if(ans == 2){
        message("`create_rds_files_retro()`: Bailing out at the user's ",
                "request")
        return(invisible())
      }
    }
    plan("sequential")
  }

  future_walk(retro_paths, \(pth, ...){
    create_rds_file(pth, verbose = verbose, ...)
  }, ...)

}
