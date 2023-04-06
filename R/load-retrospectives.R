#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param retro_path The path in which the retrospective directories reside
#' @param ... Arguments passed to [load_ss_files()] and [load_extra_mcmc()]
#'
#' @return The list of retrospective outputs
#' @export
load_retrospectives <- function(retro_path,
                                ...){

  if(!dir.exists(retro_path)){
    warning("The retrospectives directory `", retro_path, "` does not ",
            "exist. Not loading any retrospectives.")
    return(NA)
  }
  fns <- dir(retro_path)
  pat <- "^retro-([0-9]+)$"
  num_fns_match <- length(grep(pat, fns))
  if(!num_fns_match){
    stop("There were no subdirectories in the `", retro_path, "` directory ",
         "that matched the pattern ", pat,
         call. = FALSE)
  }
  retro_yrs <- sort(as.numeric(gsub(pat, "\\1", fns)))

  message("Loading retrospectives from ", retro_path)
  plan("multisession", workers = length(retro_yrs))
  retros_list <- future_imap(retro_yrs, function(x, y, ...){
    retro_sub <- paste0("retro-", pad_num(x, 2))
    retro_dir <- file.path(retro_path, retro_sub)
    message("Loading from ", retro_dir)
    model <- load_ss_files(retro_dir, ...)
    model$extra_mcmc <- load_extra_mcmc(model, ...)
    model$endyr <- model$endyr - y
    model
  }, ...,
  .options = furrr_options(globals = c("hake::pad_num")))
  plan()
  message("Finished loading retrospectives")
  retros_list
}
