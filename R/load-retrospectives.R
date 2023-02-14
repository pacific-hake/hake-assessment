#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param retro_path The path in which the retrospective directories reside
#' @param retrospective_yrs A vector of years for the retrospectives
#' @param end_yr The end year of the model. Retrospectives will have
#' their `endyr` element set to this minus the numbers of years of the
#' retorspective
#' @return The list of retrospective outputs
#' @export
load_retrospectives <- function(retro_path,
                                retro_yrs,
                                ...){

  message("\nLoading retrospectives from ", retro_path)
  plan("multisession", workers = length(retro_yrs))
  retros_list <- future_imap(retro_yrs, function(x, y, ...){
    retro_sub <- paste0("retro-", pad.num(x, 2))
    retro_dir <- file.path(retro_path, retro_sub)
    message("Loading from ", retro_dir)
    model <- load_ss_files(retro_dir, ...)
    model$extra.mcmc <- load_extra_mcmc(model, small = TRUE, ...)
    model$endyr <- model$endyr - y
    model
  }, ...)
  plan()
  message("Finished loading retrospectives")
  retros_list
}
