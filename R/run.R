#' Run extra models for forecasting, retrospectives, and extra MCMC (one report file per posterior)
#'
#' @details This is a wrapper function for calling [run_catch_levels()], [run_forecasts()],
#' [run_retrospectives()], and [run_extra_mcmc()] functions.
#'
#' @param model_dir The directory the models are located in
#' @param run_forecasts Logical. Run forecasting?
#' @param run_retrospectives Logical. Run restrospectives?
#' @param run_extra_mcmc Logical. Run extra-mcmc calculations?
#' @param run_catch_levels Logical. Run catch levels estimation routines?
#' @param ... Passed to the subroutines
#'
#' @return [base::invisible()]
#' @export
run <- function(model_dir = NULL,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                run_extra_mcmc = FALSE,
                run_catch_levels = FALSE,
                ...){

  stopifnot(!is.null(model_dir))

  if(!dir.exists(model_dir)){
    stop("The ", model_dir, " directory does not exist",
         call. = FALSE)
  }
  if(dir.exists(file.path(model_dir, "mcmc"))){
    if(run_catch_levels){
      run_catch_levels(model_dir, ...)
    }
    if(run_forecasts){
      catch_levels_fullpath <- file.path(model_dir, catch_levels_path)
      if(!dir.exists(catch_levels_fullpath) | run_catch_levels){
        run_catch_levels(model_dir, ...)
      }
      run_forecasts(model_dir, catch_levels_path, ...)
    }
    if(run_retrospectives){
      run_retrospectives(model_dir, ...)
    }
    if(run_extra_mcmc){
      run_extra_mcmc(model_dir, ...)
    }
  }
}
