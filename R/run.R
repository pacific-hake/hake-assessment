#' Run extra models for forecasting, retrospectives, and extra MCMC
#' (one report file per posterior)
#'
#' @details This is a wrapper function for calling [run_ct_levels()],
#' [run_forecasts()], and [run_retrospectives()] functions.
#'
#' @param model_dir The directory the models are located in
#' @param run_forecasts Logical. Run forecasting?
#' @param run_retrospectives Logical. If `TRUE`, run retrospectives?
#' @param run_catch_levels Logical. If `TRUE`, run catch levels estimation
#' routines
#' @param ... Arguments passed to the [run_ct_levels()], [run_forecasts()],
#' and [run_retrospectives()] functions
#'
#' @return [base::invisible()]
#' @export
run <- function(model_dir = NULL,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                run_catch_levels = FALSE,
                ...){

  stopifnot(!is.null(model_dir))

  if(!dir.exists(model_dir)){
    stop("The ", model_dir, " directory does not exist",
         call. = FALSE)
  }
  if(dir.exists(file.path(model_dir, "mcmc"))){
    if(run_catch_levels){
      run_ct_levels(model_dir, ...)
    }
    if(run_forecasts){
      ct_levels_fullpath <- file.path(model_dir, ct_levels_path)
      if(!dir.exists(ct_levels_fullpath) | run_ct_levels){
        run_ct_levels(model_dir, ...)
      }
      run_forecasts(model_dir, ...)
    }
    if(run_retrospectives){
      run_retrospectives(model_dir, ...)
    }
  }
}
