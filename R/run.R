#' Run catch-levels, forecasting, and retrospectives in one function
#'
#' @details
#' This is a wrapper function for calling [run_ct_levels()],
#' [run_forecasts()], and [run_retrospectives()] functions.
#'
#' @param model_path The directory the models are located in
#' @param run_forecasts Logical. If `TRUE` run forecasting
#' @param run_retrospectives Logical. If `TRUE` run retrospectives
#' @param run_catch_levels Logical. If `TRUE` run catch levels estimation
#' @param ... Arguments passed to the [run_ct_levels()], [run_forecasts()],
#' and [run_retrospectives()] functions
#'
#' @return [base::invisible()]
#' @export
run <- function(model = NULL,
                model_path = NULL,
                run_catch_levels = FALSE,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                ...){

  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
    }
    model <- load_ss_files(model_path, ...)
  }else{
    if(is.null(model_path)){
      model_path <- model$path
    }else{
      if(model$path != model_path){
        stop("You provided both `model` and `model_path` and `model$path` ",
             "does not math `model_path`")
      }
    }
  }

  if(!dir.exists(model_path)){
    stop("The ", model_path, " directory does not exist")
  }

  mcmc_fullpath <- file.path(model_path, mcmc_path)
  if(!dir.exists(mcmc_fullpath)){
    stop("The ", mcmc_fullpath, " directory does not exist")
  }

  if(run_catch_levels){
    run_ct_levels(model, ...)
  }
  if(run_forecasts){
    ct_levels_fullpath <- file.path(model_path, ct_levels_path)
    if(!dir.exists(ct_levels_fullpath)){
      message("You requested the forecasting be run bu the catch-levels ",
              "directory does not exist so `run_catch_levels() must be run ",
              "first. Executing run_catch_levels().\n")
      run_ct_levels(model, ...)
    }
    run_forecasts(model, ...)
  }
  if(run_retrospectives){
    run_retrospectives(model, ...)
  }

  invisible()
}
