#' A wrapper to run the catch levels determination routines
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param model_path The model directory name
#' @param forecast_yrs A vector of forecast years
#' @param ... Passes arguments to [run_ct_levels_default_hr()],
#' [run_ct_levels_spr_100()], and [run_ct_levels_stable_catch()]
#'
#' @return [base::invisible()]
#' @export
run_ct_levels <- function(model = NULL,
                          model_path = NULL,
                          forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                          run_default_hr = TRUE,
                          run_spr_100 = TRUE,
                          run_stable_catch = TRUE,
                          ...){

  if(!any(c(run_default_hr,
            run_spr_100,
            run_stable_catch))){
    message("All of `run_default_hr`, `run_spr_100`, and  `run_stable_catch` ",
            "are `FALSE`,\nso no catch levels routines were run")
    return(invisible())
  }

  # First, check if the computer/terminal combination of capable of running
  # the code in parallel. If only sequential is possible, ask if caller wants
  # to continue, because it takes a long time to run sequentially.
  supports_multicore <- supportsMulticore()
  if(!supports_multicore){
    message(paste0("`run_ct_levels`: ", parallelism_warning))
    if(interactive()){
      continue_ans <- readline("Do you want to continue (y/n)? ")
      continue_ans <- substr(tolower(continue_ans), 1, 1)
      if(continue_ans == "n"){
        stop_quietly("Halted program at user request, without any changes ",
                     "to\ndirectories/files and without any catch level runs ",
                     "done\n")
      }
    }
  }

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

  ct_levels_fullpath <- file.path(model_path, ct_levels_path)
  dir.create(ct_levels_fullpath, showWarnings = FALSE)
  file_chmod(ct_levels_fullpath, output_permissions)
  unlink(file.path(ct_levels_fullpath, "*"), recursive = TRUE)

  if(supportsMulticore()){
    plan("multicore", workers = 3)
  }else{
    message(paste0("`run_ct_levels`: ", parallelism_warning))
    plan("sequential")
  }

  # `funcs` holds the names of the functions to run
  funcs <- c(run_ct_levels_default_hr,
             run_ct_levels_spr_100,
             run_ct_levels_stable_catch)
  # Use the input logicals to filter the functions so to be only the ones
  # set to `TRUE`
  funcs <- funcs[c(run_default_hr,
                   run_spr_100,
                   run_stable_catch)]

  if(supports_multicore){
    # One worker for each catch stream
    plan("multicore", workers = length(funcs))
  }else{
    plan("sequential")
  }
  future_walk(funcs, \(func,
                       mdl = model,
                       fore_yrs = forecast_yrs,
                       ...){

    func(mdl, fore_yrs, ...)
  }, ...)

  invisible()
}
