#' A wrapper to run the catch levels determination routines
#'
#' @param model_path The model directory name
#' @param ... Absorbs arguments intended for other functions
#'
#' @return [base::invisible()]
#' @export
run_ct_levels <- function(model = NULL,
                          model_path = NULL,
                          forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                          ...){

  if(is.null(model)){
    if(is.null(model_path)){
      stop("`run_ct_levels`: Either `model` or `model_path` must be supplied")
    }
    model <- load_ss_files(model_path, ...)
  }

  ct_levels_fullpath <- file.path(model_path, ct_levels_path)
  dir.create(ct_levels_fullpath, showWarnings = FALSE)
  unlink(file.path(ct_levels_fullpath, "*"), recursive = TRUE)

  if(supportsMulticore()){
    plan("multicore", workers = 3)
  }else{
    message("`run_ct_levels`: Your operating system does not support ",
            "forking, so multicore parallelism cannot be used. Use a Mac or ",
            "Linux machine if you want this to run in parallel. Even if you ",
            "are on a Mac or Linux machine, you cannot use multicore ",
            "parallelism if you are in an Rstudio terminal, you must run ",
            "the code using Rscript (via bash file perhaps) or terminal R")
    plan("sequential")
  }
  future_walk(1:3, \(x = .x,
                     mdl = model,
                     fore_yrs = forecast_yrs){

    if(x == 1){
      run_ct_levels_default_hr(mdl, fore_yrs)
    }else if(x == 2){
      run_ct_levels_spr_100(mdl, fore_yrs)
    }else{
      run_ct_levels_stable_catch(mdl, fore_yrs)
    }
  })

  invisible()
}
