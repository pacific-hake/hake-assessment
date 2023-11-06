#' Build RDS file for a list of models by running forecasts, retrospectives, and extra-mcmc
#' where applicable
#'
#' @param model_dirs A vector of model directory names (not full paths)
#' @param build_file If `TRUE`, create the RDS file. If `FALSE`, do not create it (used for running retros, etc)
#' @param ... Arguments passed to [run()] and [create_rds_file()]
#'
#' @return Nothing
#' @export
build_rds <- function(model_dirs = model_list,
                      build_file = TRUE,
                      ...){

  map(model_dirs, function(x = .x, ...){
    run(model_dir = x,
        ct_levels_path = ct_levels_dir,
        ct_levels = ct_levels,
        ...)
    if(build_file){
      create_rds_file(model_dir = x,
                      ct_levels = ct_levels,
                      ...)
    }
  }, ...)
  message("\nCompleted build.\n")

  invisible()
}

