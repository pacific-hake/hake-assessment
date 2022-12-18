#' Build RDS file for a list of models by running forecasts, retrospectives, and extra-mcmc
#' where applicable
#'
#' @param model_dirs A vector of model directory names (not full paths)
#' @param run_forecasts Logical. TRUE to run forecasts on the models (when an mcmc directory is present)
#' @param run_retrospectives Logical. TRUE to run retrospectives on the models (when an mcmc directory is present)
#' @param run_extra_mcmc Logical. TRUE to run extra-mcmc routine on the models (when an mcmc directory is present)
#' @param run_catch_levels Logical. If TRUE, the catch levels calculations routine will run, overwriting any previous
#' @param catch_levels A list of lists for the forecast catch levels to run forecasts for. See
#' catch levels runs. If FALSE, catch levels will not be run if the catch-levels directory exists, but they will be run
#' if the directory does not exist and run_forecasts is TRUE because it is required for the forecasting step
#' forecast-catch-levels.R
#' @param build_file If `TRUE`, create the RDS file. If `FALSE`, do not create it (used for running retros, etc)
#'
#' @return Nothing
#' @export
#' @importFrom purrr map
#'
#' @examples
#' build_rds(c("base", "nuts"), TRUE, TRUE, TRUE)
build_rds <- function(model_dirs = model_list,
                      build_file = TRUE,
                      ...){
  tic()
  map(model_dirs, function(x = .x, ...){
    run(model_dir = x,
        catch_levels_path = catch_levels_path,
        catch_levels = catch_levels,
        ...)
    if(build_file){
      create_rds_file(model_dir = x,
                      catch_levels = catch_levels,
                      ...)
    }
  }, ...)
  message("\nCompleted build.\n")
  invisible()
  toc()
}

