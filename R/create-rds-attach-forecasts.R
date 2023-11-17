#' Load catch levels and forecasting and attach to as RDS file
#'
#' @details
#' Load forecasting and attach it to an already-created RDS file as the list
#' element `forecasts` and overwrite the RDS file
#'
#' @param model_path Directory name of model to be loaded
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param ... Passed to [check_catch_levels()]
#'
#' @return Nothing
#' @export
create_rds_attach_forecasts <- function(model_path = NULL,
                                        verbose = TRUE,
                                        ...){

  # Check for RDS file existence
  if(is.null(model_path)){
    stop("`model_path` must be supplied")
  }

  if(length(grep("\\/$", model_path))){
    # Remove trailing slashes
    model_path <- gsub("\\/+$", "", model_path)
  }

  if(!dir.exists(model_path)){
    stop("Model directory `", model_path, "` does not exist")
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_path, paste0(basename(model_path), ".rds"))
  if(!file.exists(rds_file)){
    stop("The RDS file `", rds_file,"` does not exist. You must create it ",
         "first by running the `create_rds_file()` function")
  }

  # by passing `ct_levels` which has those values as NA. `load_ct_levels()`
  # populates them with the values from the catch levels runs which were
  # run using reduction search type algorithms and have their results
  # located in forecast files in their respective run directories
  ct_levels_lst <- load_ct_levels(model_path, ...)

  model <- readRDS(rds_file)

  model$ct_levels <- ct_levels_lst$ct_levels
  model$ct_levels_vals <- ct_levels_lst$ct_levels_vals

  default_policy_ind <- ct_levels_lst$ct_levels_vals$ct_default_policy_ind
  model$ct_default_policy <-
    model$ct_levels[[default_policy_ind]][[1]]

  model$forecasts <- load_forecasts(model_path, ...)
  model$risks <- calc_risk(model, ...)
  # Remove "outputs" from forecasts lists after calculating risk
  model$forecasts <- remove_forecast_outputs(model$forecasts)

  saveRDS(model, file = rds_file)
  if(file.exists(rds_file)){
    dt <- now() - file.info(rds_file)$mtime
    message("RDS file `", rds_file, "` was created ",
            f(dt[[1]], 2), " ", units(dt), " ago\n\n")
  }else{
    stop("File was not created during the `saveRDS()` call")
  }

  invisible()
}