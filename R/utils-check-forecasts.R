#' Check that forecasts have been run
#'
#' @details
#' Ensure that the [run_forecasts()] function has been run successfully
#' by looking in to the forecasts directory and checking values in the
#' forecast files
#'
#' @param model_path The directory the models are located in
#' @param forecast_yrs A vector of forecast years
#' @param ... Absorbs arguments meant for other functions
#'
#' @return Logical. `TRUE` if the catch levels appears to have been run
#' successfully and `FALSE` otherwise
#' @export
check_forecasts <- function(model_path = NULL,
                            forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                            ...){

  if(is.null(model_path)){
    stop("`model_path` must not be `NULL`")
  }

  fore_fullpath <- file.path(model_path, forecasts_path)
  if(!dir.exists(fore_fullpath)){
    return(FALSE)
  }

  dir_nms <- paste0(forecasts_prepend, forecast_yrs)
  dir_nms_fullpath <- file.path(model_path, forecasts_path, dir_nms)
  if(!all(dir.exists(dir_nms_fullpath))){
    return(FALSE)
  }

  ct_levels_lst <- set_ct_levels()
  ct_levels <- ct_levels_lst$ct_levels
  ct_levels_dir_nms <- map_chr(ct_levels, ~{.x[[3]]})
  ct_levels_dir_nms_fullpath <- file.path(dir_nms_fullpath, ct_levels_dir_nms)
  if(!all(file.exists(ct_levels_dir_nms_fullpath))){
    return(FALSE)
  }

  ct_levels_fns_fullpath <- file.path(ct_levels_dir_nms_fullpath, posts_fn)
  if(!all(file.exists(ct_levels_fns_fullpath))){
    return(FALSE)
  }

  TRUE
}