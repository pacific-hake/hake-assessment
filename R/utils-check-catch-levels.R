#' Check that catch levels has been run
#'
#' @details
#' Ensure that the [run_catch_levels()] function has been run successfully
#' by looking in to the catch levels directory and checking values in the
#' forecast files
#'
#' @param model_path The directory the models are located in
#' @param forecast_yrs A vector of forecast years
#' @param ... Absorbs arguments meant for other functions
#'
#' @return Logical. `TRUE` if the catch levels appears to have been run
#' successfully and `FALSE` otherwise
#' @export
check_catch_levels <- function(model_path = NULL,
                               forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                               ...){

  if(is.null(model_path)){
    stop("`model_path` must not be `NULL`")
  }

  ct_levels_fullpath <- file.path(model_path, ct_levels_path)
  if(!dir.exists(ct_levels_fullpath)){
    return(FALSE)
  }

  lvls <- set_ct_levels(...)
  dir_nms <- map_chr(lvls$ct_levels, ~{.x[[3]]})
  dir_nms <- gsub("[0-9]+-(.*)", "\\1", dir_nms)
  dir_nms <- dir_nms[grep("[a-zA-Z]+", dir_nms)]
  dir_nms_fullpath <- file.path(model_path, ct_levels_path, dir_nms)
  if(!all(dir.exists(dir_nms_fullpath))){
    return(FALSE)
  }

  fore_fns_fullpath <- file.path(dir_nms_fullpath, forecast_fn)
  if(!all(file.exists(fore_fns_fullpath))){
    return(FALSE)
  }

  yr_ind_lst <- map(fore_fns_fullpath, \(fn){
    x <- readLines(fn)
    map_dbl(forecast_yrs, \(yr){
      j <- grep(paste0("[[:space:]]", yr, "[[:space:]]*1[[:space:]]*1"), x)
      if(!length(j)){
        return(FALSE)
      }
      if(length(j) > 1){
        stop("Found more than one match for year ", yr, " in the file ",
             fn)
      }
      j
    })
  })

  length_vector <- lengths(yr_ind_lst)
  if(length(length_vector) > 1 && var(length_vector)){
    # If not all the number of years in each forecast file are the same, FALSE
    # Uses variance as a bit of trickiness
    return(FALSE)
  }
  # Check that the correct number of years appears in the forecast files
  if(length_vector[1] != length(forecast_yrs)){
    return(FALSE)
  }

  TRUE
}