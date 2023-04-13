#' Get best start year and end year values given the data minimum and maximum
#' years and suggested start and end year values
#'
#' @param start_yr The suggested start year
#' @param end_yr  The suggested end year
#' @param min_yr The data's minimum year
#' @param max_yr The data's maximum year
#'
#' @return A vector of two, the start and end year that is best
#' @export
get_year_range <- function(start_yr = NA,
                           end_yr = NA,
                           min_yr = NA,
                           max_yr = NA){
  if(is.null(min_yr[1]) ||
     is.null(max_yr[1]) ||
     is.na(min_yr[1]) ||
     is.na(max_yr[1])){
    stop("`min_yr` and `max_yr` cannot be `NA` or `NULL`",
         call. = FALSE)
  }
  s_yr <- min_yr
  e_yr <- max_yr

  if(!is.null(start_yr[1]) && !is.na(start_yr[1])){
    if(start_yr %in% min_yr:max_yr){
      s_yr <- start_yr
    }else{
      warning("`start_yr` = ", start_yr, " does not fall in the range ",
              "defined by min_yr = ", min_yr, " and `max_yr` = ", max_yr)
    }
  }
  if(!is.null(end_yr[1]) && !is.na(end_yr[1])){
    if(end_yr %in% min_yr:max_yr){
      e_yr <- end_yr
    }else{
      warning("`end_yr` = ", end_yr, " does not fall in the range ",
              "defined by min_yr = ", min_yr, " and `max_yr` = ", max_yr)
    }
  }
  if(e_yr < s_yr){
    stop("End year occurs before start year",
         call. = FALSE)
  }
  if(s_yr > e_yr){
    stop("End year occurs before start year",
         call. = FALSE)
  }
  c(start_yr = s_yr, end_yr = e_yr)
}
