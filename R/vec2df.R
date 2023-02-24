#' Create a one-row data.frame from a vector of values
#'
#' @details
#' If `nms` is not given, the names of the vector (if `vec` is a named vector)
#' will be used for the column names
#' This function should not be used for massive amounts of conversions in
#' a loop as it has a bad time complexity
#'
#' @param vec A vector
#' @param nms Optional column names for the new data frame
#'
#' @return A [tibble::tibble()] with one row
#' @export
vec2df <- function(vec, nms = NULL){

  if(!is.null(nms) && length(vec) != length(nms)){
    stop("The number of names supplied does not match the number ",
         "of elements in `vec`", call. = FALSE)
    names(df) <- nms
  }

  df <- vec |>
    enframe(name = NULL) |>
    t() |>
    as_tibble()

  if(is.null(nms)){
    if(!is.null(names(vec))){
      names(df) <- names(vec)
    }
    return(df)
  }else{
    names(df) <- nms
  }

  df
}
