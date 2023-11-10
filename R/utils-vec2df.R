#' Create a one-row data frame from a vector of values
#'
#' @details
#' If `nms` is not given, the names of the vector (if `vec` is a named vector)
#' will be used for the column names
#'
#' @param vec A vector
#' @param nms Optional column names for the new data frame
#'
#' @return A one row [tibble::tibble()]
#' @export
vec2df <- function(vec, nms = NULL){

  if(!is.null(nms) && length(vec) != length(nms)){
    stop("The number of names supplied does not match the number ",
         "of elements in `vec`")
    names(df) <- nms
  }

  # The code below .name_repair = ~vec_as_names(..., quiet = TRUE)
  # stops the "New names..." warnings and having the `.name_repair = `
  # is to avoid the warning "The `x` argument of `as_tibble.matrix()` must
  #  have unique column names if `.name_repair` is omitted as of tibble 2.0.0."
  d <- as_tibble(t(vec),  .name_repair = ~vec_as_names(..., quiet = TRUE))
  if(!is.null(nms)){
    names(d) <- nms
  }

  d
}
