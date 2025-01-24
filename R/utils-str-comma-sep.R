#' Create a string in a comma-separated format from a vector
#'
#' @param v The vector of values
#'
#' @returns A single string
#' @export
str_comma_sep <- function(v, and_on_last = TRUE){

  if(is.null(v[1])){
    return(NULL)
  }

  if(length(v) == 1){
    return(v)
  }
  if(length(v) == 2){
    return(paste0(v[1], " and ", v[2]))
  }

  if(and_on_last){
    v[length(v)] <- paste0("and ", v[length(v)])
  }

  paste(v, collapse = ", ")
}