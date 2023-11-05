#' Get the object sizes in bytes for elements of the input list
#'
#' @param lst The input list
#'
#' @return A data frame with one column if the elements have no names and two
#' columns if there are names
#' @export
get_obj_sizes <- function(lst){

  j <- lst |>
    map_dbl(~{object.size(.x)}) |>
    sort() |>
    rev()
  k <- names(j)

  if(is.null(k[1])){
    enframe(j, name = NULL)
  }else{
    enframe(j, name = NULL) |>
      mutate(name = k) |>
      select(name, value)
  }
}