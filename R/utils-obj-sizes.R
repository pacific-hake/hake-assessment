#' Return a data frame of the names and sizes of the objects in a list
#'
#' @param lst The list
#' @param scale A scale factor. If 1e3, return values in kB, if 1e6, GB,
#' 1e12, TB
#' @param digits The number of decimal places to show
#' @param desc Logical. If `TRUE`, sort the table in order of largest size
#' in the first row and smallest in the last row
#'
#' @return A data frame
#' @export
obj_sizes <- function(lst,
                      scale = c("b", "kb", "mb", "gb", "tb"),
                      digits = 0,
                      desc = TRUE){

  scale <- match.arg(scale)

  if(scale == "b"){
    scale <- 1
    sc <- "B"
  }else if(scale == "kb"){
    scale <- 1e3
    sc <- "kB"
  }else if(scale == "mb"){
    scale <- 1e6
    sc <- "MB"
  }else if(scale == "gb"){
    scale <- 1e9
    sc <- "GB"
  }else if(scale == "tb"){
    scale <- 1e12
    sc <- "TB"
  }

  size_col_nm <- paste0("Size (", sc, ")")
  size_col_sym <- sym(size_col_nm)

  lst |>
    map_dbl(~as.numeric(object.size(.x)) / scale) |>
    sort(decreasing = desc) |>
    enframe() |>
    setNames(c("List element", size_col_nm)) |>
    mutate(!!size_col_sym := f(!!size_col_sym, digits))
}
