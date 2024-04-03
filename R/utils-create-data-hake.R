#' Create package data with one call
#'
#' @details
#' Usually you have to create a variable first, and then call
#' [usethis::use_data()] with the variable name as an argument and the
#' package data will be created. This function just unifies that into one step
#'
#' @param var The variable name
#' @param val The value for variable `var`
#' @param ... Additional arguments for [usethis::use_data()]
#'
#' @return Nothing, creates package data. See [usethis::use_data()]
#' @export
create_data_hake <- function(var, val, ...){

  assign(var, val)
  do.call(usethis::use_data,
          list(as.name(var),
               overwrite = TRUE,
               ...))
}
