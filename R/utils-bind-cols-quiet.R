#' Wrapper for [dplyr::bind_cols()] but silences the New names... messages
#' that appear when binding
#'
#' @param ... Arguments passed to [dplyr::bind_cols()]
#'
#' @return The output data frame from [dplyr::bind_cols()]
#' @export
bind_cols_quiet <- function(...){
  bind_cols(..., .name_repair = ~vec_as_names(..., quiet = TRUE))
}
