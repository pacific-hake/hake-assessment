#' Link together [dplyr::mutate()] and [dplyr::select()]
#'
#' @details
#' This function allows you to [dplyr::mutate()] to create/modify columns,
#' and then [dplyr::select()] them so that the newly mutated columns are in
#' the order supplied to the function. Other remaining columns will follow in
#' the same order they appear in the original data frame `d`.
#'
#' @param d A data frame
#' @param ... The column names to mutate/select
#'
#' @return The modified data frame
#' @export
mutsel <- \(d, ...) {

  nms <- names(match.call(expand.dots = FALSE)$...)
  d |>
    mutate(...) |>
    select(one_of(nms),
           everything())
}