#' Insert a new row made up of the vector `row_vec` at row `row_ind` in
# 'data frame `d`
#'
#' @param d A data frame
#' @param row_vec A vector which has the same number of elements as columns
#' in `d`
#' @param row_ind The row index to insert the new row at. Whatever row was
#' at that index before is pushed down, as are the ones following it
#' @param ... Arguments passed to [vec2df()]
#'
#' @return A data frame with the new row inserted
#' @export
insert_row <- function(d,
                       row_vec,
                       row_ind,
                       ...) {

  stopifnot(is.numeric(row_ind))
  stopifnot(length(row_ind) == 1)
  stopifnot(nrow(d) > 0)

  if(length(row_vec) != ncol(d)){
    stop("`row_vec` has ", length(row_vec), " elements and `d` has ",
         ncol(d), " columns. They must have the same number")
  }
  if(row_ind < 1 || row_ind > nrow(d)){
    stop("`row_ind` (", row_ind, ") is not within the range of the rows in ",
         "`d` (1-", nrow(d))
  }
  d[seq(row_ind + 1, nrow(d) + 1), ] <- d[seq(row_ind, nrow(d)),]
  d[row_ind, ] <- vec2df(row_vec, ...)
  d
}
