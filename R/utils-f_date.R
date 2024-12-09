#' Format dates
#'
#' Format a date object the same as [base::format()] but return a numerical
#' value when able instead of always returning a character string. Users
#' dictate what is returned by the same `format` argument that is used in
#' [base::format()].
#'
#' @param x A vector of dates with class `POSIXct`, `POSIXt`, or `Date`.
#' @param format A character value specifying the format you want
#'   the output to be in. See [base::strptime()] for options.
#' @param factor A logical specifying if you want a factor to be returned.
#'   The factor is *ALWAYS* limited to the levels that are available. The
#'   default behavior is to *NOT* return a factor. Thus, this must be invoked.
#'
#' @author Kelli F. Johnson
#' @seealso
#'   * [base::format()]
#'   * [base::factor()]
#'   * [base::droplevels()]
#'   * [base::strptime()]
#' @return A vector the same length as the input vector.
#'
f_date <- function(x, format = c("%m", "%Y"), factor = FALSE) {
  stopifnot(any(c("POSIXct", "POSIXt", "Date") %in% class(x)))
  out <- utils::type.convert(format(x, format), as.is = TRUE)
  if (factor) {
    out <- droplevels(factor(out))
  }
  return(out)
}
