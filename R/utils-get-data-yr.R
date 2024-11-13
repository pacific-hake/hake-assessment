
#' Get the current data year
#'
#' @param data_yr_month_cutoff The final month in the year that you would still
#'   only want data from the previous year. Often times we might pull the data
#'   in March but we do not actually want to include data from January,
#'   February, and March of that year. The default is April, or 4, to be safe.
#'   Thus, if you are working in June of 1999, the data year will be 1999 but if
#'   you are working in February of 1999 the data year will be 1998 using the
#'   default input.
#'
#' @return
#' A four-digit integer is returned that specifies the year of interest.
#' @export
#' @author Kelli F. Johnson
get_data_yr <- function(data_yr_month_cutoff = 4) {
  sprintf("%02d", 0:data_yr_month_cutoff)
  as.numeric(format(Sys.Date(), "%Y")) -
    ifelse(
      test = format(Sys.Date(), "%m") %in% c("01", "02", "03", "04"),
      yes = 1,
      no = 0
    )
}
