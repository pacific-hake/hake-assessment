#' Get the current assessment year
#'
#' @details
#' There is a bash script that does this as well. See
#' `hake/bash-scripts/get-assess-year.sh`
#'
#' @param assess_yr_month_cutoff The month where 1 will be added to
#' the assessment year. So if this is 12 and the current date is in December,
#' the assessment year will be the next year, which starts in January
#'
#' @return A year
#' @export
get_assess_yr <- function(assess_yr_month_cutoff = 12){

  dt <- Sys.Date()
  yr <- dt |>
    format("%Y") |>
    as.numeric()
  mn <- dt |>
    format("%m") |>
    as.numeric()

  if(mn == assess_yr_month_cutoff){
    yr <- yr + 1
  }

  yr
}
