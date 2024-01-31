#' Create a table containing some years' estimated recruitment values
#' for specific cohorts
#'
#' @details
#' This is a table typically used in the assessment presentation, so that
#' recruitment estimates of string cohorts
#'
#' @param cohorts A vector of years, representing the cohorts to use in the
#' table
#' @param yrs A vector of assessment years to include in the table
#'
#' @return A [kableExtra::kbl()] table
#' @export
table_above_avg_cohorts_yrs <- function(cohorts = large_cohorts,
                                        yrs = c(last_assess_yr,
                                                assess_yr)){

  num_groups <- length(cohorts)
  num_rows <- num_groups * length(yrs)

  cohort_lst <-map(cohorts, ~{
    rep(.x, length(yrs))
    }) |>
    unlist()

  # Get the value for a variable of the format `rec_2020` or `rec_2023` which
  # must have already been created outside this function, and is a 3-element
  # list containing 1) the current year's quantiles (a vector of 4: lo, med, hi,
  # and interval size recruitment estimates) 2) the same as (1) but for the
  # previous assessment year, and 3)
  get_val <- \(x){
    map(cohorts, \(cohort){
      rec_lst <-get(paste0("rec_", cohort))
      imap(yrs, \(yr, ind){
        rec_lst[[ind]][x]
      })
    }) |>
      unlist()
  }

  lo <- get_val(1)
  med <- get_val(2)
  hi <- get_val(3)
  int_size <- get_val(4)

  d <- tibble(assess_year = rep(yrs, num_groups),
              cohort = cohort_lst,
              `Low (2.5\\%)` = lo,
              `Median` = med,
              `High (97.5\\%)` = hi,
              `Interval size` = int_size) |>
    mutate(`Calculation (billions)` = paste0(assess_year,
                                             " assessment recruitment in ",
                                             cohort)) |>
    select(-assess_year, -cohort) |>
    select(`Calculation (billions)`, everything())

  k <- kbl(d,
           format = "latex",
           booktabs = TRUE,
           align = c("l", "r", "r", "r", "r"),
           linesep = "",
           #col.names = col_names,
           escape = FALSE) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = 8)

  hline_locs <- seq_len(num_groups - 1) * length(yrs)
  walk(hline_locs, \(row_ind){
    k <<- k |>
      row_spec(row_ind, hline_after = TRUE)
  })

  k
}