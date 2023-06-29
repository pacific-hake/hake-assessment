#' Calculate the data outside the range of the y limits and change the
#' `lo_col` and `hi_col` in the data frame to be equal to the limits for
#' those points below or above
#'
#' @param d A data frame containing `lo_col`, `med_col`, and `hi_col`
#' @param ylim A vector of two values, a lower limit and an upper limits
#' @param lo_col The name of the column representing the lower values
#' @param med_col The name of the column representing the median values
#' @param hi_col The name of the column representing the upper values
#'
#' @return A list of length up to three,, containing the modified data
#' frame `d`, the data frame containing data below the lower `ylim` value
#' (`d_outside_lo`), and the data frame containing data above the higher
#' `ylim` value (`d_outside_hi`). If no data are below or above the `ylim`
#' values, `d_outside_lo` and/or `d_outside_hi` may not be present
#' @export
calc_yoob <- function(d,
                      ylim,
                      lo_col,
                      med_col,
                      hi_col){

  lo_col_sym <- sym(lo_col)
  med_col_sym <- sym(med_col)
  hi_col_sym <- sym(hi_col)

  lst <- list()
  # Fetch rows where the median is above the upper limit of the plot.
  # This will be used to add arrows later pointing to where the missing
  # points are outside the plot
  lst$d_outside_lo <- d |>
    filter(!!med_col_sym < ylim[1])

  lst$d_outside_hi <- d |>
    filter(!!med_col_sym > ylim[2])

  lst$d <- d |>
    mutate(across(c(!!lo_col_sym, !!hi_col_sym), ~{
      ifelse(.x < ylim[1],
             ylim[1],
             ifelse(.x > ylim[2],
                    ylim[2],
                    .x))}))

  lst$ylim <- ylim

  lst
}