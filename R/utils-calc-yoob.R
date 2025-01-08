#' Calculate the data outside the range of the y limits and change the
#' `lo_col` and `hi_col` in the data frame to be equal to the limits for
#' those points below or above.  YOOB means Y-axis Out-Of-Bounds
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
                      hi_col,
                      show_arrows = TRUE){

  lo_col_sym <- sym(lo_col)
  med_col_sym <- sym(med_col)
  hi_col_sym <- sym(hi_col)

  lst <- list()

  lst$d <- d |>
    mutate(across(c(!!lo_col_sym, !!hi_col_sym), ~{
      ifelse(.x < ylim[1],
             ylim[1],
             ifelse(.x > ylim[2],
                    ylim[2],
                    .x))}))

  # Fetch rows where the median is above the upper limit of the plot.
  # This will be used to add arrows later pointing to where the missing
  # points are outside the plot
  if(show_arrows){
    lst$lo_outside <- d |>
      dplyr::filter(!!med_col_sym < ylim[1])
  }else{
    lst$d <- lst$d |>
      mutate(across(!!med_col_sym, ~{
        ifelse(.x < ylim[1],
               ylim[1],
               .x)}))
  }

  if(show_arrows){
    lst$hi_outside <- d |>
      dplyr::filter(!!med_col_sym > ylim[2])
  }else{
    lst$d <- lst$d |>
      mutate(across(!!med_col_sym, ~{
        ifelse(.x > ylim[2],
               ylim[2],
               .x)}))
  }

  lst$ylim <- ylim

  lst
}