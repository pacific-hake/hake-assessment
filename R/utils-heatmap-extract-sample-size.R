#' Extract the sample size data frame, or a data frame of logical values
#' for presence/absence of sample sizes if `ret_mask` is `TRUE`
#'
#' @param sample_size_df A data frame where ages are columns (and start with
#' the letter 'a'). If the values are zero, the weight-at-age was
#' extrapolated/interpolated. If there is a value, the weight-at-age is data
#' @param wa A weight-at-age data frame as created by [heatmap_extract_wa()]
#' @param ret_mask Logical. If `TRUE`, return a data frame the same
#' dimensions as `wa` which contains only the `yr` column and `TRUE`/`FALSE`
#' values in the table cells
#' @param ... Absorb arguments meant for other functions
#'
#' @return A data frame containing years (`yr`) column and columns for age,
#' represented as numbers (no text appended to them). If `ret_mask` is
#' `TRUE` then each cell is either `TRUE`or `FALSE` representing presence
#' or absence of samples for that year/age combination.The dimensions of the
#' returned data frame will be identical to the dimensions of `wa`.
#' If `ret_mask` is `FALSE`, the data frame returned will be the same
#' dimensions as `wa` plus two more rows and one more column. The two rows
#' are one row containing the year and all `NA`s (a blank row for the table)
#' and a row of the column totals, The extra column contains the row sums
heatmap_extract_sample_size <- function(sample_size_df = NULL,
                                        wa = NULL,
                                        ret_mask = FALSE,
                                        ...){

  stopifnot(!is.null(sample_size_df))
  stopifnot(!is.null(wa))
  #stopifnot(is.data.frame(wa))

  names(sample_size_df) <- tolower(names(sample_size_df))
  # Make age column names numeric (remove leading 'a')
  names(sample_size_df) <- gsub("^a(\\d+)$", "\\1", names(sample_size_df))

  bf <- sample_size_df %>%
    select(yr, matches("^\\d+", .)) |>
    dplyr::filter(yr > 0) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))

  # All years where all values are extrapolated (pre- and post- weight-at-age
  # data values)
  bf_missing_yrs <- wa$yr[!wa$yr %in% bf$yr]

  # Set those extrapolated years to a sample size of zero
  all_bf_vec <- rep(0, ncol(bf) - 1)
  df <- map_df(bf_missing_yrs, \(yr){
    vec2df(c(yr, all_bf_vec), names(bf))
  })

  # Add the zero-sample-size rows and arrange the data frame by year
  bf <- bf |>
    bind_rows(df) |>
    arrange(yr) |>
    mutate(yr = as.numeric(yr))

  if(ret_mask){
    # Return the TRUE/FALSE data frame for zero/non-zero sample sizes
    # which indicate interpolated/Non-interpolated values (zero/non-zero)
    bf <- bf |>
      mutate_at(vars(-yr), ~{!as.logical(.x)}) |>
      dplyr::filter(yr %in% unique(wa$yr))

    if(!identical(dim(bf), dim(wa))){
      stop("The structure (dimensions) of `bf` is not identical to the ",
           "structure of `wa`. Check `heatmap_extract_bf()`")
    }
    return(bf)
  }

  # Add row totals ----
  row_sums <- pmap_dbl(bf |> select(-yr), ~{
    list(...) |>
      as.numeric() |>
      sum()
  }) |>
    # Name the sum column `999` for now, to keep the "age" columns numeric
    enframe(name = NULL, value = "999")

  #' Set the first and second elements to `NA` so that nothing appears in
  #' row sums column for the first two cells
  row_sums[1, 1] <- NA
  row_sums[2, 1] <- NA

  # Add column totals ----
  # Add the sample size totals by age as a bottom row, and a blank row
  # before it
  blank_row <- c(min(bf$yr) + 1, rep(NA, ncol(bf) - 1)) |>
    vec2df(nms = names(bf))
  col_sums <- colSums(bf) |>
    vec2df() |>
    mutate(yr = as.integer(yr))
  col_sums[1] <- min(bf$yr)
  bf <- bf |>
    rows_update(blank_row) |>
    rows_update(col_sums) |>
    bind_cols(row_sums)

  bf
}
