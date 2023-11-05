#' Calculate and insert columns containing arbitrary quantiles for a
#' particular column
#'
#' @details
#' Uses the [probs] vector which is included in the package data for
#' this package
#'
#' @param df A [data.frame()]
#' @param col A column name on which to perform the calculations. Must
#' be in `df` or an error will be thrown
#' @param include_mean Logical. If `TRUE`, include the mean in the output
#'
#' @return A [data.frame()] with a new column for each value in the `probs`
#' vector
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' yrs <- sort(unique(pq$year))
#' df <- pq %>%
#'   group_by(year) %>%
#'   group_map(~ calc_quantiles(.x, col = "val", probs = probs)) %>%
#'   map_df(~{.x}) %>%
#'   mutate(year = yrs) %>%
#'   select(year, everything())
calc_quantiles <- function(df = NULL,
                           col = NULL,
                           include_mean = TRUE){

  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")
  col_sym <- sym(col)
  out <- summarize_at(df,
                      vars(!!col_sym),
                      map(probs,
                          ~partial(quantile, probs = .x, na.rm = TRUE)) |>
                        set_names(probs))

  if(include_mean){
    out <- out |>
      mutate(avg = mean(df[[col]]))
  }
  out
}

#' Calculate quantiles across groups for a given column
#'
#' @rdname calc_quantiles
#'
#' @param df A [data.frame] with columns with names given by `grp_col`
#' and `col`
#' @param grp_col The column name to use for grouping the data
#' @param col The column name to use as values to calculate quantiles for
#' @param include_mean If TRUE, include the mean in the output
#' @param grp_names The column name to use for labeling the grouped column. By
#' default it is the same as the
#' grouping column (`grp_col`).
#'
#' @return A [data.frame()] containing the quantile values with one row per
#' group represented by `grp_col`
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' j <- calc_quantiles_by_group(pq,
#'                              grp_col = "year",
#'                              col = "val",
#'                              probs = probs)
calc_quantiles_by_group <- function(df = NULL,
                                    grp_col = NULL,
                                    col = NULL,
                                    grp_names = grp_col,
                                    include_mean = TRUE){

  stopifnot(grp_col %in% names(df))
  stopifnot(col %in% names(df))

  grp_col_sym <- sym(grp_col)
  grp_names_sym <- sym(grp_names)
  col_sym <- sym(col)
  grp_vals <- unique(df[[grp_names]])

  df |>
    group_by(!!grp_col_sym) |>
    group_map(~ calc_quantiles(.x, col = col,
                               include_mean = include_mean)) |>
    map_df(~{.x}) |>
    mutate(!!grp_names_sym := grp_vals) |>
    select(!!grp_names_sym, everything()) |>
    ungroup()
}
