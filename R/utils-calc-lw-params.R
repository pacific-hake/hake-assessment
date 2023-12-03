#' Calculate length-weight relationship parameters
#'
#' @description
#' Calculate length-weight relationship parameters for the data with both
#' length and weight data, given one or more grouping variables.
#'
#' @details
#' If the column `lw_alpha` already exists in the input data frame `d`,
#' the calculated parameters `lw_alpha` and `lw_beta` will be coalesced
#' with the input ones. This means they will be combined together similar to
#' a union or left join. The input parameter values will take precedence and
#' the `NA` values found in that column will be replaced with the parameter
#' estimates calculated here with [fit_lw()].
#'
#' @param d A data frame as returned by [gfdata::get_commercial_samples()]
#' @param grouping_cols A vector of character strings matching the names
#'  of the columns you want to group for. i.e. `"year"` or `c("year", "month")`
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#'
#' @return The original data frame with the columns `lw_alpha` and `lw_beta`
#' added (if necessary). If those columns exist, they will be overwritten
#' with `NA`s and values calculated where data exist
#'
#' @export
calc_lw_params <- function(d,
                           grouping_cols = "year",
                           lw_tol = 0.1,
                           lw_maxiter = 1e3){

  coalesce_after <- "lw_alpha" %in% names(d)

  grouping_cols_df <- d |>
    select(grouping_cols) |>
    unique()

  x <- d |>
    group_by_at(vars(one_of(grouping_cols))) |>
    group_map(~ fit_lw(.x, lw_tol, lw_maxiter)) %>%
    set_names(1:length(.))

  y <- do.call(rbind, x) |>
    as_tibble() |>
    setNames(c("lw_alpha", "lw_beta")) |>
    bind_cols(grouping_cols_df) |>
    select(grouping_cols, lw_alpha, lw_beta)

  out <- d |>
    left_join(y, by = grouping_cols)

  if(coalesce_after){
    out <- out |>
      mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
             lw_beta = coalesce(lw_beta.x, lw_beta.y)) |>
      select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))
  }

  out
}
