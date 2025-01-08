#' Extract the number of age, length, and weight samples by fleet and
#' month for a given year
#'
#' @description
#' Used to make data frames for tables that show these summary totals
#'
#' @details
#' Creates a data frame of `month`, `num_age`, `num_length`, and `num_weight`
#' for each data frame in the `fleet_lst` list.
#'
#' @param fleet_lst A list of fleet samples. This list's format must match
#' the output of [canada_get_fleet_samples()]
#' @param yr The year to extract by-month sampling summary for
#'
#' @return A list of the same length as `fleet_lst` of data frames, explained
#' in details above
#'
#' @export
canada_extract_sampling_by_month <- function(fleet_lst,
                                             yr){

  d <- imap(fleet_lst, \(fleet_df, fleet_nm){
    x <- map(c("age", "length", "weight"), ~{
      col_sym <- sym(.x)
      col_new <- sym(paste0("num_", .x))
      fleet_df |>
        dplyr::filter(year == yr) |>
        mutate(month = month(trip_start_date)) |>
        dplyr::filter(!is.na(!!col_sym)) |>
        group_by(month) |>
        summarize(!!col_new := n()) |>
        ungroup()
    })

    j <- x[[1]] |>
      full_join(x[[2]], by = "month") |>
      full_join(x[[3]], by = "month") |>
      arrange(month) |>
      mutate(month = month.abb[month])

    colsums <- apply(j[, c("num_age",
                           "num_length",
                           "num_weight")],
                     2,
                     sum, na.rm = TRUE)

    colsums_row <- tibble(month = "Total",
                          num_age = colsums["num_age"],
                          num_length = colsums["num_length"],
                          num_weight = colsums["num_weight"])
    j |>
      bind_rows(colsums_row) |>
      mutate(across(everything(), ~{ifelse(is.na(.x), "--", .x)}))
  })

  d
}