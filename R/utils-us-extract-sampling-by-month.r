#' Extract the number of ages, lengths, and weights month for year by month
#'
#' @description
#' Used to make data frames for tables that show these summary totals.
#'
#' @param colnames A vector of column names for ages, lengths, and weights.
#' @param data A data frame of biological samples.
#' @param yr The year to extract by-month sampling summary for
#'
#' @return
#' A data frame with four columns, `month`, `num_age`, `num_length`,
#' and `num_weight`.
#'
#' @export
us_extract_sampling_by_month <- function(colnames, data, yr = last_data_yr) {
  without_sum <- purrr::map_dfr(
    colnames,
    \(x, the_data = data) {
      the_data |>
        dplyr::rename_with(.fn = \(x) gsub("SAMPLE_MONTH", "Month", x)) |>
        dplyr::rename_with(.fn = \(x) gsub("SAMPLE_YEAR", "Year", x)) |>
        dplyr::rename(test = {{x}}) |>
        dplyr::filter(Year == yr, !is.na(test)) |>
        dplyr::group_by(Month) |>
        dplyr::count() |>
        dplyr::mutate(
          type = dplyr::case_when(
            grepl("age", x, ignore.case = TRUE) ~ "num_age",
            grepl("length", x, ignore.case = TRUE) ~ "num_length",
            grepl("weight", x, ignore.case = TRUE) ~ "num_weight",
            TRUE ~ "Other"
          )
        )
    }
  ) |>
    tidyr::pivot_wider(names_from = type, id_cols = Month, values_from = n) |>
    dplyr::arrange(Month) |>
    dplyr::mutate(
      Month = month.abb[as.numeric(Month)],
      dplyr::across(dplyr::starts_with("num_"), as.numeric)
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(month = Month)

  out <- bind_rows(
    without_sum,
    summarise(
      without_sum,
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
      across(where(is.character), ~ "Total")
    )
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)
      )
    )
  
  return(out)
}
