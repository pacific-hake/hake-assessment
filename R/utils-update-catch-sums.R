#' Updates the most recent catch time values stored in data-tables/<>.csv
#'
#' Read monthly catch time series for each fleet and report their summary
#' values in the appropriate csv files stored in data-tables.
#'
#' @param dir_data A path leading to data-tables where the csv files for the
#'   stock assessment are stored.
#' @param digits An integer specifying how many significant digits you want
#'   to save in the stored files. The default is 5.
#' @param values_prev_assess A vector of three values,
#'   * estimate of spawning biomass for terminal year in 1000s mt,
#'   * estimate of depletion in percent with one decimal place,
#'   * estimate of TAC for the first year of the forecast period in mt
#'     rounded to the nearest whole number.
#'
#' @return
#' The following two files are updated in the data-tables:
#' * landings-tac-history.csv
#' * catch-targets-biomass.csv
#' @author Kelli F. Johnson
utils_update_catch_sums <- function(dir_data = here::here("data-tables"),
                                    digits = 5,
                                    values_prev_assess = c(NA, NA, NA)) {
  # Files that are read in and written back out
  file_lan <- fs::path(dir_data, "landings-tac-history.csv")
  file_tar <- fs::path(dir_data, "catch-targets-biomass.csv")

  # Catch data
  data <- fs::dir_ls(dir_data, regexp = "-.*-catch-by-month") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      fleet_name = gsub(".+[canus]{2,3}-([a-zA-Z]+)-.+", "\\1", value),
      data = purrr::map2(
        value,
        fleet_name,
        .f = \(x, y) {
          utils::read.csv(x) |>
            dplyr::mutate(fleet = y) |>
            dplyr::rename_with(.f = \(x) gsub("Catch.+", "catch", x))
        }
      )
    ) |>
    dplyr::filter(fleet_name != "ti") |>
    dplyr::filter(fleet_name != "catch") |>
    dplyr::pull(data) |>
    dplyr::bind_rows() |>
    dplyr::group_by(fleet, Year = year) |>
    dplyr::summarise(
      catch = round(sum(catch, na.rm = TRUE), digits = digits)
    ) |>
    dplyr::mutate(
      catch = tidyr::replace_na(catch, 0),
      fleet = dplyr::case_when(
        fleet == "ms" ~ "U.S. Mothership",
        fleet == "cp" ~ "U.S. Catcher-processor",
        fleet == "shore" ~ "U.S. Shoreside",
        fleet == "research" ~ "U.S. Research",
        fleet == "ss" ~ "Canada Shoreside",
        fleet == "ft" ~ "Canada Freezer-trawler",
        fleet == "jv" ~ "Canada Joint-venture",
        TRUE ~ "Unknown - fix me"
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = fleet, values_from = catch)

  # Create new landings-tac-history.csv file
  tac_data <- utils::read.csv(file_lan, check.names = FALSE)
  new_data <- dplyr::left_join(
    x = dplyr::filter(data, Year >= 2008),
    y = dplyr::filter(tac_data, Year >= 2008),
    by = "Year",
    suffix = c("", "yy")
  ) |>
    dplyr::select(!dplyr::ends_with("yy"))
  tac_out <- dplyr::bind_rows(
    dplyr::filter(tac_data, !(Year %in% new_data[["Year"]])),
    new_data
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::matches("TAC"), as.character),
      dplyr::across(dplyr::matches("TAC"), ~ tidyr::replace_na(., "")),
      dplyr::across(dplyr::matches("[FJMCSR][oaer]"), ~ tidyr::replace_na(., 0))
    ) |>
    dplyr::arrange(Year)

  # Create new catch-targets-biomass.csv file
  check_year <- utils::read.csv(file_tar, check.names = FALSE)
  tar <- utils::read.csv(file_tar, check.names = FALSE) |>
    dplyr::select(!dplyr::matches("^TAC$|Total TAC")) |>
    dplyr::right_join(
      tac_out %>%
        dplyr::filter(Year >= min(check_year[, "Year"])) %>%
        dplyr::mutate(
          TOTAL = rowSums(
            dplyr::across(
              .cols = c(-Year, -`U.S. TAC`, -`Canada TAC`, -`Total TAC`)
            ),
            na.rm = TRUE
          )
        ) |>
        dplyr::select(Year, TOTAL, `Total TAC`),
      by = "Year"
    ) |>
    # todo: should this be round instead of sprintf?
    dplyr::mutate(`Realized catch` = sprintf("%.0f", TOTAL)) %>%
    dplyr::select(-TOTAL) %>%
    dplyr::relocate(`Realized catch`, `Total TAC`, .after = Year) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(~ tidyr::replace_na(., ""))

  if (!all(is.na(values_prev_assess))) {
    tar[
      tar[, "Year"] == max(tar[, "Year"]),
      c("Biomass estimate", "Depletion", "TAC")
    ] <- values_prev_assess
  }

  # Write files back to the disk
  utils::write.table(
    x = tac_out,
    file = file_lan,
    row.names = FALSE,
    sep = ",",
    quote = FALSE
  )
  utils::write.table(
    x = tar,
    file = file_tar,
    row.names = FALSE,
    sep = ",",
    quote = FALSE
  )
  return(invisible())
}
