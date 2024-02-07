#' Update the catches in a Stock Synthesis file
#'
#' @details
#' Information from `landings-tac-history.csv` is summed and integrated into
#' the catch portion of a Stock Synthesis data file. The terminal year of
#' data becomes the same as the terminal year of the catch time series.
#'
#' @param file_ss3 A file path to the Stock Synthesis data file that you want to
#'   integrate catches into.
#' @author Kelli F. Johnson
#' @return
#' A list containing the Stock Synthesis data file is returned.
#' @export
update_ss3_catch <- function(file_ss3) {
  lan <- utils::read.csv(
    file = fs::path("data-tables", landings_tac_fn),
    check.names = FALSE
  ) |>
    dplyr::select(-dplyr::matches("TAC")) |>
    dplyr::group_by(Year) |>
    summarize(catch = rowSums(across(where(is.numeric)))) |>
    dplyr::mutate(
      seas = 1,
      fleet = 1,
      catch_se = 0.01
    ) |>
    dplyr::rename(year = Year)
  dat <- r4ss::SS_readdat(file_ss3, verbose = FALSE)
  dat[["catch"]] <- dplyr::bind_rows(
    # Keep the equilibrium catch value as specified if it exists
    dplyr::filter(dat[["catch"]], year < 0),
    # Read in catches
    lan
  )
  dat[["endyr"]] <- max(lan[["year"]])
  return(dat)
}
