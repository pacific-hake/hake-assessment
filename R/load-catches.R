#' Load a CSV file with landings and TACS and create a nice [tibble::tibble()]
#' containing output
#'
#' @param fn The name of the landings/TAC CSV file
#'
#' @return A [tibble::tibble] with the formatted output. names ending in
#' '_xx' are not to be shown in the landings/TAC table.
#' @export
load_catches <- function(fn = NA){

  stopifnot(!is.na(fn))

  read_csv(fn, col_types = readr::cols()) |>
    as_tibble() |>
    transmute(
      year = Year,
      us_catch = Ustotal,
      can_catch = CANtotal,
      tot_catch = TOTAL,
      us_prop_tot_catch = us_catch / tot_catch * 100,
      can_prop_tot_catch = can_catch / tot_catch * 100,
      us_tac = TACUSA,
      can_tac = TACCAN,
      tot_tac = TAC,
      us_attain = us_catch / us_tac * 100,
      can_attain = can_catch / can_tac * 100,
      tot_attain = tot_catch / tot_tac * 100,
      us_research_xx = USresearch,
      us_cp_xx = atSea_US_CP,
      us_ms_xx = atSea_US_MS,
      us_shore_xx = US_shore,
      us_foreign_xx = US_foreign,
      us_jv_xx = US_JV,
      can_foreign_xx =  CAN_forgn,
      can_jv_xx = CAN_JV,
      can_shore_xx = CAN_Shoreside,
      can_freeze_xx = CAN_FreezeTrawl
    )
}
