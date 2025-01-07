#' Insert ageing error matrix if needed
#'
#' Determine if an additional ageing error matrix is needed and if so, then
#' add it to the list.
#' @param data_list A list read in by [r4ss::SS_readdat()].
#' @return
#' A list with the same structure as the list that was input.
#' @author Kelli F. Johnson
#' @export
update_ss3_ageing_error <- function(data_list) {
  while (data_list[["N_ageerror_definitions"]] <
    max(data_list[["agecomp"]][["ageerr"]])) {
    data_list[["N_ageerror_definitions"]] <-
      data_list[["N_ageerror_definitions"]] + 1
    bias <- data_list[["ageerror"]][
      seq(2, NROW(data_list[["ageerror"]]), by = 2),
    ]
    bias_last_years_cohort <- which(
      round(bias[NROW(bias) - 1, ] / bias[NROW(bias), ], 2) == 0.55
    )
    data_list[["ageerror"]] <- rbind(
      data_list[["ageerror"]],
      calc_ageerror(bias_last_years_cohort + 1)
    )
  }
  return(data_list)
}
