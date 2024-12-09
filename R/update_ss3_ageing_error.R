update_ss3_ageing_error <- function(input_list) {
  while (
    input_list[["dat"]][["N_ageerror_definitions"]] <
      max(input_list[["dat"]][["agecomp"]][["Ageerr"]])
  ) {
    input_list[["dat"]][["N_ageerror_definitions"]] <-
      input_list[["dat"]][["N_ageerror_definitions"]] + 1
    bias <- input_list[["dat"]][["ageerror"]][
      seq(2, NROW(input_list[["dat"]][["ageerror"]]), by = 2),
    ]
    bias_last_years_cohort <- which(
      round(bias[NROW(bias) - 1, ] / bias[NROW(bias), ], 2) == 0.55
    )
    input_list[["dat"]][["ageerror"]] <- rbind(
      input_list[["dat"]][["ageerror"]],
      calc_ageerror(bias_last_years_cohort + 1)
    )
  }
  return(input_list[["dat"]])
}
