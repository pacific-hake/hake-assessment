update_ss3_selectivity <- function(input_list) {
  input_list[["ctl"]][["age_selex_parms"]][, "dev_maxyr"] <- ifelse(
    input_list[["ctl"]][["age_selex_parms"]][, "dev_maxyr"] == 0,
    0,
    max(input_list[["dat"]][["agecomp"]][, "Yr"])
  )
  return(input_list[["ctl"]])
}
