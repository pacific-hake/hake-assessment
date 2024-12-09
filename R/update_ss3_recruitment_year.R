update_ss3_recruitment_year <- function(input_list) {
  # TODO: think about how to do this if only age-2+ survey
  input_list[["ctl"]][["MainRdevYrLast"]] <- dplyr::filter(
    input_list[["dat"]][["CPUE"]],
    index > 0
  ) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::mutate(new = year - ifelse(index == 3, 1, 2)) %>%
    dplyr::arrange(-index) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(new)
  return(input_list[["ctl"]])
}
