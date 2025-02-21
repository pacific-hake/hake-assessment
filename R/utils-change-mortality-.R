#' Change natural mortality in a control file to be age-specific
#'
#' @param ctl An input list from r4ss with the control file.
#' @param n An integer specifying the number of breakpoints.
#' @param regular_expression A regular expression to search for in the
#'   parameter names.
#' @param estimate A logical specifying if the natural mortality base-line
#'   parameters should be estimated or fixed.
change_mortality_ctl <- function(ctl,
                                 n = 6,
                                 regular_expression = "NatM",
                                 estimate = TRUE) {
  ctl[["natM_type"]] <- 1
  ctl[["N_natM"]] <- n
  # age(real) at M breakpoints
  ctl[["M_ageBreakPoints"]] <- 1:n
  ctl[["N_natMparms"]] <- ctl[["N_natM"]]
  parameters <- ctl[["MG_parms"]] |>
    tibble::rownames_to_column("Label") |>
    dplyr::filter(grepl(regular_expression, Label)) |>
    dplyr::mutate(count = n) |>
    tidyr::uncount(count) |>
    dplyr::mutate(
      Label = paste0(gsub("1$", "", Label), 1:n),
      INIT = 0.13,
      `env_var&link` = 201:(200 + n),
      PHASE = ifelse(estimate, abs(PHASE), abs(PHASE) * -1)
    )
  parameters_other <- ctl[["MG_parms"]] |>
    tibble::rownames_to_column("Label") |>
    dplyr::filter(!grepl(regular_expression, Label))
  ctl[["MG_parms"]] <- dplyr::bind_rows(
    parameters,
    parameters_other
  ) |>
    tibble::column_to_rownames("Label")
  ctl[["MG_parms_tv"]] <- data.frame(
    Label = paste0(parameters[["Label"]], "_tv"),
    LO = -5,
    HI = 5,
    INIT = 1,
    PRIOR = 0,
    PRIOR_SD = 0,
    PR_type = 0,
    PHASE = -1
  ) |>
    tibble::column_to_rownames("Label")
  return(ctl)
}

#' Add natural mortality information in the data file to allow for deviations
#'
#' @param dat A data file list read in by r4ss.
#' @param n An integer specifying the number of deviations.
#' @param data A data frame of deviations.
#' @param years The years that those deviations pertain to.
change_mortality_dat <- function(dat, n = 6, data, years) {
  dat[["N_environ_variables"]] <- n
  data <- data |>
    dplyr::filter(
      age <= n,
      year %in% years
    ) |>
    dplyr::select(year, variable = age, value = M2)
  dat[["envdat"]] <- data |>
    dplyr::group_by(variable) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
    dplyr::full_join(
      data |>
        tidyr::expand(year = years[!years %in% data[["year"]]], variable),
      by = "variable"
    ) |>
    dplyr::full_join(data, by = colnames(data)) |>
    dplyr::select(year, variable, value) |>
    dplyr::arrange(year, variable) |>
    as.data.frame()
  return(dat)
}
