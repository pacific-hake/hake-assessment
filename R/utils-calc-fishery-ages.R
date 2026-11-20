#' Load data and create age compositions for the fishery
#'
#' Uses loaded age data from each fleet to create the age-composition data for
#' the Stock Synthesis file after weighting by catch and weight-at-age. The
#' weight-at-age information is passed as a function argument and subset for
#' fleet 1 and the catch data is available internally in the package.
#' @details
#' TODO:
#' 1. Weight-at-age is summed over all ages 0--20 but ages are not available
#'    for 16+?
#' @param weight_at_age A data frame read in by [r4ss::SS_readwtatage()] for
#'   the current year's model.
#' @author Kelli F. Johnson
#' @return
#' A data frame is returned that is appropriate for using in a Stock Synthesis
#' data file for the age-composition section.
calc_fishery_ages <- function(weight_at_age) {
  # convert the weight-at-age values from the model in a long table
  wtatage_repo <- weight_at_age |>
    dplyr::filter(fleet == 1) |>
    dplyr::select(year, dplyr::matches("^[0-9]+$")) |>
    tidyr::pivot_longer(names_to = "age", values_to = "weight", cols = -year)

  # ct is a table of catch by sector, TAC, and proportions for all model years
  # create a long table of catch by sector and year
  catch_repo <- ct |>
    dplyr::select(
      -dplyr::matches("tot|TAC|attain|_catch|_prop", ignore.case = TRUE)
    ) |>
    dplyr::rename(year = Year) |>
    tidyr::pivot_longer(
      names_to = c("sector"),
      values_to = "catch",
      cols = -year
    )

  # bind together all age data sources which include year, number of fish,
  # number of samples, and proportion by age 1-15 and label by sector
  age_repo <- dplyr::bind_rows(
    "Canada Freezer-trawler" = can_ft_age_df,
    "Canada Joint-venture" = can_jv_age_df,
    "Canada Shoreside" = can_ss_age_df,
    "U.S. Catcher-processor" = us_cp_age_df,
    "U.S. Mothership" = us_ms_age_df,
    "U.S. Shoreside" = us_sb_age_df,
    .id = "sector"
  )

  # 1. Join the catch and age data by sector and convert to a long data table
  # 2. Join the catch and age data with the weight-at-age estimates by age and year
  # 3. Calculate expansion factor (prop) by year and sector by:
  # (catch by sector, year * the proportion of age comps by age, sector, year) /
  # (sum across all ages(proportion-at-age * weight-at-age))
  # 4. Sum the "prop" across sectors by year and age
  # 5. Convert to proportions (sums to 100)
  # 6. Format composition data for SS3
  final <- dplyr::left_join(
    age_repo,
    catch_repo,
    by = c("sector", "year")
  ) |>
    tidyr::pivot_longer(
      names_prefix = "a",
      names_to = "age",
      values_to = "numbers",
      cols = dplyr::matches("^[0-9]+$")
    ) |>
    dplyr::left_join(wtatage_repo, by = c("age", "year")) |>
    dplyr::group_by(year, sector) |>
    dplyr::mutate(
      numerator = (catch * numbers),
      denominator = sum(numbers * weight),
      # catch is the sector catch, numbers is the proportion by age, and
      # weight is the weight-at-age
      prop = (catch * numbers) / sum(numbers * weight)
    ) |>
    dplyr::group_by(year, age) |>
    dplyr::summarize(
      prop = sum(prop, na.rm = TRUE)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(prop = prop.table(prop) * 100) |>
    dplyr::filter(!is.na(prop)) |>
    dplyr::arrange(age = as.numeric(age)) |>
    tidyr::pivot_wider(names_from = age, values_from = prop) |>
    dplyr::left_join(
      x = age_repo |>
        dplyr::group_by(year) |>
        dplyr::summarize(n = sum(num_samples)),
      by = "year"
    ) |>
    dplyr::mutate(
      month = 7,
      fleet = 1,
      sex = 0,
      part = 0,
      ageerr = year - 1972,
      Lbin_lo = -1,
      Lbin_hi = -1,
      .after = year
    )
  return(final)
}
