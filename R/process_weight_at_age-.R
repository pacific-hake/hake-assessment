#' Get the weight-at-age data from the NWFSC server
#'
#' @param savedir A string specifying the path of interest.
#'
#' @export
#' @author Kelli F. Johnson
#'
process_weight_at_age_survey <- function(savedir = hakedata_wd()) {
  final_data <- pull_survey_weight_at_age() |>
    dplyr::arrange(Source, Year, Month, Sex, Age_yrs) |>
    as.data.frame()
  file_path <- fs::path(savedir, "survey-weight-at-age.csv")

  utils::write.csv(
    x = final_data,
    file = file_path,
    quote = FALSE,
    row.names = FALSE
  )
  return(invisible(final_data))
}
#' Get the weight-at-age data from the commercial fisheries
#'
#' After reading in the saved `.Rdat` files from each database, the data
#' is massaged and anything from 2008 or newer is changed.
#' @param savedir A string specifying the path of interest.
#'
#' @export
#' @author Kelli F. Johnson
#'
process_weight_at_age_us <- function(savedir = hakedata_wd()) {
  base::load(file.path(savedir, "extractedData", "nages.Rdat"))
  base::load(file.path(savedir, "extractedData", "page.Rdat"))
  final_data <- dplyr::bind_rows(
    US_atsea = nages |>
      dplyr::rename(
        Weight_kg = "WEIGHT",
        Sex = "SEX",
        Age_yrs = "AGE",
        Length_cm = "LENGTH"
      ) |>
      dplyr::mutate(
        Year = as.numeric(Year, as.is = TRUE),
        Month = as.numeric(Month, as.is = TRUE)
      ) |>
      dplyr::select(Weight_kg, Sex, Age_yrs, Length_cm, Month, Year),
    US_shore = page |>
      dplyr::rename(
        Sex = "SEX",
        Age_yrs = "AGE",
        Month = "SAMPLE_MONTH",
        Year = "SAMPLE_YEAR"
      ) |>
      dplyr::mutate(
        Weight_kg = FISH_WEIGHT / 1000,
        Length_cm = FISH_LENGTH / 10
      ) |>
      dplyr::select(Weight_kg, Sex, Age_yrs, Length_cm, Month, Year),
    .id = "Source"
  ) |>
    dplyr::filter(
      !is.na(Age_yrs),
      !is.na(Weight_kg)
    ) |>
    dplyr::arrange(Source, Year, Month)
  write.csv(
    x = final_data |> as.data.frame(),
    file = fs::path(savedir, "us-weight-at-age.csv"),
    quote = FALSE,
    row.names = FALSE
  )
}
