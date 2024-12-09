#' Calculate age proportions for the acoustic age-2+ survey
#'
#' Calculate age proportions for the acoustic age-2+ survey based on
#' the estimated **abundance** at age extrapolated to the KS stratifications.
#' This information is produced by the acoustic team and stored on the
#' network in the `kriged_len_age_abundance_table` file.
#' The original code to generate these proportions was provided by
#' Allan Hicks and can be found on GitHub
#' <https://github.com/iantaylor-NOAA/hake-data/blob/master/Rcode/AcousticData2.R>.
#' Particularly lines 170-174 that explains the file location.
#' The actual code to compute the compositions is pretty trivial,
#' the difficulty is knowing what file to use.
#' As mentioned above, abundance is key here rather than biomass.
#'
#' @param year An integer specifying the year of interest.
#'   The default uses [hake::get_data_yr()] to get the current year for the data.
#' @param round An integer specifying how many digits should be printed.
#'   The default is to print the options for `digits` stored in your R session.
#' @examples
#' proportions <- pull_surveyageproportions(
#'   year = 2021, round = 2
#' )
#' # Values to paste into .dat file
#' glue::glue_collapse(proportions, sep = " ")
pull_surveyageproportions <- function(year = hake::get_data_yr(),
                                      round = options()$digits) {
  # Read file from the network
  ages <- readxl::read_excel(
    skip = 1,
    path = glue::glue("//nwcfile/fram/Survey.Acoustics/Survey Time Series Analysis/Historical Summary (for Kriging)/Outputs/Historical Outputs (KS Stratification with aged data)/with extrapolation/{year}/kriged_len_age_abundance_table.xlsx"),
    sheet = 3
  )

  # Get age 1 - 20 subtotals
  abundance <- ages[
    grepl("Subtotal", ages$"0"),
    grepl("^[1-9]+", colnames(ages))
  ] %>%
    unlist(., use.names = FALSE) %>%
    as.numeric()
  round(calc_proportions(abundance), digits = round)
}

calc_proportions <- function(data, maxcol = 15) {
  withplusgroup <- c(
    data[1:(maxcol - 1)],
    sum(data[maxcol:length(data)])
  )
  withplusgroup / sum(withplusgroup) * 100
}

pull_surveyrawproportion <- function(year, dir) {
  usfilename <- dir(
    pattern = "biodata_specimen\\.x|specimen_AGES\\.x",
    full.names = TRUE,
    path = glue::glue("{dir}\\{year}\\US\\")
  )
  cnfilename <- dir(
    pattern = "specimen_CAN_AGES|^biodata_specimen\\.x|biodata_specimen_C[a-zA-Z]+\\.x",
    full.names = TRUE,
    glue::glue("{dir}\\{year}\\CAN\\")
  )
  if (length(usfilename) == 0) {
    usfilename <- dir(
      pattern = "Ricker specimen.+\\.x",
      full.names = TRUE,
      path = "\\\\nwcfile\\fram\\Survey.Acoustics\\2003 Hake Sum WER\\00_Catch export & misc files to be placed_from xHD"
    )
  }
  usdata <- readxl::read_excel(
    path = usfilename,
    col_type = "text"
  ) %>%
    dplyr::select(dplyr::contains("age", ignore.case = TRUE)) %>%
    dplyr::rename_with(tolower)
  if (length(cnfilename) == 0 | year %in% c(2005, 2007)) {
    cndata <- usdata[0, ]
  } else {
    cndata <- readxl::read_excel(
      path = cnfilename,
      col_type = "text"
    ) %>%
      dplyr::select(dplyr::contains("age", ignore.case = TRUE)) %>%
      dplyr::rename_with(tolower)
  }
  col <- grep("age$", colnames(usdata), ignore.case = TRUE, value = TRUE)
  dplyr::full_join(usdata, cndata, by = col) %>%
    dplyr::group_by(age) %>%
    dplyr::filter(!is.na(age)) %>%
    dplyr::count() %>%
    dplyr::pull(n) %>%
    calc_proportions()
}
# pull_surveyrawproportions()
pull_surveyrawproportions <- function() {
  nwcfiledir <- "\\\\nwcfile\\fram\\Survey.Acoustics\\Survey Time Series Analysis\\Historical Summary (for Kriging)\\Biological"
  years <- dir(nwcfiledir, pattern = "^[0-9]{4}$", full.names = FALSE)
  # No US folder in 2003
  # No CAN folder in 2005, haul by numbers by year says 0 CAN trawls
  # No CAN folder in 2007, haul by numbers by year says 0 CAN trawls
  t(sapply(years, pull_surveyrawproportion, dir = nwcfiledir))
}
