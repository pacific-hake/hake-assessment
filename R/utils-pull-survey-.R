# Age compositions: ----

#' Calculate age proportions for the acoustic age-2+ survey
#'
#' Calculate age proportions for the acoustic age-2+ survey based on
#' the estimated **abundance** at age extrapolated to the KS stratifications.
#' This information is produced by the acoustic team and stored on google
#' drive in the `kriged_len_age_abundance_table` file.
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
#' @param dir The directory that stores the age-composition data that have been
#'   extrapolated. Default is `"G:/Shared drives/NMFS NWC FRAM FEAT and JTC/assessment age proportions"`.
#' @export
#' @examples
#' proportions <- pull_survey_age_proportions(
#'   year = 2021, round = 2
#' )
#' # Values to paste into .dat file
#' glue::glue_collapse(proportions, sep = " ")
pull_survey_age_proportions <- function(
  year = hake::get_data_yr(),
  round = options()$digits,
  dir = "G:/Shared drives/NMFS NWC FRAM FEAT and JTC/assessment age proportions"
) {
  # Read file from the shared google drive
  the_file <- glue::glue("{dir}/{year}/kriged_len_age_abundance_table.xlsx")
  if (!fs::file_exists(the_file)) {
    cli::cli_alert_warning(c(
      "The file {the_file} does not exist, it is probably a non-survey year."
    ))
    return()
  }
  ages <- readxl::read_excel(
    skip = 1,
    path = the_file,
    sheet = 3
  )

  # Get age 1 - 20 subtotals
  abundance <- unlist(
    ages[
      grepl("Subtotal", ages$"0"),
      grepl("^[1-9]+", colnames(ages))
    ],
    use.names = FALSE
  ) |>
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

# Weight at age: ----

#' Pull survey weight-at-age data from the NWFSC Network
#'
#' Bring both survey years and research years together but as of right now the
#' only research years available are 2016, 2017, and 2018 because the other
#' years do not have files on the network that I can grab. Alicia is working on
#' getting everything into a database but that work is going slow.
#'
#' @details
#' This function takes a while because it uses [fs::dir_ls()] to find the
#' appropriate file names when looking for the research-year data.
#' @param dir The directory of interest for the NWFSC Network Acoustic Survey
#'   data. The default is `"G:/Shared drives/NMFS NWC FRAM FEAT and JTC/survey biological data"`.
#' @param summary_file_name A string with the extension of xlsx that lives in
#'   the directory `Data Processing/Historic Biodata` beneath `dir`.
#' @export
pull_survey_weight_at_age <- function(
  dir = "G:/Shared drives/NMFS NWC FRAM FEAT and JTC/survey biological data",
  summary_file_name = "1995-2025_Survey_Biodata.xlsx"
) {
  summary_file <- fs::path(
    dir,
    summary_file_name
  )

  historical_data <- read.csv(here::here(
    "data-tables",
    "survey-weight-at-age.csv"
  )) |>
    dplyr::filter(Year < 1995)

  research_data <- pull_research_survey_file_names() |>
    dplyr::pull(id) |>
    purrr::map(read_research_year_bio_file) |>
    purrr::list_rbind()

  # Must read in haul data to get the date
  # the warnings do not impact the columns that are selected
  hauls <- readxl::read_excel(
    summary_file,
    sheet = "biodata_haul"
  ) |>
    dplyr::select(ship, survey, haul, eq_date_time) |>
    dplyr::distinct() |>
    dplyr::mutate(
      Date = as.Date(eq_date_time, tryFormats = "%m/%d/%Y"),
      Year = format(Date, "%Y"),
      Month = as.numeric(format(Date, "%m"))
    )
  # Read in bio data and combine with date from haul
  # You have to specify the col_types because the first few rows of the data do
  # not have weights and are blank so read_excel thinks that they are logical
  # and all weights get transcribed to 0 or 1. :(
  # The warning about C13 is the "maturity table" column and is not used.
  all <- readxl::read_excel(
    summary_file,
    sheet = "biodata_specimen",
    col_types = c(rep("numeric", 9), "text", rep("numeric", 5), rep("text", 4))
  ) |>
    dplyr::filter(
      # 22500 is hake
      species_code == 22500,
      !is.na(weight),
      !is.na(age)
    ) |>
    dplyr::left_join(
      y = hauls,
      by = c("survey", "haul", "ship")
    ) |>
    dplyr::mutate(
      Year = as.numeric(substr(survey, 1, 4)),
      Source = ship_to_source(ship),
      sex = dplyr::case_when(
        tolower(sex) == "f" ~ "F",
        tolower(sex) == "m" ~ "M",
        tolower(sex) == "x" ~ "U",
        sex == 1 ~ "F",
        sex == 2 ~ "M",
        sex == 3 ~ "U",
        TRUE ~ "U"
      )
    ) |>
    dplyr::rename(
      Weight_kg = weight,
      Length_cm = length,
      Age_yrs = age,
      Sex = sex
    ) |>
    dplyr::select(
      Source,
      Weight_kg,
      Sex,
      Age_yrs,
      Length_cm,
      Month,
      Year
    )

  out <- dplyr::bind_rows(
    historical_data,
    all,
    research_data
  ) |>
    dplyr::arrange(
      Source,
      Year,
      Month,
      Sex,
      Age_yrs
    )
  return(out)
}

pull_research_survey_file_names <- function(
  dir = "https://drive.google.com/drive/folders/1PIT1N3EYAcPsDRIyK80B2D_L5xwogMx8"
) {
  # The googlesheets4 package is unable to use a fixed directory (e.g., G:/) so
  # I am using this work around with googledrive to find research data that
  # are saved in gsheets
  target_folder <- googledrive::drive_get(googledrive::as_id(dir))
  all <- googledrive::drive_ls(
    target_folder,
    pattern = "length_age",
    type = "spreadsheet"
  )
  all
}

read_research_year_bio_file <- function(file_name) {
  raw_haul_data <- googlesheets4::read_sheet(
    ss = file_name,
    sheet = "haul_locations"
  )
  haul_data <- raw_haul_data |>
    dplyr::rename(
      Haul = haul,
      DateTime1 = eq_date_time
    ) |>
    dplyr::select(Haul, DateTime1) |>
    tidyr::fill(DateTime1, .direction = "updown")

  if (any(grepl("/", haul_data[["DateTime1"]]))) {
    format_date <- ifelse(
      grepl(
        haul_data[["DateTime1"]][[1]],
        pattern = "[0-9]{2}/[0-9]{2}/[0-9]{4}"
      ),
      "%m/%d/%Y",
      "%m/%d/%y"
    )
    haul_data[["DateTime1"]] <- as.Date(
      haul_data[["DateTime1"]],
      tryFormats = format_date
    )
  } else {
    which_are_na <- which(haul_data[["DateTime1"]] == 0)
    haul_data[["DateTime1"]][which_are_na] <- haul_data[["DateTime1"]][
      setdiff(seq(NROW(haul_data)), which_are_na)[1]
    ]
    #haul_data[["DateTime1"]] <- as.Date(
    #  as.numeric(haul_data[["DateTime1"]]),
    #  origin = "1899-12-30"
    #)
  }

  sheet_names <- googlesheets4::sheet_names(file_name)
  find_sheet <- sheet_names[
    sheet_names %in% c("spec_data", "AWT_spec_data", "MFT_spec_data")
  ]
  if (length(find_sheet) == 2) {
    raw_bio_data <- dplyr::bind_rows(
      googlesheets4::read_sheet(ss = file_name, sheet = "AWT_spec_data"),
      googlesheets4::read_sheet(ss = file_name, sheet = "MFT_spec_data")
    )
  } else {
    raw_bio_data <- googlesheets4::read_sheet(
      ss = file_name,
      sheet = "spec_data"
    )
  }

  bio_data <- raw_bio_data |>
    dplyr::rename_with(.fn = stringr::str_to_title) |>
    dplyr::left_join(
      y = haul_data,
      by = "Haul"
    ) |>
    dplyr::mutate(
      Month = as.numeric(format(DateTime1, "%m"))
    ) |>
    dplyr::mutate(
      Source = ship_to_source(Ship),
      Weight_kg = Weight,
      Age_yrs = Age,
      Length_cm = Length,
      Sex = dplyr::case_when(
        tolower(Sex) %in% c("f", "female") ~ "F",
        tolower(Sex) %in% c("m", "male") ~ "M",
        tolower(Sex) %in% c("x", "u", "unsexed") ~ "U",
        Sex == 1 ~ "F",
        Sex == 2 ~ "M",
        Sex == 3 ~ "U",
        TRUE ~ "U"
      ),
      Year = as.numeric(format(DateTime1, "%Y"))
    ) |>
    dplyr::select(
      Source,
      Weight_kg,
      Sex,
      Age_yrs,
      Length_cm,
      Month,
      Year
    ) |>
    dplyr::filter(!is.na(Weight_kg)) |>
    dplyr::filter(!is.na(Age_yrs))
  return(bio_data)
}


ship_to_source <- function(x) {
  dplyr::case_when(
    x %in% c(499, 584, 2026) ~ "Canada Acoustic",
    x %in% c(19, 21, 160, 499) ~ "U.S. Acoustic",
    TRUE ~ "U.S. Acoustic"
  )
}
