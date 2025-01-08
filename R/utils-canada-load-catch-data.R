#' Load Canadian catch data from files extracted using the FOS web
#' interface
#'
#' @details
#' The web interface is queried each year with the DMP landings from April 1,
#' 2007 to present being in one file called `LandingsSpeciesDateDMP.csv` and
#' N files, 1 for each year or LOGS data (logbook discards) with names in
#' the form `LogCatchReportXXXX.csv` with `XXXX` being the four digit year
#' from 2007 to present. These must be separate files due to limitations of
#' the web software, it cannot process all the years in one query. One new
#' `LOGS` file is added each year and the `DMP` file is re-generated each year.
#'
#' @param dr The directory name where the files reside
#' @param min_date Earliest date to include in the data returned
#'
#' @return A list of four named data frames, `dmp_df` for the outside DMP data,
#' `dmp_inside_df` for the inside DMP data,
#' `logs_df` for the LOGS data, and `logs_inside_df` for the area 4B data
#' which were not included. `logs_inside_df` is returned as a convenience
#' for you to see if any other data from area 4B should be included in the
#' assessment
#'
#' @export
canada_load_catch_data <- function(dr = "/srv/hake/other/landings/canada",
                                   min_date = as.Date("2007-04-01")){

  if(!dir.exists(dr)){
    stop("The supplied directory `dr` = `", dr, "` does not exist")
  }

  dmp_fn <- file.path(dr, "LandingsSpeciesDateDMP.csv")
  if(!file.exists(dmp_fn)){
    stop("The DMP file `", dmp_fn, "` does not exist")
  }

  fns <- dir(dr)
  pat <- "^LogCatchReport[0-9]{4}\\.csv$"
  logs_fns <- grep(pat, fns, value = TRUE)
  if(!length(logs_fns)){
    stop("There were no LOGS files found in the supplied sirectory `dr` =",
         "`", dr, "`")
  }
  logs_fns <- file.path(dr, logs_fns)

  # Read in DMP file ----
  ct_vec <- readLines(dmp_fn)
  ph_ind <- grep("^PACIFIC HAKE LANDINGS FROM", ct_vec[1:5])
  if(length(ph_ind)){
    ct_vec <- ct_vec[-ph_ind]
  }
  d <- read.table(text = ct_vec,
                  sep = ",",
                  fill = TRUE,
                  as.is = TRUE,
                  header = TRUE) |>
    as_tibble()

  names(d) <- map_chr(names(d), ~{
    .x <- tolower(.x)
    .x <- gsub("\\.", "_", .x)
  })

  d_inside <- d |>
    mutate(landing_date = as.Date(landing_date, "%B %d %Y")) |>
    dplyr::filter(landing_date >= min_date) |>
    dplyr::filter(grepl("(GULF)|(OPT B)", licence_trip_type))

  d <- d |>
    mutate(landing_date = as.Date(landing_date, "%B %d %Y")) |>
    dplyr::filter(landing_date >= min_date) |>
    dplyr::filter(!grepl("(GULF)|(OPT B)", licence_trip_type))

  # Read in LOGS files ----
  logs_df <- map_dfr(logs_fns, \(fn){
    logs_vec <- readLines(fn)
    hdr_ind <- grep("^TRIP", logs_vec)
    if(!length(hdr_ind) || length(hdr_ind) > 1){
      stop("The file `", fn, "` either has no header starting with 'TRIP' ",
           "or more than one line staarting with 'TRIP'. Fix that file")
    }
    logs_vec <- logs_vec[-(1:(hdr_ind - 1))]
    logs_df <- read.table(text = logs_vec,
                          sep = ",",
                          quote = "",
                          fill = TRUE,
                          as.is = TRUE,
                          header = TRUE) |>
      as_tibble()
  })
  names(logs_df) <- map_chr(names(logs_df), ~{
    .x <- tolower(.x)
    .x <- gsub("\\.", "_", .x)
  })

  logs_df <- logs_df |>
    mutate(landing_date = as.Date(landing_date, "%B %d %Y")) |>
    dplyr::filter(landing_date >= min_date) |>
    dplyr::filter(!grepl("(GULF)|(OPT B)", trip_type))

  logs_inside_df <- NULL
  if(any(logs_df$area == "4B")){
    logs_inside_df <- logs_df |>
      dplyr::filter(area == "4B")
    # logs_df <- logs_df |>
    #   dplyr::filter(landing_port != "FRENCH CREEK") |>
    #   mutate(month = month(landing_date)) |>
    #   dplyr::filter(!(area == "4B" & month < 6)) |>
    #   select(-month)
  }

  list(dmp_df = d,
       dmp_inside_df = d_inside,
       logs_df = logs_df,
       logs_inside_df = logs_inside_df)
}
