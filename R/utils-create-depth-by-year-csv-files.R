#' Get depth data by year and calculate boxplot stats on it.
#' This writes a CSV file containing the output
#'
#' @param d Output from [load_spatial_catch_data()]
#' @param col_name_depth The name of the column in the data frame `d` that
#' contains the depths
#' @param col_name_year The name of the column which holds the years
#' @param country The country the data is from (used in the output file
#' name only)
#' @param fleet The fleet shorthand for the fleet/sector (used in the
#' output file name only)
#' @param type One of `bottom` or `gear` for depth type (used in the output
#' file name only)
#' @param output_path The directory in which to write the output CSV file
#' @param probs A vector of the proabilities to use for the depth stats that
#' will become the box ends and whisker ends of the box plots when they
#' are plotted with [plot_depth()]
#' @param yrs A vector of years to include. If NULL, all years in the data
#' will be included
#' @param scale A value to multiply the depths by. Default value of 1.8288
#' converts fathoms to meters. Set to 1 to use values as-is
#' @param min_depth_cutoff The depth for which to remove data. All data
#' shallower than this will be removed. If this is left at the default of
#' zero, all zero depth records will be removed from the data
#' @param digits The number of decimal points to round the depths to
#'
#' @return Invisibly - A tibble containing year and depth record stats
#'
#' @export
create_depth_by_year_csv_files <- function(
    d = NULL,
    col_name_depth = "best_depth",
    col_name_year = "year",
    country = c("can", "us"),
    fleet = c("ft", "ss", "atsea", "cp", "ms", "sb"),
    type = c("bottom", "gear"),
    output_path = here(data_tables_path),
    probs = probs_forecast,
    yrs = NULL,
    scale = 1.8288,
    min_depth_cutoff = 0,
    digits = 2){

  if(is.null(d)){
    stop("The input data frame `d` cannot be `NULL`")
  }

  country <- match.arg(country)
  fleet <- match.arg(fleet)
  type <- match.arg(type)

  if(is.null(col_name_depth)){
    stop("`col_name_depth` cannot be `NULL`")
  }
  if(is.null(col_name_year)){
    stop("`col_name_year` cannot be `NULL`")
  }
  if(!col_name_depth %in% names(d)){
    stop("`col_name_depth` = `", col_name_depth, "` is not a column name in the data ",
         "frame `d`")
  }
  if(!col_name_year %in% names(d)){
    stop("`col_name_year` = `", col_name_year, "` is not a column name in the data ",
         "frame `d`")
  }

  col_sym_depth <- sym(col_name_depth)
  col_sym_year <- sym(col_name_year)

  # Make a shorter alias for as.data.frame to make the `do()`
  # line below nicer
  adf <- as.data.frame
  dpth <- d |>
    dplyr::filter(!is.na(!!col_sym_depth)) |>
    transmute(!!col_sym_year,
              depth = !!col_sym_depth * scale) |>
    dplyr::filter(depth > min_depth_cutoff) |>
    group_by(year) |>
    do(adf(t(adf(quantile(.$depth, probs))))) |>
    ungroup() |>
    setNames(c("year",
               "lower95",
               "lowerhinge",
               "median",
               "upperhinge",
               "upper95"))

  if(!is.null(yrs)){
    dpth <- dpth |>
      dplyr::filter(!!col_sym_year %in% yrs)
  }

  # Round all columns except `year`
  dpth <- dpth |>
    mutate(across(-!!col_sym_year, round, digits))

  # Create the output filename
  out_fn <- paste0("depth-", country, "-", fleet, "-", type, ".csv")
  out_fn <- file.path(output_path, out_fn)
  write_csv(dpth, out_fn)
  message("Created depth file:\n`", out_fn, "`")
}
