#' Makes a table of the estimated -at-age values for 5 different values:
#' Numbers-at-age, Catch-at-age, Biomass-at-age, Exploitation-at-age,
#' and Catch-at-age-biomass
#'
#' @param model A model, created by [create_rds_file()]
#' @param type `naa` = Numbers-at-age, `eraa` = Exploitation-rate-at-age,
#' `caan` = Catch-at-age-number, `caab` = Catch-at-age-biomass,
#' `baa` = Biomass-at-age
#' @param csv_dir Directory for CSV output
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param digits Number of decimal points to show in values in the table
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return An [knitr::kable()] object
#' @export
table_at_age <- function(model,
                         start_yr = NA,
                         end_yr = NA,
                         type = c("naa", "eraa", "caan", "caab", "baa"),
                         digits = 0,
                         csv_dir = here::here("doc", "out-csv"),
                         font_size = 10,
                         header_font_size = 10,
                         header_vert_spacing = 12,
                         header_vert_scale = 1.2,
                         ...){

  type <- match.arg(type)

  if(!dir.exists(csv_dir)){
    dir.create(csv_dir)
  }

  # At-age output data table file names ----
  out_est_naa_fn <- "estimated-numbers-at-age.csv"
  out_est_eaa_fn <- "estimated-exploitation-at-age.csv"
  out_est_caa_fn <- "estimated-catch-at-age.csv"
  out_est_caa_bio_fn <- "estimated-catch-at-age-biomass.csv"
  out_est_baa_fn <- "estimated-biomass-at-age.csv"

  d <- switch (type,
               naa = model$extra_mcmc$natage_med,
               eraa = model$extra_mcmc$expatage_med,
               caan = model$extra_mcmc$catage_med,
               caab = model$extra_mcmc$cbatage_med,
               baa = model$extra_mcmc$batage_med)
  fn <- switch (type,
                naa = file.path(csv_dir, out_est_naa_fn),
                eraa = file.path(csv_dir, out_est_eaa_fn),
                caan = file.path(csv_dir, out_est_caa_fn),
                caab = file.path(csv_dir, out_est_caa_bio_fn),
                baa = file.path(csv_dir, out_est_baa_fn))

  yrs_in_table <- sort(unique(d$yr))
  min_yr <- min(yrs_in_table)
  max_yr <- max(yrs_in_table)
  start_yr <- ifelse(is.na(start_yr), min_yr, start_yr)
  end_yr <- ifelse(is.na(end_yr), max_yr, end_yr)
  if(start_yr > end_yr){
    start_yr <- min_yr
    end_yr <- max_yr
  }
  start_yr <- ifelse(start_yr < min_yr, min_yr, start_yr)
  end_yr <- ifelse(end_yr > max_yr, max_yr, end_yr)
  yrs <- start_yr:end_yr

  d <- d |>
    filter(yr %in% yrs) |>
    rename(Year = yr) |>
    mutate(Year = as.character(Year))

  write_csv(d, fn)

  d <- d |>
    mutate_at(vars(-Year), ~{f(.x, digits)})
  names(d)[length(names(d))] <- paste0(names(d)[length(names(d))], "+")

  # Extra header
  ages <- grep("[0-9]+", names(d), value = TRUE)
  header <-
    c("Year",
      "Age" = length(ages))

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  names(d)[names(d) == "Year"] <- ""
  col_names <- names(d)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  kbl(d,
      format = "latex",
      booktabs = TRUE,
      align = "c",
      linesep = "",
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    add_header_above(header,
                     bold = TRUE,
                     line = FALSE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
