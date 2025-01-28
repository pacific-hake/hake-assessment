#' Make a latex table of credible intervals with columns for female spawning biomass,
#' relative spawning biomass, total biomass, age 2+ biomass, age-0 recruits, relative
#' fishing intensity, and exploitation fraction. Also output a CSV file with the table contents.
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param csv_dir Directory for CSV output
#' @param digits Number of decimal points to show in values in the table
#' @param lower_col Column name in the data frames for the lower ci value
#' @param upper_col Column name in the data frames for the upper ci value
#' @param scale Scale factor for biomasses and recruitments
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
table_ci_posteriors <- function(model,
                                start_yr,
                                end_yr,
                                scale = 1000,
                                digits = 1,
                                lower_col = "2.5%",
                                upper_col = "97.5%",
                                font_size = 10,
                                header_font_size = 10,
                                header_vert_spacing = 12,
                                header_vert_scale = 1.2,
                                ...){

  lower_col_sym <- sym(lower_col)
  upper_col_sym <- sym(upper_col)

  yr_range <- get_year_range(start_yr,
                             end_yr,
                             min(model$extra_mcmc$total_biomass_quants$yr),
                             max(model$extra_mcmc$total_biomass_quants$yr))
  yrs <- yr_range$start_yr:yr_range$end_yr
  df <- map(model$mcmccalcs, ~{.x[names(.x) %in% yrs]})
  ts <- model$timeseries |>
    as_tibble()

  tot_bm <- model$extra_mcmc$total_biomass_quants |>
    dplyr::filter(yr %in% yrs)
  stopifnot(lower_col %in% names(tot_bm))
  stopifnot(upper_col %in% names(tot_bm))
  tot_bm_lower <- tot_bm |>
    pull(!!lower_col_sym)
  tot_bm_upper <- tot_bm |>
    pull(!!upper_col_sym)

  age2plus_bm <- model$extra_mcmc$total_age2_plus_biomass_quants |>
    dplyr::filter(yr %in% yrs)
  stopifnot(lower_col %in% names(age2plus_bm))
  stopifnot(upper_col %in% names(age2plus_bm))
  age2plus_bm_lower <- age2plus_bm |>
    pull(!!lower_col_sym)
  age2plus_bm_upper <- age2plus_bm |>
    pull(!!upper_col_sym)

  d <- cbind(yrs,
             paste0(trimws(f(df$slower * scale)),
                    "-",
                    trimws(f(df$supper * scale))),
             paste0(f(df$dlower * 100, digits),
                    "-",
                    trimws(f(df$dupper * 100, digits))),
             paste0(trimws(f(tot_bm_lower / scale)),
                    "-",
                    trimws(f(tot_bm_upper / scale))),
             paste0(f(age2plus_bm_lower / scale),
                    "-",
                    trimws(f(age2plus_bm_upper / scale))),
             paste0(trimws(f(df$rlower * scale)),
                    "-",
                    trimws(f(df$rupper * scale))),
             paste0(trimws(f(df$plower * 100, digits)),
                    "-",
                    trimws(f(df$pupper * 100, digits))),
             paste0(trimws(f(df$flower * 100, digits)),
                    "-",
                    trimws(f(df$fupper * 100, digits)))) |>
    as.data.frame() |>
    set_names(c("Year",
                "Female spawning biomass (kt)",
                "Relative spawning biomass (%)",
                "Total biomass (kt)",
                "Age-2$+$ biomass (kt)",
                "Age-0 recruits (millions)",
                "Relative fishing intensity (%)",
                "Exploitation fraction (%)")) |>
    as_tibble()

  # Make current year have dashes for exploitation rate and fishing intensity
  d[nrow(d), ncol(d)] <- latex_bold("--")
  d[nrow(d), ncol(d) - 1] <- latex_bold("--")

  col_names <- c("Year",
                 "Female\nspawning\nbiomass\n(kt)",
                 "Relative\nspawning\nbiomass\n(\\%)",
                 "Total\nbiomass\n(kt)",
                 "Age-2$+$\nbiomass\n(kt)",
                 "Age-0\nrecruits\n(millions)",
                 "Relative\nfishing\nintensity\n(\\%)",
                 "Exploitation\nfraction\n(\\%)")

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  kbl(d,
      format = "latex",
      booktabs = TRUE,
      align = "r",
      linesep = "",
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
