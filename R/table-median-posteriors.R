#' Make a latex table of medians with columns for female spawning biomass,
#' relative spawning biomass, total biomass, age 2+ biomass, age-0 recruits, relative
#' fishing intensity, and exploitation fraction. Also output a CSV file with the table contents.
#'
#' @param model A model, created by [create_rds_file()]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param csv_dir Directory for CSV output
#' @param digits Number of decimal points to show in values in the table
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
table_median_posteriors <- function(model,
                                    start_yr = NULL,
                                    end_yr = NULL,
                                    digits = 1,
                                    scale = 1000,
                                    csv_dir = here::here("doc", out_csv_path),
                                    font_size = 10,
                                    header_font_size = 10,
                                    header_vert_spacing = 12,
                                    header_vert_scale = 1.2,
                                    ...){
  if(!is.null(csv_dir)){
    if(!dir.exists(csv_dir)){
      dir.create(csv_dir)
    }
  }

  yr_range <- get_year_range(start_yr,
                             end_yr,
                             min(model$extra_mcmc$total_biomass_quants$yr),
                             max(model$extra_mcmc$total_biomass_quants$yr))
  yrs <- yr_range$start_yr:yr_range$end_yr
  df <- map(model$mcmccalcs, ~{.x[names(.x) %in% yrs]})
  ts <- model$timeseries |>
    as_tibble()
  tot_bm <- model$extra_mcmc$total_biomass_quants |>
    dplyr::filter(yr %in% yrs) |>
    select(`50%`)
  age2plus_bm <- model$extra_mcmc$total_age2_plus_biomass_quants |>
    dplyr::filter(yr %in% yrs) |>
    select(`50%`)

# d <- cbind(yrs,
#            f(df$smed * scale),
#            paste0(f(df$dmed * 100, digits), "\\%"),
#            f(tot_bm / scale),
#            f(age2plus_bm / scale),
#            f(df$rmed * scale),
#            paste0(f(df$pmed * 100, digits), "\\%"),
#            paste0(f(df$fmed * 100, digits), "\\%")) |>
#
  d <- cbind(yrs,
             f(df$smed * scale),
             f(df$dmed * 100, digits),
             f(tot_bm / scale),
             f(age2plus_bm / scale),
             f(df$rmed * scale),
             f(df$pmed * 100, digits),
             f(df$fmed * 100, digits)) |>
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

  # Write the median posteriors CSV file --------------------------------------
  csv_d <- d
  #csv_d <- map_df(csv_d, ~{
  #  gsub("\\\\%", "%", .x)
  #})
  csv_d[nrow(csv_d), ncol(csv_d)] <- ""
  csv_d[nrow(csv_d), ncol(csv_d) - 1] <- ""
  write_csv(csv_d, file.path(csv_dir, "median-population-estimates.csv"))

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
