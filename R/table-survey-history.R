#' Create a table of the acoustic survey history
#'
#' @details
#' The vessel names need to be fixed. They are seperated by spaces, and may
#' or may not have dashes in their names. The dashes will be replaced with
#' spaces, and the spaces will be replaced by newlines in the output
#'
#' @param model A model, created by [create_rds_file()]
#' @param d A data frame containing the survey history as read in from
#' the file `survey-history.csv`
#' @param digits Number of decimal places
#' @param font_size The table data and header font size in points
#' @param header_font_size The font size for the headers only. If `NULL`,
#' the headers will have the same font size as the table cell data
#' @param vert_spacing The vertical spacing between newlines for this font.
#' If `NULL` this will be calculated as `header_font_size * header_vert_scale`
#' @param header_vert_scale Scale factor to create the vertical spacing value.
#' See `header_vert_spacing`
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_survey_history <- function(model,
                                 d,
                                 digits = 3,
                                 font_size = 10,
                                 header_font_size = 10,
                                 header_vert_spacing = 12,
                                 header_vert_scale = 1.2,
                                 ...){

  vess <- gsub(" +", "\n", d$vessels)
  vess <- gsub("-", " ", vess)
  d$vessels <- linebreaker(vess, align = "c")
  d <- d |>
    rename(yr = year,
           start_dt = start.date,
           end_dt = end.date,
           num_hauls_samples = hauls.with.samples)

  cpue <- model$dat$CPUE |>
    as_tibble() |>
    filter(index > 0) |>
    select(-seas) |>
    rename(yr = year) |>
    mutate(obs = f(obs / 1e6, digits),
           se_log = f(se_log, digits)) |>
    pivot_wider(names_from = index,
                values_from = c("obs", "se_log"),
                values_fill = "--")


  d <- d |>
    full_join(cpue, by = "yr") |>
    select(yr,
           start_dt,
           end_dt,
           vessels,
           obs_2,
           se_log_2,
           num_hauls_samples,
           obs_3,
           se_log_3)

  col_names <- c("Year",
                 "Start\ndate",
                 "End\ndate",
                 "Vessels",
                 "Age-2+\nbiomass\nindex\n(million t)",
                 "Sampling\nCV\nage-2+",
                 "Number\nof\nhauls\nwith\nage\nsamples",
                 "Age-1\nindex\n(billions\nof\nfish)",
                 "Sampling\nCV\nage-1")

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
