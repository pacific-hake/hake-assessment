#' Title
#'
#' @param d A data frame containing the ovary sampling information  as read
#' in from the file `ovary-samples.csv`
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
#' @return A [knitr::kable()] object
#' @export
table_assessment_changes <- function(d,
                                     start_yr,
                                     end_yr,
                                     digits = 3,
                                     font_size = 10,
                                     header_font_size = 10,
                                     header_vert_spacing = 12,
                                     header_vert_scale = 1.2,
                                     ...){

  yrs <- d$Year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  d <- d |>
    mutate(Comp_Method = gsub("Dirichlet-multinomial", "D-M", Comp_Method)) |>
    mutate(Comp_Method = gsub("McAllister-Ianelli", "M-I", Comp_Method)) |>
    mutate(MCMC = f(MCMC)) |>
    mutate(Comp_Fishery = f(Comp_Fishery, digits),
           Comp_Survey = f(Comp_Survey, digits)) |>
    mutate(Comp_Method = paste0(Comp_Method,
                                " (",
                                Comp_Fishery,
                                ", ",
                                Comp_Survey,
                                ")")) |>
    select(-c(Comp_Fishery, Comp_Survey)) |>
    mutate(Change = gsub("\\\\sigma", "\\sigma", Change)) |>
    # Cannot have multiple periods in a number (software version must be separated by dashes
    # instead to satisfy accessibility requirements)
    mutate(Framework = gsub("\\.", "-", Framework)) |>
    dplyr::filter(Year %in% yrs)

  col_names <- c("Year",
                 "Framework",
                 "Survey",
                 "Comp\nMethod",
                 "Num.\nMCMC\nsamples",
                 "Change")

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
      align = c("c", "l", "r", "r", "r", "l"),
      linesep = "",
      col.names = col_names,
      escape = FALSE,
      ...) |>
    row_spec(0, bold = TRUE) |>
    column_spec(ncol(d), width = "5cm") |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
