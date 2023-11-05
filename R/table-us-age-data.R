#' Creates a table containing the U.S. age data which is inout into the model
#'
#' @param d A data frame of the age data as read in from the files
#' `us-*-age-data.csv` found in the data directory
#' @param fleet Use codes of `cp` = Catcher-processor, `ms` = Mothership, and
#' `sb` = Shore-based
#' @param start_yr Start year in table
#' @param end_yr End year in table
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
table_us_age_data <- function(d,
                              fleet = c("cp", "ms", "sb"),
                              start_yr,
                              end_yr,
                              digits = 2,
                              font_size = 10,
                              header_font_size = 10,
                              header_vert_spacing = 12,
                              header_vert_scale = 1.2,
                              ...){

  fleet <- match.arg(fleet)

  yrs <- d$year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  ages <- grep("[0-9]+", names(d), value = TRUE)
  d <- d |>
    filter(year %in% yrs) |>
    mutate_at(vars(ages), ~{f(.x * 100, digits)}) |>
    mutate_at(vars(2:3), ~{f(.x)})

  nms <- names(d)
  nms <- gsub("^a([0-9]+)$", "\\1", nms)
  nms <- gsub("n.fish", "", nms)
  nms <- gsub("n.hauls", "", nms)
  nms <- gsub("year", "", nms)
  nms[length(nms)] <- paste0(nms[length(nms)], "+")
  names(d) <- nms
  col_names <- nms

  # Extra header
  header <-
    c("Year",
      "Number\nof fish",
      "Number\nof hauls",
      "Age (% of total for each year)" = length(ages))

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
    add_header_above(header,
                     bold = TRUE,
                     line = FALSE) |>
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header"))
}
