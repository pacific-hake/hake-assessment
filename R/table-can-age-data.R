#' Creates a table containing the U.S. age data which is input into the model
#'
#' @param lst A list of data frames of the age data as read in from the files
#' `us-*-age-data.csv` found in the data directory. The list has one data frame
#' per fleet
#' @param d_num_aged A data frame of number of fish aged as read in from
#' the files `can-*-num-fish-aged.csv` found in the data directory
#' @param fleet Use codes of `ft` = Freezer trawlers, `ss` = Shoreside, and
#' `jv` = Joint venture
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
table_can_age_data <- function(lst,
                               d_num_aged,
                               fleet = c("ft", "ss", "jv"),
                               start_yr,
                               end_yr,
                               digits = 2,
                               font_size = 10,
                               header_font_size = 10,
                               header_vert_spacing = 12,
                               header_vert_scale = 1.2,
                               ...){

  fleet <- match.arg(fleet)
  flt <-
    ifelse(fleet == "ft",
           "Freezer Trawler age comps",
           ifelse(fleet == "ss",
                  "Shoreside age comps",
                  "Joint Venture age comps"))
  flt_num_hauls <-
    ifelse(fleet == "ft",
           "Freezer Trawler number of hauls sampled for age",
           ifelse(fleet == "ss",
                  "Shoreside number of trips sampled for age",
                  "Joint Venture number of hauls sampled for age"))

  d <- lst[[flt]] |>
    as_tibble(rownames = "yr") |>
    mutate(yr = as.numeric(yr))

  d_num_hauls <- lst[[flt_num_hauls]] |>
    enframe(name = "yr", value = "num_hauls") |>
    mutate(yr = as.numeric(yr))

  d_num_fish <- d_num_aged |>
    rename(yr = year)

  d <- d |>
    left_join(d_num_fish, by = "yr") |>
    left_join(d_num_hauls, by = "yr") |>
    select(yr, num_fish, num_hauls, everything()) |>
    mutate(num_hauls = as.numeric(num_hauls))

  yrs <- d$yr
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  ages <- grep("[0-9]+", names(d), value = TRUE)
  d <- d |>
    filter(yr %in% yrs) |>
    mutate_at(vars(ages), ~{f(.x * 100, digits)}) |>
    mutate_at(vars(c(num_fish, num_hauls)), ~{f(.x)})

  nms <- names(d)
  nms <- gsub("^a([0-9]+)$", "\\1", nms)
  nms <- gsub("num_fish", "", nms)
  nms <- gsub("num_hauls", "", nms)
  nms <- gsub("yr", "", nms)
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
    kable_styling(font_size = font_size,
                  latex_options = c("repeat_header")) |>
    add_header_above(header,
                     bold = TRUE,
                     line = FALSE)
}
