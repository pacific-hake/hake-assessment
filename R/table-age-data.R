#' Creates a table containing single-fleet age proportions which are
#' input into the model
#'
#' @param fleet Use codes of `can_ft` = Canada Freezer trawlers, `can_ss` =
#' Canada Shoreside, `can_jv` = Canada Joint venture, `us_cp` = U.S. Catcher-
#' processor, `us_ms` = U.S. Mothership, or `us_sb` = U.S. Shoreside
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
table_age_data <- function(fleet = c("can_ft", "can_ss", "can_jv",
                                     "us_cp", "us_ms", "us_sb"),
                           start_yr,
                           end_yr,
                           digits = 2,
                           font_size = 10,
                           header_font_size = 10,
                           header_vert_spacing = 12,
                           header_vert_scale = 1.2,
                           ...){

  fleet <- match.arg(fleet)
  switch(fleet,
         can_ft = {
           flt <- "Freezer Trawler age comps"
           flt_num_hauls <- "Freezer Trawler number of hauls sampled for age"
           d <- can_ft_age_df
         },
         can_ss = {
           flt <- "Shoreside age comps"
           flt_num_hauls <- "Shoreside number of trips sampled for age"
           d <- can_ss_age_df
         },
         can_jv = {
           flt <- "Joint Venture age comps"
           flt_num_hauls <- "Joint Venture number of hauls sampled for age"
           d <- can_jv_age_df
         },
         us_cp = {
           flt <- "Catcher-processor age comps"
           flt_num_hauls <- "Catcher-processor number of hauls sampled for age"
           d <- us_cp_age_df
         },
         us_ms = {
           flt <- "Mothership age comps"
           flt_num_hauls <- "Mothership number of hauls sampled for age"
           d <- us_ms_age_df
         },
         us_sb = {
           flt <- "Shore-based age comps"
           flt_num_hauls <- "Shore-based number of trips sampled for age"
           d <- us_sb_age_df
         })

  yrs <- d$year
  start_yr <- start_yr %||% min(yrs)
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- end_yr %||% max(yrs)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  ages <- grep("[0-9]+", names(d), value = TRUE)
  d <- d |>
    dplyr::filter(year %in% yrs) |>
    mutate(across(ages, ~{f(.x * 100, digits)})) |>
    mutate(across(c(num_fish, num_samples), ~{f(.x)}))

  # Make all column names except the ages empty
  nms <- names(d)
  nms <- gsub("num_fish", "", nms)
  nms <- gsub("num_samples", "", nms)
  nms <- gsub("year", "", nms)
  nms[length(nms)] <- paste0(nms[length(nms)], "+")
  names(d) <- nms
  col_names <- nms

  th <- ifelse(fleet %in% c("us_sb", "can_ss"), "trips", "hauls")
  # Extra header
  header <-
    c("Year",
      "Number\nof fish",
      paste0("Number\nof ", th),
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
