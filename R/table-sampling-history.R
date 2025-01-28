#' Creates a table of the sampling history for hake
#'
#' @param d Data frame as found in [sampling_history_df]
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param replace_start_yr The first year to replace in the csv file using the
#' age data frames for each fleet. All data for this year to `end_yr` will be
#' replaced in the "data-tables/fishery-sampling-history.csv" file. If
#' `NULL`, the file is not loaded and `sampling_history_df` is used instead.
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
table_sampling_history <- function(start_yr,
                                   end_yr,
                                   replace_start_yr = NULL,
                                   font_size = 10,
                                   header_font_size = 10,
                                   header_vert_spacing = 12,
                                   header_vert_scale = 1.2,
                                   ...){

  if(is.null(replace_start_yr)){
    d <- sampling_history_df
  }else{
    d <- read_csv(here("data-tables/fishery-sampling-history.csv"),
                  col_types = cols(),
                  show_col_types = FALSE)

    if(!nrow(d)){
      stop("Problem reading the sampling CSV file: ",
           "`data-tables/fishery-sampling-history.csv`. No rows were loaded")
    }
    row_template <- d |>
      slice(1) |>
      mutate(across(everything(), ~{.x = NA_real_}))

    lst <- list(us_ms_age_df,
                us_cp_age_df,
                us_sb_age_df,
                can_ss_age_df,
                can_ft_age_df) |>
      map(~{.x |>
          dplyr::filter(year %in% replace_start_yr:end_yr) |>
          select(year, num_samples) |>
          complete(year = replace_start_yr:end_yr,
                   fill = list(num_samples = NA))})

    d <- d |>
      dplyr::filter(!Year %in% replace_start_yr:end_yr)
    rows <- row_template
    for(row in seq_along(replace_start_yr:end_yr)[-1]){
      rows <- rows |>
        bind_rows(row_template)
    }
    rows$Year <- replace_start_yr:end_yr
    rows$`U.S. Mothership (hauls)` <- lst[[1]]$num_samples
    rows$`U.S. Catcher-processor (hauls)` <- lst[[2]]$num_samples
    rows$`U.S. Shoreside (trips)` <- lst[[3]]$num_samples
    rows$`Canada Shoreside (trips)` <- lst[[4]]$num_samples
    rows$`Canada Freezer trawlers (hauls)` <- lst[[5]]$num_samples

    d <- d |>
      bind_rows(rows)

    write_csv(d, here("data-tables/fishery-sampling-history.csv"),
              na = "")
  }

  yrs <- d$Year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  d <- d |>
    dplyr::filter(Year %in% yrs) |>
    mutate_all(~{as.numeric(.x)}) |>
    mutate_at(.vars = vars(-Year), ~{
      ifelse(is.na(.x),
             "--",
             .x)})

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)
  # Insert header font size after every newline
  col_names <- gsub(" ", "\n", names(d))
  col_names <- gsub("-", "-\n", col_names)
  col_names <- gsub("Shoreside", "Shore-\nside", col_names)
  col_names <- gsub("Mothership", "Mother-\nship", col_names)
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
