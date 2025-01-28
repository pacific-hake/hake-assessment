#' Create a table of total catch
#'
#' @param ct Data frame as in the file `landing-tac-history.csv`
#' @param country One of "both", "can", or "us"
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param inc_foreign_jv Logical. If `TRUE`, include columns for the U.S.
#' foreign fleets and Canadian foreign fleet
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
table_catch <- function(ct,
                        country = c("both", "can", "us"),
                        start_yr,
                        end_yr,
                        inc_foreign_jv = FALSE,
                        font_size = 10,
                        header_font_size = 10,
                        header_vert_spacing = 12,
                        header_vert_scale = 1.2,
                        ...){

  country <- match.arg(country)

  yrs <- ct$Year
  start_yr <- ifelse(start_yr < min(yrs), min(yrs), start_yr)
  end_yr <- ifelse(end_yr > max(yrs) | end_yr < start_yr, max(yrs), end_yr)
  yrs <- start_yr:end_yr

  if(country == "can"){
    df <- ct |>
      select(Year,
             `Canada Foreign`,
             `Canada Joint-venture`,
             `Canada Shoreside`,
             `Canada Freezer-trawler`,
             `Canada Total`)
    if(!inc_foreign_jv){
      df <- df |>
        select(-`Canada Foreign`,
               -`Canada Joint-venture`)
    }
    names(df) <- gsub("Canada +", "", names(df))
  }else if(country == "us"){
    df <- ct |>
      select(Year,
             `U.S. Foreign`,
             `U.S. Joint-venture`,
             `U.S. Mothership`,
             `U.S. Catcher-processor`,
             `U.S. Shoreside`,
             `U.S. Research`,
             `U.S. Total`)
    if(!inc_foreign_jv){
      df <- df |>
        select(-`U.S. Foreign`,
               -`U.S. Joint-venture`)
    }
    names(df) <- gsub("U.S. +", "", names(df))
  }else if(country == "both"){
    df <- ct |>
      select(Year,
             `U.S. Foreign`,
             `U.S. Joint-venture`,
             `U.S. Mothership`,
             `U.S. Catcher-processor`,
             `U.S. Shoreside`,
             `U.S. Research`,
             `U.S. Total`,
             `Canada Foreign`,
             `Canada Joint-venture`,
             `Canada Shoreside`,
             `Canada Freezer-trawler`,
             `Canada Total`,
             `Total`)
    if(!inc_foreign_jv){
      df <- df |>
        select(-`Canada Foreign`,
               -`U.S. Foreign`,
               -`U.S. Joint-venture`)
    }
  }

  df <- df |>
    dplyr::filter(Year %in% yrs) |>
    mutate(Year = as.character(Year)) |>
    mutate_at(.vars = vars(-Year), ~{f(.x, 0)})

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)
  # Add newlines into the headers at certain locations
  col_names <- gsub(" ", "\n", names(df))
  col_names <- gsub("-", "-\n", col_names)
  col_names <- gsub("Shoreside", "Shore-\nside", col_names)
  #col_names <- gsub("\nbased", "\nside", col_names)
  col_names <- gsub("Mothership", "Mother-\nship", col_names)
  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  kbl(df,
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
