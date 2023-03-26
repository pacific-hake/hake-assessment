#' Create a table of total catch
#'
#' @param ct Data frame as in the file `landing-tac-histroy.csv`
#' @param country One of "both", "can", or "us"
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param inc_foreign_jv Logical. If `TRUE`, include columns for the U.S.
#' foreign fleets and Canadian foreign fleet
#' @param font_size The table data and header font size in points
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
             `U.S. Shore-based`,
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
             `U.S. Shore-based`,
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
    filter(Year %in% yrs) |>
    mutate(Year = as.character(Year)) |>
    mutate_at(.vars = vars(-Year), ~{f(.x, 0)})

  # Add newlines into the headers at certain locations
  col_names <- gsub(" ", "\n", names(df))
  col_names <- gsub("-", "-\n", col_names)
  col_names <- gsub("Shoreside", "Shore-\nside", col_names)
  col_names <- gsub("Mothership", "Mother-\nship", col_names)
  # Add \\makecell{} latex macro to headers with newlines
  col_names <- linebreak(col_names, align = "c")
  # Center header names which don't have newlines, they are ignored by
  # linebreak(), so add \\makecell manually
  col_names[col_names == "Year"] <- "\\makecell[c]{Year}"
  col_names[col_names == "Total"] <- "\\makecell[c]{Total}"

  kable(df,
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
