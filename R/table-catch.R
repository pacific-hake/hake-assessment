#' Create a table of total catch
#'
#' @param ct Output from [load_catches()]
#' @param country One of "both", "can", or "us"
#' @param start_yr Start year in table
#' @param end_yr End year in table
#' @param col_width Width in inches for all the columns
#' @param font_size The table data and geader font size in points
#' @param ... Arguments passed to [knitr::kable()]
#'
#' @return A [knitr::kable()] object
#' @export
table_catch <- function(ct,
                        country = c("both", "can", "us"),
                        start_yr,
                        end_yr,
                        inc_foreign_jv = FALSE,
                        col_width = 0.5,
                        font_size = 11,
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
               -`Canada Joint-venture`,
               -`U.S. Foreign`,
               -`U.S. Joint-venture`)
    }
  }

  df <- df |>
    filter(Year %in% yrs) |>
    mutate(Year = as.character(Year)) |>
    mutate_at(.vars = vars(-Year), ~{f(.x, 0)})

  kable(df,
        format = "latex",
        booktabs = TRUE,
        align = "r",
        linesep = "",
        ...) |>
    column_spec(column = 1:ncol(df), width = paste0(col_width, "in")) |>
    kable_styling(font_size = font_size)
}
