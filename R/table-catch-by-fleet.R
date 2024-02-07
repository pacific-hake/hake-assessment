#' Create a table containing the catch-at-age for one of the 3 Canadian
#' or 3 U.S. fleets
#'
#' @param flt The fleet index. 1 = CAN FT, 2 = CAN SS, 3 = CAN JV,
#' 4 = US MS, 5 = US CP, 6 = US SB
#' @param font_size The size of the font in the table cells
#' @param country If `NULL`, use `flt` to make the plot. Either 1 or 2 can
#' be supplied. If 1, plot Canada. If 2, plot US
#'
#' @return A [kableExtra::kbl()] table
#' @export
table_catch_by_fleet <- function(flt = 1,
                                 country = NULL,
                                 font_size  = 4){

  if(!is.null(country)){
    if(!country %in% 1:2){
      stop("`country` must be either 1, 2, or `NULL`")
    }
  }

  catch_raw_lst <- list(
    can_ft_catch_by_month_df,
    can_ss_catch_by_month_df,
    can_jv_catch_by_month_df,
    us_ss_catch_by_month_df,
    us_cp_catch_by_month_df,
    us_ms_catch_by_month_df)

  nms <- c("CAN FT",
           "CAN SS",
           "CAN JV",
           "US MS",
           "US CP",
           "US SB")

  # Merge age props
  age_raw_lst <- list(can_ft_age_df,
                  can_ss_age_df,
                  can_jv_age_df,
                  us_ms_age_df,
                  us_cp_age_df,
                  us_sb_age_df)
  age_lst <- map2(age_raw_lst, nms, ~{
    x <- .x |>
      select(year, grep("^[0-9]+$", names(.x))) |>
      mutate(fleet = .y)

    if(length(grep("^CAN", .y))){
      x <- x |>
        mutate(country = "Canada")
    }else{
      x <- x |>
        mutate(country = "U.S.")
    }
    x |>
      select(country, fleet, everything())
  })

  catch_lst <- map2(catch_raw_lst, nms, ~{
    x <- .x |>
      group_by(year) |>
      summarize(year, catch = sum(catch)) |>
      ungroup() |>
      mutate(fleet = .y)

    if(length(grep("^CAN", .y))){
      x <- x |>
        mutate(country = "Canada")
    }else{
      x <- x |>
        mutate(country = "U.S.")
    }
    x |>
      distinct() |>
      select(country, fleet, everything())
  })

  # Truncate age_df to catch_df by years
  age_lst <- map2(age_lst, catch_lst, ~{
    .x |>
      filter(year %in% unique(.y$year))
  })
  # Truncate catch_df to age_df by years
  catch_lst <- map2(catch_lst, age_lst, ~{
    .x |>
      filter(year %in% unique(.y$year))
  })

  joined_lst <- map2(catch_lst, age_lst, ~{
    xx <- .x |>
      full_join(.y, by = c("year", "fleet", "country"))
  })

  if(!is.null(country)){
    joined_df <- joined_lst |>
      bind_rows() |>
      select(-fleet)
    df <- joined_df |>
      filter(country == ifelse(!!country == 1, "Canada", "U.S.")) |>
      group_by(year) |>
      select(-country, -catch)

    catch_age_lst <- list(df)
  }

  catch_age_lst <- map(joined_lst, ~{
    .x |>
      mutate(across(grep("^[0-9]+$", names(.x)), ~{.x * catch} )) |>
      mutate(across(c(catch, grep("^[0-9]+$", names(.x))), ~{f(.x)})) |>
      select(-country, -fleet, -catch) |>
      rename(Year = year)
  })


  num_cols <- ncol(catch_age_lst[[flt]])

  k <- kbl(catch_age_lst[[flt]],
           format = "latex",
           booktabs = TRUE,
           align = c("l",
                     rep("r", num_cols - 1)),
           linesep = "",
           escape = FALSE) |>
    kable_styling(font_size = font_size) |>
    row_spec(0, bold = TRUE)

  k
  }
