#' Create a table containing the catch-at-age for one of the 3 Canadian
#' or 3 U.S. fleets
#'
#' @param flt The fleet index. 1 = CAN FT, 2 = CAN SS, 3 = CAN JV,
#' 4 = US MS, 5 = US CP, 6 = US SB
#' @param font_size The size of the font in the table cells
#' @param country If `NULL`, use `flt` to make the plot. Either 1 or 2 can
#' be supplied. If 1, plot Canada. If 2, plot US
#' @param ... Arguments passed to [kableExtra::kbl()]
#'
#' @return A [kableExtra::kbl()] table
#' @export
table_catch_by_fleet <- function(flt = 1,
                                 country = NULL,
                                 font_size  = 4,
                                 ...){

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
      dplyr::filter(year %in% unique(.y$year))
  })
  # Truncate catch_df to age_df by years
  catch_lst <- map2(catch_lst, age_lst, ~{
    .x |>
      dplyr::filter(year %in% unique(.y$year))
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
      dplyr::filter(country == ifelse(!!country == 1, "Canada", "U.S.")) |>
      select(-country) |>
      mutate(across(c(-year, -catch), ~{.x = catch * .x})) |>
      split(~year) |>
      map(~{
        yrs <- .x$year
        ct <- .x$catch
        sums <- .x |>
          select(-year, -catch) |>
          colSums(na.rm = TRUE)
        sums <- c(first(yrs), sum(ct), sums)
        vec2df(sums)
      }) |>
      bind_rows()

      names(df)[1:2] <- c("Year", "Catch")

    joined_lst <- list(df)
  }

  catch_age_lst <- map(joined_lst, ~{
    .x |>
      #mutate(across(c(-Year, -Catch), ~{.x / 1e3} )) |>
      mutate(across(c(-Year), ~{f(.x)}))
  })


  num_cols <- ncol(catch_age_lst[[flt]])

  k <- kbl(catch_age_lst[[flt]],
           format = "latex",
           booktabs = TRUE,
           align = c("l",
                     rep("r", num_cols - 1)),
           linesep = "",
           escape = FALSE,
           ...) |>
    kable_styling(font_size = font_size) |>
    row_spec(0, bold = TRUE)

  k
  }
