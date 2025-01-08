#' Merge Canadian DMP and LOGS catch data and extract into 3 data frames,
#' for Freezer trawlers, Shoreside, and Joint-venture
#'
#' @param lst a list of three data frames as returned by
#' [load_catch_data_canada()]
#' @return A list of three data frames, one for each gear: Freezer trawlers,
#' Shoreside, and Joint Venture
#' @export
canada_extract_fleet_catch <- function(lst){

  if(length(lst) != 4){
    stop("The length of the input list `lst` does not equal 4")
  }
  if(!all(names(lst) %in% c("dmp_df",
                            "dmp_inside_df",
                            "logs_df",
                            "logs_inside_df"))){
    stop("The names of the elements in the list `lst` are not correct. ",
         "They must be `dmp_df`, `dmp_inside_df`, `logs_df`, and ",
         "`logs_inside_df`")
  }

  # If the non-JV vessel was fishing in JV, remove those catches
  dmp_df <- lst$dmp_df |>
    dplyr::filter(!grepl("JV", licence_trip_type))
  logs_df <- lst$logs_df |>
    dplyr::filter(!grepl("JV", trip_type))

  # At Sea Observer Program records only (all of those are discards in the
  # LOGS data), for all fleet types
  discards_df <- lst$logs_df |>
    dplyr::filter(source == "ASOP") |>
    dplyr::filter(!is.na(released_wt))

  dmp_ft_df <- dmp_df |>
    dplyr::filter(vrn %in% pull(freezer_trawlers, fos_id))
  discards_ft_df <- discards_df |>
    dplyr::filter(vrn %in% pull(freezer_trawlers, fos_id))

  dmp_ss_df <- dmp_df |>
    dplyr::filter(!vrn %in% pull(freezer_trawlers, fos_id))
  discards_ss_df <- discards_df |>
    dplyr::filter(!vrn %in% pull(freezer_trawlers, fos_id))

  dmp_jv_df <- lst$dmp_df |>
    dplyr::filter(grepl("JV", licence_trip_type))
  discards_jv_df <- discards_df |>
    dplyr::filter(grepl("JV", trip_type))

  summarize_dmp <- \(d, wt_col){

    wt_col_sym <- sym(wt_col)
    d |>
      complete(landing_date = seq.Date(min(landing_date),
                                       max(landing_date),
                                       by = "day")) |>
      mutate(year = year(landing_date),
             month = month(landing_date),
             day = day(landing_date),
             !!wt_col_sym := as.numeric(!!wt_col_sym)) |>
      select(year, month, day, !!wt_col_sym) |>
      group_by(year, month, day) |>
      summarize(landings = sum(!!wt_col_sym) * lbs_to_kilos,
                count =  n()) |>
      ungroup() |>
      mutate(landings = ifelse(is.na(landings), 0, landings))
  }

  dmp_lst <- map(list(dmp_ft_df, dmp_ss_df, dmp_jv_df),
                 ~{summarize_dmp(.x, "converted_wght_lbs_")})
  discards_lst <- map(list(discards_ft_df, discards_ss_df, discards_jv_df),
                      ~{summarize_dmp(.x, "released_wt")})

  join_dmp <- \(d, d_discards){

    full_join(d,
              d_discards,
              by = c("year", "month", "day")) |>
      group_by(year, month, day) |>
      mutate(landings.x = ifelse(is.na(landings.x),
                                 0,
                                 landings.x),
             landings.y = ifelse(is.na(landings.y),
                                 0,
                                 landings.y)) |>
      summarize(landings = landings.x + landings.y,
                landings_count = count.x,
                discards_count = count.y) |>
      ungroup() |>
      mutate(discards_count = ifelse(is.na(discards_count),
                                     0,
                                     discards_count)) |>
      arrange(year, month, day)
  }

  map2(dmp_lst, discards_lst,
       join_dmp) |>
    setNames(c("ft", "ss", "jv"))
}
