#' Inject SQL code for fishery categories and types based on the fleet type
#'
#' @description Injects `GFBIO VESSEL_REGISTRATION_NUMBER`s where the text
#' "-- inject vessel codes here" appears in the SQL code and `TRIP_CATEGORY`s
#' where "-- inject fishery categories here" appears
#'
#' @param sql SQL code as a vector of character strings (from [readLines()])
#' @param type one of "ft", "ss", or "jv" for Freezer Trawler, Shoreside,
#' or Joint Venture respectively
canada_sql_inject_fishery_filters <- function(sql,
                                              type = c("ft", "ss", "jv")){

  type <- match.arg(type)

  if(type == "ft"){
    search_flag = "-- inject fishery categories here"
    ind <- grep(search_flag, sql)
    sql[ind] <- paste0("(c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' or ",
                       "c.TRIP_CATEGORY = 'OPT A - QUOTA') and ")

    search_flag = "-- inject vessel codes here"
    ind <- grep(search_flag, sql)
    repl_text <- paste0("(",
                        paste("c.VESSEL_REGISTRATION_NUMBER = ",
                              freezer_trawlers$fos_id, collapse = " or "),
                        ") and ")
  }else if(type == "ss"){
    search_flag = "-- inject fishery categories here"
    ind <- grep(search_flag, sql)
    repl_text <- paste0("(c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' or ",
                        "c.TRIP_CATEGORY = 'OPT A - QUOTA') and ")

    search_flag = "-- inject vessel codes here"
    ind <- grep(search_flag, sql)
    repl_text <- paste0("(",
                        paste("c.VESSEL_REGISTRATION_NUMBER <> ",
                              freezer_trawlers$fos_id, collapse = " and "),
                        ") and")
  }else if(type == "jv"){
    search_flag = "-- inject fishery categories here"
    ind <- grep(search_flag, sql)
    repl_text <- "c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (JV)' and"
  }

  sql[ind] <- repl_text

  sql
}
