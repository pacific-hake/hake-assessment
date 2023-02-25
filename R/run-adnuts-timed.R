#' Run [run_adnuts()] with a timer
#'
#' @param ... Arguments to pass to [run_adnuts()]
#'
#' @return Nothing
#' @export
run_adnuts_timed <- function(...){

  # Determine if the caller is calling from an Rstudio session
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"

  tm <- system.time(out <- run_adnuts(...))

  td <- seconds_to_period(tm["elapsed"])
  hrs_text <- ifelse(hour(td) == 1, " hour, ", " hours, ")
  mins_text <- ifelse(minute(td) == 1, " minute, ", " minutes, ")
  secs_text <- ifelse(second(td) == 1, " second.", " seconds.")

  msg <- paste0("Elapsed time: ",
                `if`(as.numeric(hour(td)) == 0, NULL, hour(td)),
                `if`(as.numeric(hour(td)) == 0, NULL, hrs_text),
                minute(td),
                mins_text,
                round(second(td)),
                secs_text)

  if(is_rstudio){
    message(green(symbol$play, msg))
  }else{
    message(msg)
  }

  out
}