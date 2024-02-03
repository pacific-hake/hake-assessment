#' Add a new year of biomass dispersion stats to the CSV file holding all
#' years and return the data frame
#'
#' @details
#' Adds the current year to the CSV file if it is not present in it
#'
#' @param model A model object
#' @param yr The year to calculate the statistics for
#'
#' @return The contents of the `data-tables/"assessment-history-dispersion.csv`
#' file, after adding a new year if necessary
#'
#' @export
calc_retro_stats <- function(model,
                             yr = year(Sys.Date()),
                             fn = here("data-tables",
                                       "assessment-history-dispersion.csv")){

  if(!file.exists(fn)){
    stop("File `", fn, "` does not exist")
  }

  assess_history_disp_df <- read_csv(fn,
                                     col_types = cols(),
                                     comment = "#",
                                     show_col_types = FALSE)

  if(yr %in% assess_history_disp_df$Assessment_Year){
     return(assess_history_disp_df)
  }

  yr_colname <- paste0("SSB_", yr)
  if(!yr_colname %in% names(model$mcmc)){
    stop("The year '", yr, "' (colname ", yr_colname, ") does not appear to ",
         "be present in the `mcmc` data frame in the model object passed in")
  }
  yr_col <- sym(as.character(yr))
  ssb_vec <- get_post_cols(model$mcmc, "^SSB", 1e3) |>
    select(yr_col) |>
    unlist()

  q <- quantile(ssb_vec, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    round(1)
  q <- unname(q)
  qcd <- ((q[4] - q[2]) / (q[4] + q[2])) |>
    round(9)
  qcd <- unname(qcd)
  iqr <- IQR(ssb_vec) |>
    round(1)
  iqr <- unname(iqr)

  df <- tibble(Assessment_Year = yr,
               Terminal_SSB_med = q[3],
               Terminal_SSB_5 = q[1],
               Terminal_SSB_95 = q[5],
               First_Quartile = q[2],
               Third_Quartile = q[4],
               InterQuartileRange = iqr,
               QuartileCoeffDispersion = qcd)

  assess_history_disp_df <- assess_history_disp_df |>
    bind_rows(df)

  help_msg <- paste0("# To add a new year, run calc_retro_stats",
                     " (base_model) each year (while currently in the ",
                     "year you want to add)")
  writeLines(help_msg, fn)
  write_csv(assess_history_disp_df,
            fn,
            append = TRUE,
            col_names = TRUE)

  assess_history_disp_df
}
#
#
