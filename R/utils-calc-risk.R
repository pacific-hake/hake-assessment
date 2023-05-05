#' Calculate the probabilities of being under several reference points from
#' one forecast year to the next
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of length 1 less than the number of forecast years. Each
#' element is a data.frame of catch levels holding the probabilities. For
#' example, list element 1 will hold the probabilities for each catch.level
#' of being under several reference points for the first two years in the
#' forecast_yrs vector. If forecast.outputs is NA, NA will be returned,
#' otherwise the risk.list will be returned
#' @export
calc_risk <- function(model,
                      ...){

  # Make the catch level values a matrix where the columns represent the
  # cases in catch_names
  ct_levels <- map(model$ct_levels, ~{.x[[1]]}) |>
    setNames(seq_along(model$ct_levels)) |>
    map_df(~{.x})

  metric <- function(case_ind, x, yr, yr_ind){
    out <- NULL
    out[1] <- ct_levels[yr_ind, case_ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] <
                    x[, paste0("SSB_", yr)], na.rm = TRUE) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] <
                    0.40, na.rm = TRUE) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] <
                    0.25, na.rm = TRUE) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] <
                    0.10, na.rm = TRUE) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] >
                    1.00, na.rm = TRUE) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] <
                    out[1], na.rm = TRUE) / nrow(x) * 100.0
    ## DFO values
    out[8] <- sum(x[, paste0("SSB_", yr)] >
                    x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[9] <- sum(x[, paste0("SSB_", yr)] >
                    0.4 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[10] <- sum(x[, paste0("SSB_", yr)] >
                     0.8 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    names(out) <- c(paste0("ForeCatch_", yr),
                    paste0("SSB_", yr + 1, "<SSB_", yr),
                    paste0("Bratio_", yr + 1, "<0.40"),
                    paste0("Bratio_", yr + 1, "<0.25"),
                    paste0("Bratio_", yr + 1, "<0.10"),
                    paste0("SPRratio_", yr, ">1.00"),
                    paste0("ForeCatch_", yr + 1, "<ForeCatch_", yr),
                    ## DFO values
                    paste0("SSB_", yr, ">SSB_MSY"),
                    paste0("SSB_", yr, ">0.4SSB_MSY"),
                    paste0("SSB_", yr, ">0.8SSB_MSY"))

    out
  }

  risk_lst <- NULL
  forecast_yrs <- sort(as.numeric(names(model$forecasts)))

  for(yr_ind in seq_along(forecast_yrs[-1])){
    # `outputs` is a list of one data frame per catch level for year `yr`
    outputs <- map(model$forecasts[[yr_ind]], ~{.x$outputs})
    # This call calculates the metrics for each element in the list
    # (each catch level) and binds them together into a data frame.
    risk_lst[[yr_ind]] <- map(seq_along(outputs), \(catch_level_ind){
      metric(catch_level_ind,
             outputs[[catch_level_ind]],
             forecast_yrs[yr_ind],
             yr_ind)
    }) |>
      map_df(~{.x})

  }
  names(risk_lst) <- names(model$forecasts[seq_along(model$forecasts[-1])])

  risk_lst
}
