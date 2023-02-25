#' Calculate the probabilities of being under several reference points from
#' one forecast year to the next
#'
#' @param forecast_outputs A list as output by [fetch_forecasts()]
#' @param catch_levels A list of catch levels which are represented by lists
#' of length 3
#' @param ... Absorb arguments intended for other functions
#'
#' @return A list of length 1 less than the number of forecast years. Each
#' element is a data.frame of catch levels holding the probabilities. For
#' example, list element 1 will hold the probabilities for each catch.level
#' of being under several reference points for the first two years in the
#' forecast_yrs vector. If forecast.outputs is NA, NA will be returned,
#' otherwise the risk.list will be returned
#' @export
calc_risk <- function(forecast_outputs = NA,
                      catch_levels,
                      ...){

  stopifnot(!is.na(forecast_outputs))

  # Make the catch level values a matrix where the columns represent the
  # cases in catch_names
  catch_levels <- sapply(catch_levels, "[[", 1)

  if(is.na(forecast_outputs)[1]){
    return(NA)
  }

  metric <- function(case_ind, x, yr, yr_ind){
    out <- NULL
    out[1] <- catch_levels[yr_ind, case_ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] < x[, paste0("SSB_", yr)], na.rm = TRUE) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40, na.rm = TRUE) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25, na.rm = TRUE) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10, na.rm = TRUE) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00, na.rm = TRUE) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1], na.rm = TRUE) / nrow(x) * 100.0
    ## DFO values
    out[8] <- sum(x[, paste0("SSB_", yr)] > x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[9] <- sum(x[, paste0("SSB_", yr)] > 0.4 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
    out[10] <- sum(x[, paste0("SSB_", yr)] > 0.8 * x[, "SSB_MSY"], na.rm = TRUE) / nrow(x) * 100.0
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
  risk_list <- vector(mode = "list", length = length(forecast_yrs) - 1)
  for(yr in 1:(length(forecast_yrs) - 1)){
    # outputs is a list of one data frame per case, for the current year yr
    outputs <- lapply(forecast_outputs[[yr]], "[[", "outputs")
    # This call calculates the metrics for each element in the list (each catch case)
    #  and binds them together into a data frame. If there was a problem,
    #  (e.g. a bridge model is set up for forecasting) it will be set to NA.
    risk_list[[yr]] <-
      do.call("rbind",
              lapply(1:length(outputs),
                     function(ind, yr, yr_ind){
                       metric(ind, outputs[[ind]], yr, yr_ind)
                     },
                     yr = forecast_yrs[yr],
                     yr_ind = yr))
  }
  names(risk_list) <- names(forecast_outputs[1:(length(forecast_outputs) - 1)])

  risk_list
}
