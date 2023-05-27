#' Calculate a model's probability of stock decline in a year and
#' combine with past assessment estimates
#'
#' @param model A model as loaded by [create_rds_file()]
#' @param hist_probs A data frame containing the data found in
#' `assessment-history-probs.csv`
#' @param start First assessment year to do comparisons
#' @param end Last assessment year to do comparisons
#' @param ... Further arguments to pass to [calc_historical_probs()]
#'
#' @return Data frame with columns
#' * `year` - year of assessment
#' * `P_decline` - estimate from `year' assessment model of the probability
#'    that the spawning biomass will decline from `year` to `year` + 1 for a
#'    catch that turned out to be the catch in `year'
#' * `P_below_B40` - estimate from `year' assessment model of the probability
#'    that the spawning biomass will be below B_40 in `year`+1 for a catch that
#'   turned out to be the catch in `year'
#' * `P_decline_curr` - current estimate (from `model`) of the probability that
#'   the spawning biomass declined from year to year+1
#' * `P_below_B40_curr` - current estimate (from `model`) of the probability
#'    that the spawning biomass was below B_40 in year+1
#' @export
combine_historical_probs <- function(model,
                                     hist_probs,
                                     start = 2012,
                                     end,
                                     ...){

  res <- cbind(hist_probs[hist_probs$Year %in% start:end, ],
               calc_historical_probs(model,
                                     start = start,
                                     end = end,
                                     ...)
  )
  stopifnot(res$year == res$Year)
  res <- res[ , !(names(res) %in% c("year"))]
}
