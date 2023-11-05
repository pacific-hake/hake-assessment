#' Calculate probabilities from an MCMC model to compare with estimates
#' from past assessments
#'
#' @details
#' Calculate a model's estimate of P(B_t+1 < B_t) and P(B_t+1 < B_40%)
#' to then compare with the estimates of those quantities in year t's
#' assessment, in [combine_historical_probs()].
#'
#' @param model A model as loaded by [create_rds_file()]
#' @param start First assessment year to do comparisons
#' @param end Final assessment year to do comparisons
#' @return Data frame with columns
#' * `year` - year (lower case to check when binding columnsin
#'   [combine_historical_probs()]
#' * `P_decline_curr` - current estimate (from `model`) of the probability that
#'   the spawning biomass declined from year to year + 1
#' * `P_below_B40_curr` - current estimate (from `model`) of the probability that
#'   the spawning biomass was below B_40 in year + 1
#' @export
calc_historical_probs <- function(model,
                                  start = 2012,
                                  end
                                  ){
  P_decline_curr <- vector()
  P_below_B40_curr <- vector()
  year <- seq(start, end)

  for(i in 1:length(year)){
    P_decline_curr[i] <- mean(model$mcmc[[paste0("SSB_", year[i] + 1) ]] <
                              model$mcmc[[paste0("SSB_", year[i]) ]]) * 100

    P_below_B40_curr[i] <- mean(model$mcmc[[paste0("Bratio_", year[i] + 1) ]]
                                < 0.40) * 100
  }

  cbind(year,
        P_decline_curr,
        P_below_B40_curr)
}



