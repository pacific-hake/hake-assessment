#' Calculate a model's probability of stock decline in a year and
#' combine with past assessment estimates
#'
#' @param model A model as loaded by [create_rds_file()]
#' @param start First assessment year to do comparisons
#' @param end Last assessment year to do comparisons
#' @param ... Further arguments to pass to [calc_historical_probs()]
#'
#' @return Data frame with columns
#' * `Year` - year of assessment
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
                                     start = 2012,
                                     end,
                                     ...){

  hist_probs_rng <- assess_history_probs_df |>
    dplyr::filter(Year %in% start:end) |>
    rename(year = Year)
  new_hist_probs_rng <- calc_historical_probs(model,
                                              start = start,
                                              end = end
                                              ) |>
    as_tibble()

  if(nrow(new_hist_probs_rng) < nrow(hist_probs_rng)){
    stop("The number of rows (years) in the model-calculated historical ",
         "probabilities is less than what appears in the ",
         "`assess_history_probs_df` package data frame (from CSV file)")
  }

  if(nrow(new_hist_probs_rng) > nrow(hist_probs_rng)){
    # Add new row(s) of NA's to the old table, if the new one has more
    # years than the old
    hist_probs_rng <- hist_probs_rng |>
      complete(year = sort(new_hist_probs_rng$year),
               fill = list(value = NA))
  }

  hist_probs_rng |>
    full_join(new_hist_probs_rng, by = "year")
}
