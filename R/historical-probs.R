##' Calculate probabilities from base model to compare with estimates from past assessments
##'
##' Calculate the base model's estimate of P(B_t+1 < B_t) and P(B_t+1 < B_40%)
##'  to then compare with the estimates of those quantities in year t's
##'  assessment, in ****().
##'
##' @param model A model as loaded by [load_ss_files()]
##' @param start First assessment year to do comparisons
##' @param end Final assessment year to do comparisons
##' @return Data frame with columns
##'   * `year` - year (lower case to check when cbinding in `combine_historical_probs()`
##'   * `P_decline_curr` - current estimate (from `model`) of the probability that
##'   the spawning biomass declined from year to year+1
##'   * `P_below_B40_curr` - current estimate (from `model`) of the probability that
##'   the spawning biomass was below B_40 in year+1
##' @export
##' @author Andrew Edwards
##' @examples
##' calc_historical_probs(base.model, end = assess.yr-1)
calc_historical_probs <- function(model,
                                  start = 2012,
                                  end
                                  ){
  P_decline_curr <- vector()
  P_below_B40_curr <- vector()
  year <- seq(start, end)

  for(i in 1:length(year)){
    P_decline_curr[i] <- mean(base.model$mcmc[[paste0("SSB_", year[i] + 1) ]] <
                              base.model$mcmc[[paste0("SSB_", year[i]) ]]) * 100

    P_below_B40_curr[i] <- mean(base.model$mcmc[[paste0("Bratio_", year[i] + 1) ]]
                                < 0.40) * 100
  }

  cbind(year,
        P_decline_curr,
        P_below_B40_curr)
}



##' Calculate the base model's probability of stock decline in a year and
##'  combine with past assessment estimates
##'
##' @param model A model as loaded by [load_ss_files()]
##' @param file Filename (.csv) of historical values from past assessments
##' @param ... Further arguments to pass to `calc_historical_probs()`
##' @return Data frame with columns
##'   * `year` - year of assessment
##'   * `P_decline` - estimate from `year' assessment model of the probability that
##'   the spawning biomass will decline from `year` to `year`+1 for a catch that
##'   turned out to be the catch in `year'
##'   * `P_below_B40` - estimate from `year' assessment model of the probability that
##'   the spawning biomass will be below B_40 in `year`+1 for a catch that
##'   turned out to be the catch in `year'
##'   * `P_decline_curr` - current estimate (from `model`) of the probability that
##'   the spawning biomass declined from year to year+1
##'   * `P_below_B40_curr` - current estimate (from `model`) of the probability that
##'   the spawning biomass was below B_40 in year+1
##' @return
##' @export
##' @author Andrew Edwards
##' @examples combine_historical_probs(model = base.model, end = assess.yr-1)
combine_historical_probs <- function(model,
                                     file = paste0(rootd,
                                              "/data/assessment-history-probs.csv"),
                                     ...){
  res <- cbind(read.csv(file, comment.char = "#"),
               calc_historical_probs(model,
                                     ...)
               )
  stopifnot(res$year == res$Year)
  res <- res[ , !(names(res) %in% c("year"))]
}


##' Plots to compare historical and current probabilities
##'
##' Plot either:
##' (a) base model's estimate of P(B_t+1 < B_t) and compare with the estimate of
##' that from the year t assessment (using the known catch in year t)
##' (b) base model's estimate of P(B_t+1 < B_40%) and compare with the estimates
##' of that from the year t assessment (using the known catch in year
##' t).
##' Included in management presentation.
##' @param model
##' @param type "decline" to show probs of spawning biomass declining in year
##'   after historical assessment year, "bforty" to show prob of being below
##'   `B_40\%` in the year after ther historical assessment year
##' @param end
##' @param colors
##' @param pch
##' @param legend.cex
##' @param legend.loc
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##'  make.historical.probs.plot(base.model)
##' @donttest{
##' @
##' @}
make.historical.probs.plot <- function(model,
                                       type = "decline",
                                       end = assess.yr - 1,
                                       colors = c("red", "blue"),
                                       pch = c(16, 17),
                                       legend.cex = 1,
                                       legend.loc = "bottomright",
                                       ...){
  res <- combine_historical_probs(model = model,
                                  end = end,
                                  ...)

  oldpar <- par("mar", "xpd")
  on.exit(par(oldpar))
  par(mar = c(4.5, 4.5, 1, 1), xpd = TRUE) # xpd=TRUE allows points to overlap box around plot

  if(type == "decline"){
    plot(res$Year,
         res$P_decline,
         type = "o",
         pch = pch[1],
         col = colors[1],
         xlab = "Year, t",
         ylab = "P(biomass declines from t to t+1)",
         ylim = c(0, 100))
    points(res$Year,
           res$P_decline_curr,
           type = "o",
           pch = pch[2],
           col = colors[2])
  }

  if(type == "bforty"){
    plot(res$Year,
         res$P_below_B40,
         type = "o",
         pch = pch[1],
         col = colors[1],
         xlab = "Year, t",
         ylab = "P(biomass is below B40% in year t+1)",
         ylim = c(0, 100))
    points(res$Year,
           res$P_below_B40_curr,
           type = "o",
           pch = pch[2],
           col = colors[2])
    legend.loc = "topleft"
  }

  legend.text <- c("From year t's assessment",
                   "From current base model")

  legend(legend.loc,
         legend.text,
         col = colors,
         lty = 1,
         lwd = 2,
         pch = pch,
         cex = legend.cex,
         bty = "y")
}
