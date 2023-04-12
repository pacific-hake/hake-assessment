#' Make a plot of the age-1 index overlaid with model fit to that index
#'
#' @param model A model as output by [create_rds_file()].
#' @param legendloc See [legend()].
#' @param ylim The limits of the y axis as a range.
#'
#' @return A plot of the age-1 index fit
#' @export
make.survey.age1.plot <- function(model,
                                  legendloc = "topleft",
                                  ylim = c(0.015, 50)){

  oldpar <- par("mar")
  on.exit(par(mar = oldpar))
  par(mar = c(4, 4, 1, 1) + 0.1)

  x <- dplyr::filter(
    model[["dat"]][["CPUE"]],
    index == 3
  )
  yrs <- x$year

  recr1 <- model$extra_mcmc$natage_median %>%
    filter(Yr %in% yrs) %>%
    pull(`1`)
  recrAll <- model$extra_mcmc$natage_median %>%
    filter(Yr %in% min(yrs):max(yrs)) %>%
    pull(`1`)

  logAge1 <- log(recr1)
  logIndex <- log(x$obs)
  mn <- mean(logAge1)
  index <- mn * logIndex / mean(logIndex[!is.na(x$obs)])
  plot(min(yrs):max(yrs),
       recrAll / 1e3,
       pch = 4,
       type = "b",
       log = "y",
       ylim = ylim,
       lwd = 2,
       xaxt = "n",
       xlab = "Year",
       ylab = "Estimated age-1 fish (billions)",
       las = 1,
       col = gray(0.7),
       cex = 0.8)
  points(yrs,
         exp(index) / 1e3,
         pch = 16,
         col = "blue",
         cex = 1.5)
  points(x$year[!is.na(x$obs)],
         recr1 / 1e3,
         pch = 4,
         col = "black",
         cex = 1,
         lwd = 2)
  axis(1, at = x$year)
  legend(legendloc,
         c("Model-estimated age-1 fish",
           "Scaled acoustic survey age-1 index"),
         col = c("black", "blue"),
         pch = c(4,16),
         lty = NA,
         lwd = 2,
         bty = "o")
}

# Plot the age-1 index data only (see make.survey.age1.plot for results)
make.survey.age1.plot.data <- function(dat,
                                       log.scale = TRUE,
                                       yLim = c(10, 26)){
  ## dat - data.frame of age 1 index (from base_model$dat$CPUE)
  ## log - whether to show log scale or not
  ## yLim - ylim value, default is for log.scale = TRUE in 2022
  oldpar <- par("mar", "las", "cex.axis")
  on.exit(par(oldpar))

  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)
  # values with extrapolation used in base model
  ests <- dat[, c("year", "obs", "se_log")]

  # convert to 1000s of fish for log plot, billions for linear plot:
  if(log.scale){
    ests$obs <- ests$obs*1e3} else {
    ests$obs <- ests$obs/1e6}

  ests$lo <- exp(log(ests$obs) - 1.96 * ests$se_log)
  ests$hi <- exp(log(ests$obs) + 1.96 * ests$se_log)
  ests$value <- ests$obs

  if(log.scale){
    plotBars.fn(ests$year,
                log(ests),
                scale = 1,
                ylim = yLim,
                yaxs = 'i',
                pch = 20,
                xlab="Year",
                ylab = "Relative age-1 index estimate (log(fish))",
                cex = 1.5,
                las = 1,
                gap = 0.05,
                xaxt = "n",
                ciLwd = 3,
                ciCol = rgb(0, 0, 0, 0.5))
  }

  if(!log.scale){
    plotBars.fn(ests$year,
                ests,
                scale = 1,
                ylim = yLim,
                yaxs = 'i',
                pch = 20,
                xlab="Year",
                ylab = "Relative age-1 index (billions of fish)",
                cex = 1.5,
                las = 1,
                gap = 0.05,
                xaxt = "n",
                ciLwd = 3,
                ciCol = rgb(0, 0, 0, 0.5))
  }
  axis(1, at = ests$year, cex.axis = 0.8)
}
