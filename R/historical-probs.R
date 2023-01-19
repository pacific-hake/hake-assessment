##' Calculate probabilities from an MCMC model to compare with estimates from past assessments
##'
##' Calculate a model's estimate of P(B_t+1 < B_t) and P(B_t+1 < B_40%)
##'  to then compare with the estimates of those quantities in year t's
##'  assessment, in combine_historical_probs().
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
##' @donttest{
##' calc_historical_probs(base_model, end = assess_yr-1)
##' @}
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



##' Calculate a model's probability of stock decline in a year and
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
##' @examples
##' @donttest{
##' combine_historical_probs(model = base_model, end = assess_yr-1)
##' @}
combine_historical_probs <- function(
    model,
    fn = file.path(rootd_data,
                     "assessment-history-probs.csv"),
    start = 2012,
    end,
    ...){

  hist_probs <- read.csv(fn,
                         comment.char = "#")

  res <- cbind(hist_probs[hist_probs$Year %in% start:end, ],
               calc_historical_probs(model,
                                     start = start,
                                     end = end,
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
##'   after historical assessment year, "decline.one.year" to show that for just
##'   one year (to explain in a talk), "bforty" to show prob of being below
##'   `B_40\%` in the year after ther historical assessment year
##' @param end final year for calculations
##' @param xLim range of x (years) axis
##' @param add.50 Whether to add horizontal line at 50%
##' @param add.50.col Colour for 50% line
##' @param one.year A single year to plot (may automatically work for more years)
##' @param add.projs Whether to add future projections from current base model
##' @param num.projs Num of projection catch levels to show
##' @param colors
##' @param pch change legend call also if change this
##' @param lwd.val
##' @param legend.cex
##' @param legend.loc
##' @param main.title
##' @param legend.text
##' @param add.to.plot whether to add to an existing plot
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' make.historical.probs.plot(base_model)
##'
make.historical.probs.plot <- function(model,
                                       type = "decline",
                                       end = assess_yr - 1,
                                       xLim = NULL,
                                       add.50 = TRUE,
                                       add.50.col = "darkgrey",
                                       one.year = 2019,
                                       add.projs = FALSE,
                                       num.projs = catch.levels.num,
                                       colors = c("red", "blue"),
                                       pch = c(16, 17),
                                       lwd.val = 1,
                                       legend.cex = 1,
                                       legend.loc = "bottomright",
                                       main.title = NULL,
                                       legend.text =
                                         c("From year t's assessment",
                                           "From current base model"),
                                       add.to.plot = FALSE,
                                       ...){
  res <- combine_historical_probs(model = model,
                                  end = end,
                                  ...)

  if(add.to.plot){
    if(type == "decline"){
      points(res$Year,
             res$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_decline[length(res$P_decline)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("SSB_", assess_yr + 1, "<SSB_",
                                         assess_yr)],
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("SSB_", assess_yr + 1, "<SSB_",
                                       assess_yr)],
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }

    if(type == "bforty"){
      points(res$Year,
             res$P_below_B40_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_below_B40[length(res$P_below_B40)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("Bratio_", assess_yr + 1, "<0.40")],
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("Bratio_", assess_yr + 1, "<0.40")],
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }
  } else {      # create new plot
    if(is.null(xLim)) xLim = range(res$Year) + c(0, 1)
    oldpar <- par("mar", "xpd")
    on.exit(par(oldpar))
    par(mar = c(4.5, 4.5, 1, 1))

    if(type == "decline"){
      plot(res$Year,
           res$P_decline,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = colors[1],
           xlab = "Year, t",
           ylab = "P(biomass declines from t to t+1)",
           ylim = c(0, 100),
           main = main.title,
           lwd = lwd.val)
      points(res$Year,
             res$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_decline[length(res$P_decline)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("SSB_", assess_yr + 1, "<SSB_",
                                         assess_yr)],
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("SSB_", assess_yr + 1, "<SSB_",
                                       assess_yr)],
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }

    if(type == "decline.one.year"){
      plot(res$Year,
           res$P_decline,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = "white",    # to get axes correct
           xlab = "Year, t",
           ylab = "P(biomass declines from t to t+1)",
           ylim = c(0, 100),
           main = main.title,
           lwd = lwd.val)

      res_one_year <- res[which(res$Year == one.year), ]

      points(res_one_year$Year,
             res_one_year$P_decline,
             type = "o",
             pch = pch[1],
             col = colors[1],
             lwd = lwd.val)

      points(res_one_year$Year,
             res_one_year$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)

      text(rep(res_one_year$Year, 2),
           c(res_one_year$P_decline,
             res_one_year$P_decline_curr),
           labels = c(paste0(res_one_year$P_decline, "%"),
                      paste0(round(res_one_year$P_decline_curr), "%")),
           col = colors,
           pos=2)
    }


    if(type == "bforty"){
      plot(res$Year,
           res$P_below_B40,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = colors[1],
           xlab = "Year, t",
           ylab = "P(biomass is below B40% in year t+1)",
           ylim = c(0, 100),
           lwd = lwd.val)
      points(res$Year,
             res$P_below_B40_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)

      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_below_B40[length(res$P_below_B40)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("Bratio_", assess_yr + 1, "<0.40")],
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("Bratio_", assess_yr + 1, "<0.40")],
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }

    }

    if(add.50){
      abline(h = 50,
             col = add.50.col) # extended past axes for some reason
    }

    legend(legend.loc,
           legend.text,
           col = colors,
           lty = 1,
           lwd = lwd.val,
           pch = pch,
           cex = legend.cex,
           bty = "y")
  }
}



#' Make individual plots or one overlaid historical probability calculations that uses
#'  retrospective runs (given we have them). Calls
#'  `make.historical.probs.plot()` for each retrospective.
#'
#' @param model Model with retrospectives
#'
#' @param type "decline" or "bforty" as needed for
#'   `make.historical.probs.plot()`
#' @param xLim range of x (years) axis
#' @param make.one.figure whether to plot everything on one figure or new figure
#'   for each retro.
#' @param cols vector colors to use
#' @param time.sleep seconds to pause between plots
#' @param omit.current omit current base model plot when doing individual plots
#'   (e.g. for panel plot don't need it, probably already shown it -- see example).
#' @param select.retros vector of retrospective runs to use, where 1:4 would be
#'   the earliest four retrospective runs (for not doing all plots to be able to
#'   spread them out over multiple pages).
#' @param lwd.val.for.retros lwd for retro lines, default of 1 is for one.figure
#' @param legend.text.model text to describe `model` in the legend
#' @param ... arguments to pass to `make.historical.probs.plot()`
#' @export
#' @examples
#' @donttest{
#' make.historical.probs.retro.plot(base_model)
#'
#' par(mfrow = c(3,3))
#' make.historical.probs.retro.plot(base_model, make.one.figure = FALSE,
#'  time.sleep = 2)
#' @}
make.historical.probs.retro.plot <- function(model,
                                             type = "decline",
                                             xLim = c(2012, assess_yr),
                                             make.one.figure = TRUE,
                                             cols = c("darkgreen",
                                                      "lightgreen",
                                                      "brown",
                                                      "pink",
                                                      "orange",
                                                      "gold",
                                                      "darkgrey",
                                                      "violet",
                                                      "black",
                                                      "lightgrey"),
                                             time.sleep = 0,
                                             omit.current = TRUE,
                                             select.retros = NULL,
                                             lwd.val.for.retros = 1,
                                             legend.text.model = "From current base model",
                                             ...){
  if(!make.one.figure){   # override default for multiple figures
    lwd.val.for.retros = 3
  }

  earliest.retro.available = length(model$retros)  # For 2021 base.case: 10
  earliest.retro.to.use = min(assess_yr - xLim[1],
                              earliest.retro.available)
                                        # Any further is before
                                        # xLim[1], want first retro year of data
                                        # to be 2011 (for 2012 calcs to compare
                                        # with 2012 assessment), which is retros[[9]].
                                        # Now for 2023 assessment, we can't got
                                        # back to data up to 2011 because first
                                        # 10th retro year back is data up to 2012.

  retros.to.use = rev(1:earliest.retro.to.use)
  if(!is.null(select.retros)){
    retros.to.use = retros.to.use[select.retros]
  }


  # Always do the current base model first, since want it when make.one.figure =
  # TRUE, but don't (for multiple plots) if omit.current = FALSE
  if(make.one.figure | !omit.current){
      make.historical.probs.plot(model,
                                 type = type,
                                 end = assess_yr - 1,
                                 xLim = xLim,
                                 add.to.plot = FALSE,
                                 lwd.val = 3,
                                 pch = c(16, 17, 15),
                                 legend.text =
                                   c("From year t's assessment",
                                     legend.text.model,
                                     "Retrospectives"),
                                 colors = c("red", "blue", "grey"),
                                 ...)
      Sys.sleep(time.sleep)
  }

  for(i in retros.to.use){
    if(!make.one.figure){
      # Still want current base model results and full legend for individual plots
      make.historical.probs.plot(model,
                                 type = type,
                                 end = assess_yr - i,
                                 xLim = xLim,
                                 add.to.plot = FALSE,
                                 lwd.val = 3,
                                 pch = c(16, 17, 15),
                                 legend.text =
                                   c("From year t's assessment",
                                     legend.text.model,
                                     paste0("Retrospective: data to ",
                                            assess_yr - 1 - i)),
                                 colors = c("red", "blue", cols[i]),
                                 ...)
    }

    make.historical.probs.plot(model$retros[[i]],
                               type = type,
                               end = assess_yr - i,
                               xLim = xLim,
                               add.to.plot = TRUE,
                               colors = c("red", cols[i]),
                               pch = c(16, 15),
                               lwd.val = lwd.val.for.retros,
                               ...)
  Sys.sleep(time.sleep)
  }
  # From squid plot, but don't think needed
  # oldpar <- par()
  # on.exit(par(oldpar))
}
