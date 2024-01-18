#' Make individual plots (or one overlaid) of historical probability calculations
#' that uses retrospective runs (given we have them). Calls
#' `make.historical.probs.plot()` for each retrospective
#'
#' @param model Model with retrospectives
#' @param hist_probs A data frame containing the data found in
#' `assessment-history-probs.csv`
#' @param type One of `decline` or `bforty` as needed for
#' [make.historical.probs.plot()]
#' @param xLim Range of x (years) axis
#' @param make.one.figure Whether to plot everything on one figure or new
#' figure for each retro.
#' @param cols Vector colors to use
#' @param time.sleep Seconds to pause between plots
#' @param omit.current Omit current base model plot when doing individual plots
#' (e.g. for panel plot don't need it, probably already shown it)
#' @param select.retros Vector of retrospective runs to use, where 1:4 would be
#' the earliest four retrospective runs (for not doing all plots to be able to
#' spread them out over multiple pages)
#' @param lwd.val.for.retros lwd for retro lines, default of 1 is for one
#' figure
#' @param legend.text.model text to describe `model` in the legend
#' @param ... Arguments to pass to [make.historical.probs.plot()]
#' @export
plot_historical_probs_retro <- function(
    model,
    hist_probs,
    type = "decline",
    xLim = c(2012, assess_yr),
    make.one.figure = TRUE,
    cols = c("lightgrey",  # rotate colours to match previous assessments
             "darkgreen",
             "lightgreen",
             "brown",
             "pink",
             "orange",
             "gold",
             "darkgrey",
             "violet",
             "black"),
    time.sleep = 0,
    omit.current = TRUE,
    select.retros = NULL,
    lwd.val.for.retros = 1,
    legend.text.model = "From current base model",
    ...){

  # Override default for multiple figures
  if(!make.one.figure){
    lwd.val.for.retros = 3
  }

  # For 2021 base.case: 10
  earliest.retro.available = length(model$retrospectives)
  earliest.retro.to.use = min(assess_yr - xLim[1],
                              earliest.retro.available)

  # Any further is before xLim[1], want first retro year of data to be 2011
  # (for 2012 calcs to compare with 2012 assessment), which is retros[[9]].
  # Now for 2023 assessment, we can't got back to data up to 2011 because first
  # 10th retro year back is data up to 2012

  retros.to.use = rev(1:earliest.retro.to.use)
  if(!is.null(select.retros)){
    retros.to.use = retros.to.use[select.retros]
  }

  # Always do the current base model first, since want it when make.one
  #.figure = `TRUE`, but don't (for multiple plots) if omit.current = `FALSE`
  if(make.one.figure | !omit.current){
    plot_historical_probs(model,
                               hist_probs = hist_probs,
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
      # Still want current base model results and full legend for
      # individual plots
      plot_historical_probs(model,
                                 hist_probs = hist_probs,
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

    plot_historical_probs(model$retrospectives[[i]],
                               hist_probs = hist_probs,
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
}
