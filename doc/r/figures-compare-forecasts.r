make.forecast.depletion.comparison.plot <- function(model,        ## model is the model for which mcmc runs with different forecasts
                                                                  ## has been made
                                                    models.inds,  ## A vector of model indices for the forecast models
                                                                  ## you want to compare. These numbers will reference
                                                                  ## list members of the base.model$forecasts$outputs list
                                                    models.names, ## a vector of names for the models (used in legend)
                                                    start.yr,     ## Year to start the plot on
                                                    model.end.yr, ## The model end year, not including forecasting.
                                                                  ## This will be pasted to Bratio_ for densitynames
                                                                  ## argument to SSplotComparisons
                                                    end.yr,       ## Year to end the plot on
                                                    legend.loc = "topleft"
                                                    ){
  ## Plots several forecasts against each other. model is typically the
  ## base model and the forecasts to be plotted are referenced by model.inds
  ## of the forecasts$outputs list. See Readme.md for reference on the
  ## model list strucure.

  oldpar <- par()
  par(mar=c(4.5,4,1,1))
  num.models <- length(models.inds)
  fore.list <- model$forecasts$outputs
  model.list <- rep(list(model), num.models)
  compare.summary <- SSsummarize(model.list)
  compare.summary$mcmc <- fore.list[models.inds]

  SSplotComparisons(compare.summary,
                    legendlabels = models.names,
                    endyr = end.yr,
                    densitynames = c(paste0("Bratio_",model.end.yr)),
                    new = FALSE,
                    minbthresh = 0,
                    subplots = 4,
                    plot = TRUE,
                    mcmc = rep(TRUE,4),
                    xlim = c(start.yr,end.yr),
                    legendloc = legend.loc,
                    labels = c("Year",
                               "Spawning biomass (t)",
                               "Relatvie Spawning Biomass",
                               "Age-0 recruits (1,000s)",
                               "Recruitment deviations",
                               "Index",
                               "Log index",
                               "SPR ratio",
                               "Density",
                               "",
                               ""),
                    btarg = -0.4,
                    staggerpoints = 1990,
                    spacepoints=200)
  abline(h=c(0.1,0.4),lty=2,col="grey")
  axis(2,at=c(0.1,0.4),las=1,cex.axis=0.8)
  axis(1,at=seq(start.yr,end.yr,2))
  par <- oldpar
}
