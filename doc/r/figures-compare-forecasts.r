make.forecast.catch.posterior.plot <- function(model,         ## model is the model for which mcmc runs with different forecasts
                                                              ## has been made
                                               fore.yr,       ## Forecast year to make the plot with
                                               do.plot = TRUE ## If FALSE, no plot will be drawn, but the return values will be returned
                                               ){
  if(do.plot){
    oldpar <- par()
  }
  dat <- model$mcmc
  dat <- eval(parse(text = paste0("dat$ForeCatch_", fore.yr))) / 1000.0
  med.catch <- median(dat)
  dens <- density(dat, from = min(dat))
  dens.orig <- dens
  if(do.plot){
    plot(dens,
       yaxt = "n",
       ylab = "",
       xlab = paste0("Projected ",fore.yr," catch based on the default harvest policy ('000 t)"),
       type = "l",
       lty = 1,
       pch = 16,
       xlim = c(0, 2500),
       xaxs = "i",
       yaxs = "i",
       ylim = c(0, max(dens$y) * 1.02), lwd = 3, main="", xaxt = "n")

    axis(1, at = seq(0, 2600, 200))
    mtext("Density", side = 2, line = 0.5, las = 0)
  }
  yy <- dens
  yy$x <- c(min(dens$x), dens$x, max(dens$x))
  yy$y <- c(-1, dens$y, -1)
  if(do.plot){
      polygon(yy$x, yy$y, col = gray(0.9), lwd = 3)
  }
  ind <- dens$x >= quantile(dat, 0.025) & dens$x <= quantile(dat, 0.975)
  dens$x <- dens$x[ind]
  dens$y <- dens$y[ind]
  yy$x <- c(min(dens$x), dens$x, max(dens$x))
  yy$y <- c(-1, dens$y, -1)
  if(do.plot){
  polygon(yy$x, yy$y, col = rgb(0, 0, 1, 0.3), lty = 0)
  lines(dens.orig, lwd = 3)
  }
  if(do.plot){
  tmpy <- dens$y[min(abs(dens$x - med.catch)) == abs(dens$x - med.catch)]
  lines(c(med.catch, med.catch), c(0, tmpy), lwd = 2)
  text(med.catch, mean(c(0, tmpy)), paste0("Median = ", round(med.catch,3)), srt = 90, adj = c(0.5, -0.5))
  box()
  par <- oldpar
  }
  if(!do.plot){
    ret.vec <-c(min(yy$x), med.catch, max(yy$x))
    names(ret.vec) <- c("lower", "median", "upper")
    return(ret.vec)
  }
}

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
                               "Relative Spawning Biomass",
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

make.forecast.risk.comparison.plot <- function(model,        ## model is the model for which mcmc runs with different forecasts
                                                             ## has been made
                                               forecast.yrs, ## a vector of the years which were forecast,
                                                             ## used in conjunction with fore.yr to determine which
                                                             ##  forecast list element is used
                                               fore.yr,      ## Forecast year for probabilities
                                               colors = c("black","blue","green","orange","red","tan"),
                                                             ## color is a vector of colors for the lines. This must be the same length
                                                             ## as the number of rows models[[N]]$risks data frames has
                                               pch = c(16,17,17,17,15,18),
                                                             ## pch is a vector of symbols. This must be the same length
                                                             ## as the number of rows models[[N]]$risks data frames has
                                               legend.cex = 0.7,   ## Text size for the legend
                                               legend.loc = "topleft"){
  oldpar <- par()
  par(mar=c(4.5,4.5,1,1))
  prob.dat <- model$risks[fore.yr == forecast.yrs][[1]]
  ## Sort the table by catches
  prob.dat <- prob.dat[order(prob.dat[,1]),]
  ## Divide all the percentages by 100 to get probabilities
  prob.dat[,-1] <- prob.dat[-1] / 100.0
  ## Divide all catches by 1000 to get megatonnes
  catches <- round(prob.dat[,1] / 1000.0, 0)
  ## Remove catches from the table
  prob.dat <- prob.dat[,-1]

  legend.text <- c(paste0("P(B",fore.yr+1,"<B",fore.yr,"): Stock declines in ",fore.yr+1),
                   paste0("P(B",fore.yr+1,"<B40%)"),
                   paste0("P(B",fore.yr+1,"<B25%)"),
                   paste0("P(B",fore.yr+1,"<B10%)"),
                   paste0("P(",fore.yr," Fishing intensity > Target of 40%)"),
                   paste0("P(C",fore.yr+1,"<C",fore.yr,"): F40% catch declines in ",fore.yr+1))

  matplot(catches, prob.dat,
          xlim=c(0,max(catches)),
          ylim=c(0,1),
          xaxt="n",
          ylab="Probability",
          xlab=paste0("Catch in ",fore.yr," ('000 t)"),
          type="b",
          lty=2,
          pch=pch,
          col=colors)
  abline(h=0.5, lty=2, lwd=1, col="grey")
  axis(1, at=catches, cex.axis=0.9, las=2)
  legend(legend.loc, legend.text, col=colors, lty=1, lwd=2, pch=pch, cex=legend.cex, bty="n")
  par <- oldpar
}
