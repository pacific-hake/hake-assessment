make.biomass.plot <- function(model,    ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                              equil.yr, ## Year in which unfished equilibium is assumed
                              start.yr, ## Year the timeseries starts (i.e. first year in model)
                              end.yr,   ## Year the timeseries ends (i.e. last year in model)
                              color = "blue"
                              ){
  oldpar <- par()

  slower <- model$mcmccalcs$slower
  smed <- model$mcmccalcs$smed
  supper <- model$mcmccalcs$supper

  unfished.eq.s <- model$mcmccalcs$sinit

  yrs <- equil.yr:end.yr
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))

  start.equil.diff <- start.yr - equil.yr
  non.equil.yrs <- yrs[-(1:start.equil.diff)]
  non.equil.smed <- smed[names(smed) %in% non.equil.yrs]

  plot(non.equil.yrs,
       non.equil.smed, type = "o", lwd = 2, ylim = c(0, max(supper) + 0.1),
       xlab = "Year", ylab = "Female Spawning Biomass (million t)",
       xlim = range(yrs),cex.axis=0.9, cex.lab = 1, mgp = c(2.3,1,0), yaxs = "i")

  axis(1,at = end.yr, cex.axis=0.9)
  axis(1,at = yrs[1], labels = "Unfished\nequilibrium", cex.axis = 0.9, mgp = c(3,1.5,0))
  points(equil.yr, unfished.eq.s[2], pch=16)
  arrows(equil.yr, unfished.eq.s[1], equil.yr, unfished.eq.s[3],
         angle = 90, code = 3, length = 0.06, col = color)
  addpoly(non.equil.yrs, slower[names(slower) %in% non.equil.yrs],
          supper[names(supper) %in% non.equil.yrs], color)
  par <- oldpar
}

make.depletion.plot <- function(model,    ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                start.yr, ## Year the timeseries starts (i.e. first year in model)
                                end.yr,   ## Year the timeseries ends (i.e. last year in model)
                                color = "blue"
                                ){
  oldpar <- par()

  dlower <- model$mcmccalcs$dlower
  dmed <- model$mcmccalcs$dmed
  dupper <- model$mcmccalcs$dupper

  yrs <- start.yr:end.yr
  ## Only include start year to end year
  dlower <- dlower[names(dlower) %in% yrs]
  dmed <- dmed[names(dmed) %in% yrs]
  dupper <- dupper[names(dupper) %in% yrs]

  par(mfrow = c(1,1), las = 1, mar = c(3.5,3.5,1,1))

  plot(yrs, dmed, type="o", lwd=2, ylim = c(0,1.1*max(dupper)), xlab="Year",
       ylab = expression(paste("Relative spawning biomass", ~~~(italic(B[t])/italic(B)[0]))),
       xlim = range(yrs), cex.axis = 0.9,
       cex.lab = 1, mgp = c(2.3,1,0), yaxs = "i")
  addpoly(yrs, dlower, dupper, color)
  abline(h = c(0.1,0.4,1), lty = 2,col = gray(0.5))
  axis(2,at = c(0.1,0.4), cex.axis = 0.8)
  axis(1, at = end.yr, cex.axis = 0.9)
  mtext("Year", side = 1,cex = 1.1, outer = TRUE, line = 1.4)
  par <- oldpar
}

make.recruitment.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                  equil.yr,         ## Year in which unfished equilibium is assumed
                                  start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                  end.yr,           ## Year the timeseries ends (i.e. last year in model)
                                  color = "blue",
                                  add.mean = FALSE,
                                  add.r0   = FALSE, ## show the posterior distribution for initial recruitment as an envelope
                                  upper.lim = 70
                                  ){
  ## Plots the recruitment for the mcmc given by model
  ## If add.mean = TRUE, the means will be shown and the unfished equilibrium recruitment will not be shown
  ## If add.mean = FALSE, the unfished equilibrium recruitment will be shown
  oldpar <- par()

  rlower <- model$mcmccalcs$rlower
  rmed <- model$mcmccalcs$rmed
  rupper <- model$mcmccalcs$rupper
  rmean <- model$mcmccalcs$rmean

  unfished.eq.r <- model$mcmccalcs$rinit

  non.equil.yrs <- start.yr:(end.yr - 1)
  yrs <- equil.yr:end.yr
  ## Only include start year to end year
  rlower <- rlower[names(rlower) %in% non.equil.yrs]
  rmed <- rmed[names(rmed) %in% non.equil.yrs]
  rupper <- rupper[names(rupper) %in% non.equil.yrs]
  rmean <- rmean[names(rmean) %in% non.equil.yrs]

  y <- data.frame(value = rmed, lo = rlower, hi = rupper)
  par(mfrow = c(1,1), las = 1, mar = c(3.5,3.5,1,1), oma = c(0,0,0,0))
  plotBars.fn(non.equil.yrs, y, scalar = 1, ylim = c(0,upper.lim), pch = 20,
              xlab = "Year", ylab = "Age 0 recruits (billions)",
              cex = 0.8, las = 1, gap = 0, xaxt = "n", ciLwd = 1,
              ciCol = rgb(0,0,1,0.5), mgp = c(2.3,1,0), xlim = range(non.equil.yrs))
  ## AME tried ylim = c(0,max(y$hi)) in above line, but 2014 recruitment
  ##  goes up to 80, squishing everything else down too much.
  if(add.mean){
    points(non.equil.yrs, rmean, pch = 4, cex = 0.8)
  }else{
    plotBars.fn(equil.yr,
                data.frame(value = unfished.eq.r[2],
                           lo = unfished.eq.r[1],
                           hi = unfished.eq.r[3]),
                scalar = 1, pch = 4, cex = 0.8, las = 1, gap = 0, ciLwd = 1,
                ciCol = rgb(0,0,1,0.5), add = TRUE)
    legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
  }
  axis(1, at = seq(equil.yr + 1, end.yr, 5))
  abline(h = 0, col = rgb(0,0,0,0.5))

  if(add.r0){
    abline(h = unfished.eq.r[2], lty = 2, col = rgb(0,0,0,0.5))
    polygon(c(0, 0, max(yrs + 10), max(yrs) + 10), c(unfished.eq.r[1], unfished.eq.r[3],
                                                     unfished.eq.r[3], unfished.eq.r[1]),
            col = rgb(0,0,0,0.1), lty = 3)
    axis(2, at = unfished.eq.r[2], label = expression(italic(R)[0]),
         cex.axis = 0.7, mgp = c(1,0.3,0), tcl = -0.2)
  }
  par <- oldpar
}

make.recruitment.dev.plot <- function(model,  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                      end.yr  ## Year the timeseries ends (i.e. last year in model). Need this so forecast years aren't shown
                                      ){
  ## Plots the recruitment deviations for the mcmc given by model
  oldpar <- par()

  dev.lower <- model$mcmccalcs$devlower
  dev.med <- model$mcmccalcs$devmed
  dev.upper <- model$mcmccalcs$devupper
  dev.yrs <- as.numeric(names(dev.med))
  dev.yrs <- dev.yrs[dev.yrs <= end.yr]
  y <- data.frame(value = dev.med, lo = dev.lower, hi = dev.upper)
  y <- y[as.numeric(rownames(y)) <= end.yr,]

  plotBars.fn(dev.yrs,
              y,
              scalar = 1,
              ylim = c(-4.5, 4.5),
              pch = 20,
              xlab = "Year",
              ylab = "Log-scale recruitment deviations ",
              cex = 0.8,
              las = 1,
              gap = 0,
              xaxt = "n",
              ciLwd = 1,
              ciCol = rgb(0, 0, 1, 0.5),
              mgp = c(2.3, 1, 0),
              xlim = c(min(dev.yrs), end.yr))
  axis(1, at = seq(min(dev.yrs) - 1, end.yr, 5))
  abline(h = 0, col = rgb(0, 0, 0, 0.5))
  abline(h = seq(-4, 4, 2), col = rgb(0, 0, 0, 0.5), lty = "13", lwd = 0.5)

  par <- oldpar
}

make.fishing.intensity.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                        start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                        end.yr,           ## Year the timeseries ends (i.e. last year that values are required for; note that SS calculates a fishing intensity for the final year of the model, but the number is meaningless).
                                        color = "blue",
                                        upper.lim = 1.4){
  ## Plots the 1-SPR / 1-SPR40% for the mcmc given by model
  oldpar <- par()

  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  yrs <- start.yr:end.yr

  ## Only include start year to end year
  plower <- plower[names(plower) %in% yrs]
  pmed <- pmed[names(pmed) %in% yrs]
  pupper <- pupper[names(pupper) %in% yrs]

  y <- data.frame(value = pmed, lo = plower, hi = pupper)
  par(mfrow = c(1,1), las = 1, mar = c(3.5,3.5,1,1), oma = c(0,0,0,0))
  plotBars.fn(yrs, y, scalar = 1, ylim = c(0,upper.lim), pch = 20,
            ## xlab="Year",ylab="Fishing intensity (1-SPR)/(1-SPR_40%)",
            xlab="Year",
            ylab = expression(paste("Rel. fishing intensity", ~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
            cex = 0.8, las = 1, gap = 0.02, xaxt = "n", ciLwd = 1, ciCol = rgb(0,0,1,0.5),
            mgp = c(2.3,1,0), xlim = range(yrs), yaxs = "i")
  axis(1, at = seq(1960, end.yr+4, 5))
  axis(1, at = seq(1960, end.yr+4, 1), lab = rep("",length(seq(1960, end.yr+4, 1))), tcl = -0.3)
  abline(h = 1, col = rgb(0,0,0,0.4))
  text(start.yr+4, 1.05, "Management Target", cex = 0.8, col = rgb(0,0,0,0.4))
  par <- oldpar
}

make.exploitation.fraction.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                            start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                            end.yr,           ## Year the timeseries ends (i.e. last year that values are required for; note that SS calculates an exploitation rate for the final year of the model, but the number is meaningless because that year has no catch).
                                            color = "blue",
                                            upper.lim = 0.4){
  ## Plots the exploitation fraction for the mcmc given by model
  oldpar <- par()

  flower <- model$mcmccalcs$flower
  fmed <- model$mcmccalcs$fmed
  fupper <- model$mcmccalcs$fupper

  yrs <- start.yr:end.yr

  ## Only include start year to end year
  flower <- flower[(names(flower) %in% yrs)]
  fmed <- fmed[(names(fmed) %in% yrs)]
  fupper <- fupper[(names(fupper) %in% yrs)]

  y <- data.frame(value=fmed,lo=flower,hi=fupper)

  par(mfrow = c(1,1), las = 1, mar = c(3.5,4.5,1,1), oma = c(0,1,0,0))
  plotBars.fn(yrs, y, scalar = 1, ylim = c(0,upper.lim), pch = 20, xlab = "Year", ylab = "",
              cex = 0.8, las = 1, gap = 0.005, xaxt = "n", ciLwd = 1, ciCol = rgb(0,0,1,0.5),
              mgp = c(2.3,1,0), xlim = range(yrs), yaxs="i")
  axis(1, at = seq(1960, end.yr+4, 5))
  axis(1, at = seq(1960, end.yr+4, 1), lab = rep("",length(seq(1960, end.yr+4, 1))), tcl = -0.3)
  mtext("Exploitation fraction", side=2, line=3, las=0)
  par <- oldpar
}

make.phase.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                            start.yr,         ## Year the timeseries starts (i.e. first year in model)
                            end.yr,           ## Year the timeseries ends (i.e. last year in model)
                            color = "blue"
                            ){
  ## Plots the relative fishing intensity and relative spawning biomass as a historical
  ## look at the fishery for the mcmc given by model
  oldpar <- par()

  yrs <- start.yr:end.yr

  dlower <- model$mcmccalcs$dlower
  dmed <- model$mcmccalcs$dmed
  dupper <- model$mcmccalcs$dupper

  sb40 <- model$mcmccalcs$sinit[2] * 0.4
  sb0 <- model$mcmccalcs$sinit[2]

  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  ## Only include start year to end year
  dlower <- dlower[(names(dlower) %in% yrs)]
  dmed <- dmed[(names(dmed) %in% yrs)]
  dupper <- dupper[(names(dupper) %in% yrs)]
  plower <- plower[(names(plower) %in% yrs)]
  pmed <- pmed[(names(pmed) %in% yrs)]
  pupper <- pupper[(names(pupper) %in% yrs)]

  sb <- dmed[yrs %in% c(start.yr:(end.yr-1))]
  sb.hi <- dupper[yrs %in% (end.yr-1)]
  sb.lo <- dlower[yrs %in% (end.yr-1)]

  spr <- pmed[yrs %in% c(start.yr:(end.yr-1))]
  spr.hi <- pupper[yrs %in% (end.yr-1)]
  spr.lo <- plower[yrs %in% (end.yr-1)]

  ## par(mfrow=c(1,1), las = 1, mar = c(3.6,3.6,1,1), oma = c(0,0,0,0))
  par(las = 1, mar = c(3.6, 3.6, 1, 1), oma = c(0, 0, 0, 0))
  plot(sb, spr, type = "n", pch = 20, xlim = c(0,1.3), ylim = c(0,1.3),
       xlab = expression(paste("Relative spawning biomass", ~~~(italic(B[t])/italic(B)[0]))),
       ylab = expression(paste("Relative fishing intensity", ~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
       xaxs = "i", yaxs = "i", mgp = c(2.4,1,0))
  colvec <- rev(rich.colors.short(n = length(sb))[-1])
  arrows(sb[-length(sb)], spr[-length(spr)], sb[-1], spr[-1], length=0.09,
         ## col = rgb(0,0,0,0.4))
         col = colvec)
  # add points for each year
  points(sb, spr, pch = 21, col=1, bg=colvec)
  # add uncertainty intervals for final year
  segments(sb[length(sb)], spr.lo, sb[length(sb)], spr.hi, col = rgb(0,0,0,0.5))
  segments(sb.lo, spr[length(spr)], sb.hi, spr[length(spr)], col = rgb(0,0,0,0.5))
  # label first and final years
  text(sb[1], spr[1] - 0.01, start.yr, cex = 0.6, pos = 1,
       col=colvec[1])
  text(sb[length(sb)], spr[length(spr)] + 0.01, end.yr-1, pos = 4, cex = 0.6,
       col=colvec[length(spr)-1])
  # add label to year in upper-left quadrant if there's only one
  # this facilitates labeling 1999 in the 2017 assessment, but may work for future years
  upper.left.yrs <- yrs[sb < 0.4 & spr > 1]
  if(length(upper.left.yrs)==1){
    text(x=sb[yrs %in% upper.left.yrs], y=spr[yrs %in% upper.left.yrs] + 0.015,
         labels=upper.left.yrs, pos = 4, cex = 0.6,
         col=colvec[yrs %in% upper.left.yrs])
  }
  # add lines at the reference points
  abline(h = 1, v = 1, lty = 2, col = rgb(0,0,0,0.4))
  abline(h = 1, v = c(0.1,0.4), lty = 2, col = rgb(0,0,0,0.4))
  # add bigger points for first and final years
  points(sb[length(sb)], spr[length(spr)], pch = 21, col = 1,
         bg=colvec[length(spr)-1], cex = 1.2)
  points(sb[1],spr[1], pch = 21, col = 1, bg=colvec[1], cex = 1.2)

  par <- oldpar
}

make.squid.plot <- function(models,      ## A list of models to compare (typically base and its retrospectives)
                            subplot = 1, ## 1 = Recruitment devs, 2 = Rec dev strength relative to most recent estimate
                            cohorts){    ## Vector of years to plot the cohorts for
  ## Plot the retrospective recruitment deviations AKA "squid" plot to outline
  ##  deviation estimates changing for cohorts between assessment years

  oldpar <- par()
  compare.summary <- SSsummarize(models, SpawnOutputUnits="biomass")
  neg.yr.vec <- seq(0, -(length(models) - 1), -1)
  endyrvec <- compare.summary$endyrs + 1 + neg.yr.vec
  if(subplot == 1){
    relative = FALSE
  }else{
    relative = TRUE
  }
  SSplotRetroRecruits(compare.summary,
                      endyrvec = endyrvec,
                      cohorts = cohorts,
                      ylim = NULL,
                      uncertainty = FALSE,
                      main="",
                      mcmcVec = FALSE,
                      devs = TRUE,
                      relative = relative,
                      labelyears = TRUE,
                      legend = FALSE,
                      leg.ncols = 4)
  par <- oldpar
}

make.comparison.plot <- function(models,                   ## models is a list of model runs to be plotted against of which
                                                           ## each element is the output of the r4ss package's function SSgetMCMC
                                 subplots = 1,             ## Same as subplot argument in SSplotComparisons
                                 model.names = NULL,       ## vector of model names. Must be same length as models
                                 densitynames = NULL,      ## Same as densitynames argument in SSplotComparisons
                                 densityxlab  = NULL,      ## X-axis label
                                 indexPlotEach = FALSE,    ## Same as indexPlotEach argument in SSplotComparisons
                                 indexUncertainty = FALSE, ## Same as indexUncertainty argument in SSplotComparisons
                                 indexfleets = NULL,       ## Passed to the SSplotComparisons function
                                 is.retro = FALSE,         ## Is this a retrospective plot?
                                 legend = TRUE,            ## Passed to the SSplotComparisons function
                                 legendloc = "topright",   ## Passed to the SSplotComparisons function
                                 end.yr = NULL             ## End year of the plot. If is.retro is TRUE, this is ignored and endyrvec is calculated
                                 ){
  ## Plot the list of models against each other.
  ## if model.names is null, the directory names will be used
  oldpar <- par()
  if(is.null(model.names)){
    tmp.names <- sapply(models[1:length(models)], "[[", "path")
    model.names <- gsub(".*/","", tmp.names)
  }
  compare.summary <- SSsummarize(models, SpawnOutputUnits="biomass")
  endyrvec <- "default"
  ## If it is a retropective plot, compute the end year vector of years so the lines end on the correct years
  if(is.retro){
    endyrvec <- compare.summary$endyrs + 1 + (0:-(length(models) - 1))
  }else{
    if(!is.null(end.yr)){
      endyrvec <- end.yr
    }
  }
  if(is.null(densitynames)){
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "", "", "Spawning output",
                                 "Harvest rate"))
  }else{
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      densitynames = densitynames,
                      densityxlabs = densityxlab,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "", "", "Spawning output",
                                 "Harvest rate"))
  }
  par <- oldpar
}

make.comparison.plot.mcmc <- function(
            models,                   ## models is a list of model runs to be plotted against of which
                                      ## each element is the output of the r4ss package's function SSgetMCMC
            subplots = 1,             ## Same as subplot argument in SSplotComparisons
            model.names = NULL,       ## vector of model names. Must be same length as models
            densitynames = NULL,      ## Same as densitynames argument in SSplotComparisons
            indexPlotEach = FALSE,    ## Same as indexPlotEach argument in SSplotComparisons
            indexUncertainty = FALSE, ## Same as indexUncertainty argument in SSplotComparisons
            indexfleets = NULL,       ## Passed to the SSplotComparisons function
            is.retro = FALSE,         ## Is this a retrospective plot?
            legend = TRUE,            ## Passed to the SSplotComparisons function
            legendloc = "topright",   ## Passed to the SSplotComparisons function
            end.yr = NULL,            ## End year of the plot. If is.retro is TRUE, this is ignored and endyrvec is calculated
            xlims="default",          ## xlim to zoom on a specific area
            mcmc = rep(TRUE,length(models))   ## model numbers to plot mcmc instead of MLE
            ){
  ## Plot the list of models against each other.
  ## if model.names is null, the directory names will be used
  ## Used in the SRG management presentation (beamer)
  oldpar <- par()
  if(is.null(model.names)){
    tmp.names <- sapply(models[1:length(models)], "[[", "path")
    model.names <- gsub(".*/","", tmp.names)
  }
  compare.summary <- SSsummarize(models, SpawnOutputUnits="biomass")
  compare.summary$mcmc <- vector(mode="list",length=length(models))
  for(i in 1:length(models)) {
    compare.summary$mcmc[[i]] <- models[[i]]$mcmc
  }

  endyrvec <- "default"
  ## If it is a retrospective plot, compute the end year vector of years so the lines end on the correct years
  if(is.retro){
    endyrvec <- compare.summary$endyrs + 1 + (0:-(length(models) - 1))
  }else{
    if(!is.null(end.yr)){
      endyrvec <- end.yr
    }
  }
  if(is.null(densitynames)){
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      xlim=xlims,
                      mcmc = TRUE,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "", "", "Spawning output",
                                 "Harvest rate"))
  }else{
    SSplotComparisons(compare.summary,
                      subplots = subplots,
                      legend = legend,
                      legendlabels = model.names,
                      legendloc = legendloc,
                      densitynames = densitynames,
                      indexPlotEach = indexPlotEach,
                      indexUncertainty = indexUncertainty,
                      indexfleets = indexfleets,
                      endyrvec = endyrvec,
                      xlim=xlims,
                      mcmc = mcmc,
                      new=FALSE,
                      labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                                 "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                                 "", "", "Spawning output",
                                 "Harvest rate"))
  }
  par <- oldpar
}
