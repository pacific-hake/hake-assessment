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
       non.equil.smed,
       type = "o",
       lwd = 2,
       ylim = c(0, max(supper) + 0.1),
       xlab = "Year",
       ylab = "Female Spawning Biomass (million t)",
       xlim = range(yrs),
       cex.axis =0.9,
       cex.lab = 1,
       mgp = c(2.3,1,0),
       xaxt = "n",
       yaxs = "i")

  axis(1, at = big.ticks)
  axis(1,
       at = little.ticks,
       lab = rep("",length(little.ticks)), tcl = -0.3)
  axis(1,
       at = yrs[1],
       lab = paste0("Unfished\nequilibrium"),
       cex.axis = 0.9,
       mgp = c(3,2.5,0))
  box()
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

  plot(yrs,
       dmed,
       type = "o",
       lwd = 2,
       ylim = c(0,1.1*max(dupper)),
       xlab = "Year",
       ylab = expression(paste("Relative spawning biomass", ~~~(italic(B[t])/italic(B)[0]))),
       xlim = range(yrs),
       cex.axis = 0.9,
       cex.lab = 1,
       mgp = c(2.3,1,0),
       xaxt = "n",
       yaxs = "i")
  axis(1, at = big.ticks)
  axis(1,
       at = little.ticks,
       lab = rep("",length(little.ticks)), tcl = -0.3)
  axis(2,at = c(0.1,0.4), cex.axis = 0.8)
  addpoly(yrs, dlower, dupper, color)
  abline(h = c(0.1,0.4,1), lty = 2,col = gray(0.5))
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
                                  upper.lim = NULL  ## upper limit of y-axis (specify if necessary, else will be automatic)
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

  #non.equil.yrs <- start.yr:(end.yr - 1) # former setting to exclude final year
  non.equil.yrs <- start.yr:end.yr
  yrs <- equil.yr:end.yr
  ## Only include start year to end year
  rlower <- rlower[names(rlower) %in% non.equil.yrs]
  rmed <- rmed[names(rmed) %in% non.equil.yrs]
  rupper <- rupper[names(rupper) %in% non.equil.yrs]
  rmean <- rmean[names(rmean) %in% non.equil.yrs]
  if(is.null(upper.lim)){
     upper.lim = max(rupper)
  }

  y <- data.frame(value = rmed, lo = rlower, hi = rupper)

  par(mfrow = c(1,1), las = 1, mar = c(3.5,3.5,1,1), oma = c(0,0,0,0))
  plotBars.fn(non.equil.yrs,
              y,
              ylim = c(0,upper.lim),
              pch = 20,
              xlab = "Year",
              ylab = "Age 0 recruits (billions)",
              cex = 0.8,
              las = 1,
              gap = 0,
              xaxt = "n",
              ciLwd = 1,
              ciCol = rgb(0,0,1,0.5),
              mgp = c(2.3,1,0),
              xlim = range(non.equil.yrs),
              xaxt = "n")
  ## AME tried ylim = c(0,max(y$hi)) in above line, but 2014 recruitment
  ##  goes up to 80, squishing everything else down too much.
  if(add.mean){
    points(non.equil.yrs, rmean, pch = 4, cex = 0.8)
  }else{
    plotBars.fn(equil.yr,
                data.frame(value = unfished.eq.r[2],
                           lo = unfished.eq.r[1],
                           hi = unfished.eq.r[3]),
                pch = 4,
                cex = 0.8,
                las = 1,
                gap = 0,
                ciLwd = 1,
                ciCol = rgb(0,0,1,0.5),
                xaxt = "n",
                add = TRUE)
    legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
  }
  axis(1, at = big.ticks)
  axis(1,
       at = little.ticks,
       lab = rep("",length(little.ticks)), tcl = -0.3)
  ##axis(1, at = seq(equil.yr + 1, end.yr, 5))
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

  par(mfrow = c(1,1), las = 1, mar = c(3.5,3.5,1,1), oma = c(0,0,0,0))
  plotBars.fn(dev.yrs,
              y,
              ylim = c(-4.5, 4.5),
              pch = 20,
              xlab = "Year",
              ylab = "Log-scale recruitment deviations ",
              cex = 0.8,
              gap = 0,
              xaxt = "n",
              ciLwd = 1,
              ciCol = rgb(0, 0, 1, 0.5),
              mgp = c(2.3, 1, 0),
              xlim = c(min(dev.yrs), end.yr))
  ##axis(1, at = seq(min(dev.yrs) - 1, end.yr, 5))
  axis(1, at = seq(min(dev.yrs) - 1, end.yr+4, 5))
  axis(1,
       at = seq(min(dev.yrs) - 1, end.yr+4, 1),
       lab = rep("",length(seq(min(dev.yrs) - 1, end.yr+4, 1))), tcl = -0.3)
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
  plotBars.fn(yrs, y, ylim = c(0,upper.lim), pch = 20,
            ## xlab="Year",ylab="Fishing intensity (1-SPR)/(1-SPR_40%)",
            xlab="Year",
            ylab = expression(paste("Rel. fishing intensity", ~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
            cex = 0.8, las = 1, gap = 0.02, xaxt = "n", ciLwd = 1, ciCol = rgb(0,0,1,0.5),
            mgp = c(2.3,1,0), xlim = range(yrs), yaxs = "i")
  axis(1, at = big.ticks)
  axis(1, at = little.ticks, lab = rep("",length(little.ticks)), tcl = -0.3)
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
  plotBars.fn(yrs, y, ylim = c(0,upper.lim), pch = 20, xlab = "Year", ylab = "",
              cex = 0.8, las = 1, gap = 0.005, xaxt = "n", ciLwd = 1, ciCol = rgb(0,0,1,0.5),
              mgp = c(2.3,1,0), xlim = range(yrs), yaxs="i")
  axis(1, at = big.ticks)
  axis(1, at = little.ticks, lab = rep("",length(little.ticks)), tcl = -0.3)
  mtext("Exploitation fraction", side=2, line=3, las=0)
  par <- oldpar
}

make.phase.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                            start.yr,         ## Year the timeseries starts (i.e. first year in model)
                            end.yr,           ## Year the timeseries ends (i.e. last year in model)
                            cex.lab = 1,
                            x.max = 1.5,      ## maximum of x-axis (1.3 for base
                                              ##  model in 2019 and 2020)
                            y.max = 1.2       ## maximum of y-axis (1.3 for 2019, 2020)
                            ){
  ## Plots the relative fishing intensity in year t-1 against relative spawning
  ## biomass in year t, as a historical look at the fishery for the mcmc given by model
  oldpar <- par()

  yrs <- start.yr:end.yr

  dlower <- model$mcmccalcs$dlower
  dmed   <- model$mcmccalcs$dmed
  dupper <- model$mcmccalcs$dupper

  plower <- model$mcmccalcs$plower
  pmed   <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  ## Only include start year to end year (results contain extra calculations),
  ##  but don't need dmed[start.yr], since no pmed[start.yr-1] to plot it with,
  ##  or pmed[end.yr] since that's meaningless here (yet SS calculates it)
  ## as we don't know catch in end.yr.
  dmed   <- dmed[(names(dmed) %in% yrs[-1])]
  pmed   <- pmed[(names(pmed) %in% yrs[-length(yrs)])]

  ## Credible intervals for final year:
  dlower.final.yr <- dlower[names(dlower) %in% max(names(dmed)) ]
  dupper.final.yr <- dupper[names(dupper) %in% max(names(dmed)) ]

  plower.final.yr <- plower[names(plower) %in% max(names(pmed)) ]
  pupper.final.yr <- pupper[names(pupper) %in% max(names(pmed)) ]

  par(las = 1,
      mar = c(3.6, 3.6, 1, 1),
      oma = c(0, 0, 0, 0))
  plot(dmed,
       pmed,
       type = "n",
       pch = 20,
       xlim = c(0, x.max),
       ylim = c(0, y.max),
       xlab = expression(paste("Relative spawning biomass", ~~~(italic(B[t])/italic(B)[0]))),
       ylab = expression(paste("Relative fishing intensity", ~~(1-SPR[t-1])/(1-SPR['40%']))),
       xaxs = "i",
       yaxs = "i",
       mgp = c(2.4, 1,0),
       cex.lab = cex.lab)
  colvec <- rev(rich.colors.short(n = length(dmed))[-1])
  arrows(dmed[-length(dmed)],
         pmed[-length(pmed)],
         dmed[-1],
         pmed[-1],
         length=0.09,
         col = colvec)
  # add points for each year:
  points(dmed,
         pmed,
         pch = 21,
         col=1,
         bg=colvec)
  # add uncertainty intervals for final year:
  segments(dmed[length(dmed)],
           plower.final.yr,
           dmed[length(dmed)],
           pupper.final.yr,
           col = rgb(0,0,0,0.5))
  segments(dlower.final.yr,
           pmed[length(pmed)],
           dupper.final.yr,
           pmed[length(pmed)],
           col = rgb(0,0,0,0.5))
  # label first and final years:
  text(dmed[1],
       pmed[1] - 0.01,
       names(dmed[1]),
       cex = 0.6,
       pos = 1,
       col=colvec[1])
  text(dmed[length(dmed)],
       pmed[length(pmed)] + 0.015,
       names(dmed[length(dmed)]),
       pos = 2,
       cex = 0.6,
       col=colvec[length(colvec)])

  # Add label to year with highest fishing intensity:
  highest.fish.int.yr <- yrs[which.max(pmed)]
  text(x = dmed[yrs %in% highest.fish.int.yr],
       y = pmed[yrs %in% highest.fish.int.yr],
       labels = highest.fish.int.yr + 1,
       pos = 4,
       cex = 0.6,
       col=colvec[yrs %in% highest.fish.int.yr])

  # add lines at the reference points
  abline(h = 1,
         v = 1,
         lty = 2,
         col = rgb(0,0,0,0.4))
  abline(h = 1,
         v = c(0.1, 0.4),
         lty = 2,
         col = rgb(0,0,0,0.4))

  # add bigger points for first and final years
  points(dmed[1],
         pmed[1],
         pch = 21,
         col = 1,
         bg=colvec[1],
         cex = 1.2)
  points(dmed[length(dmed)],
         pmed[length(pmed)],
         pch = 21,
         col = 1,
         bg=colvec[length(pmed)-1],
         cex = 1.2)

  par <- oldpar
}

#' @param getdevs A logical value specifying if the plot should be of
#' recruitment deviations, which is the default. If \code{FALSE}, then
#' the squid plot will be made using absolute recruitment instead of
#' deviations.
make.squid.plot <- function(models,      ## A list of models to compare (typically base and its retrospectives)
                            subplot = 1, ## 1 = Recruitment devs, 2 = Rec dev strength relative to most recent estimate
                            cohorts,
                            getdevs = TRUE){    ## Vector of years to plot the cohorts for
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
  plot <- SSplotRetroRecruits(compare.summary,
                      endyrvec = endyrvec,
                      cohorts = cohorts,
                      ylim = NULL,
                      uncertainty = FALSE,
                      main="",
                      mcmcVec = FALSE,
                      devs = getdevs,
                      relative = relative,
                      labelyears = TRUE,
                      legend = FALSE,
                      leg.ncols = 4)
  par <- oldpar
  return(plot)
}

#' Plot a comparison of multiple models
#'
#' @param models A list of model runs to be plotted against of which each
#' element is the output of [r4ss::SSgetMCMC()]
#' @param subplots See [r4ss::SSplotComparisons()]
#' @param model.names A vector of model names. Must be same length as `models`.
#' If `NULL`, the directory names will be used
#' @param densitynames See [r4ss::SSplotComparisons()]
#' @param densityxlab X-axis label
#' @param indexPlotEach See [r4ss::SSplotComparisons()]
#' @param indexUncertainty See [r4ss::SSplotComparisons()]
#' @param indexfleets See [r4ss::SSplotComparisons()]
#' @param is.retro If `TRUE`, it is a retrospective plot
#' @param legend See [r4ss::SSplotComparisons()]
#' @param legendloc See [r4ss::SSplotComparisons()]
#' @param end.yr End year of the plot. If is.retro is TRUE, this is ignored
#' and the `endyrvec` argument for [r4ss::SSplotComparisons()] is calculated
#' @param plot_mcmc If `TRUE`, plot MCMC values. If `FALSE`, plot MLE values
#' @param verbose Be verbose on output
make.comparison.plot <- function(models,
                                 subplots = 1,
                                 model.names = NULL,
                                 densitynames = NULL,
                                 densityxlab  = NULL,
                                 indexPlotEach = FALSE,
                                 indexUncertainty = FALSE,
                                 indexfleets = NULL,
                                 is.retro = FALSE,
                                 legend = TRUE,
                                 legendloc = "topright",
                                 end.yr = NULL,
                                 plot_mcmc = TRUE,
                                 verbose = FALSE){

  if(is.null(model.names)){
    tmp.names <- sapply(models[1:length(models)], "[[", "path")
    model.names <- gsub(".*/", "", tmp.names)
  }
  compare.summary <- SSsummarize(models, SpawnOutputUnits = "biomass")
  endyrvec <- "default"
  # If it is a retrospective plot, compute the end year vector of years so the lines end on the correct years
  if(is.retro){
    endyrvec <- compare.summary$endyrs + 1 + (0:-(length(models) - 1))
  }else{
    if(!is.null(end.yr)){
      endyrvec <- end.yr
    }
  }
  if(subplots == 14 & plot_mcmc){
    for(i in seq_along(models)){
      if(is.na(models[[i]]$extra.mcmc[1])){
        stop("All models must have had the extra-mcmc run to make this plot with plot_mcmc == TRUE\n",
             "Model '", model.names[[i]], "' does not have any extra-mcmc information.\n",
             call. = FALSE)
      }
    }
    # Make a list of data frames of the indices, one data frame for each model
    indices <- compare.summary$indices %>%
      as_tibble() %>%
      filter(Use == 1, Fleet == 2) %>%
      select(Yr, name, Obs, Exp) %>%
      mutate(Obs = log(Obs), Exp = log(Exp)) %>%
      group_split(name)

    # MCMC fit requires extra-mcmc
    indices <- map2(models, indices, ~{
      yrs <- min(.y$Yr):max(.y$Yr)
      em_lower <- .x$extra.mcmc$cpue.0.025
      em_med <- .x$extra.mcmc$cpue.median
      em_upper <- .x$extra.mcmc$cpue.0.975
      if(length(.x$fleet_ID) == 3){
        # Scenario where Age-1 index was included
        em_lower <- em_lower[1:length(yrs)]
        em_med <- em_med[1:length(yrs)]
        em_upper <- em_upper[1:length(yrs)]
      }
      fits <- as_tibble(list(Yr = yrs,
                             lower = log(em_lower),
                             med = log(em_med),
                             upper = log(em_upper)))
      left_join(fits, .y, by = "Yr", copy = TRUE)
    })
    indices <- map_df(seq_along(indices), ~{
      indices[[.x]] <- indices[[.x]] %>% mutate(name = model.names[.x])
    })
    cols <- rich.colors.short(2 * length(models))
    cols <- cols[seq(2, length(cols), 2)]
    fill_cols <- rich.colors.short(2 * length(models), alpha = 0.1)
    fill_cols <- fill_cols[seq(2, length(fill_cols), 2)]
    shapes <- 1:length(models)

    indices <- indices %>%
      rename(`Log Index` = Obs) %>%
      rename(Year = Yr)

    g <- ggplot(indices) +
      aes(x = Year, y = `Log Index`, group = name, color = name, fill = name, shape = name) +
      geom_ribbon(aes(ymin = lower, ymax = upper), color = NA) +
      geom_line(aes(y = med, color = name), size = 1) +
      geom_point(aes(y = med, color = name), size = 3, stroke = 1.5) +
      scale_fill_manual(values = fill_cols) +
      scale_color_manual(values = cols) +
      scale_shape_manual(values = shapes) +
      geom_point(aes(x = Year, y = `Log Index`), size = 2, inherit.aes = FALSE) +
      theme(legend.position = "none")
    return(g)
  }
  SSplotComparisons(compare.summary,
                    subplots = subplots,
                    legend = legend,
                    legendlabels = model.names,
                    legendloc = legendloc,
                    indexPlotEach = indexPlotEach,
                    indexUncertainty = indexUncertainty,
                    indexfleets = indexfleets,
                    endyrvec = endyrvec,
                    densitynames = ifelse(is.null(densitynames), c("SSB_Virgin", "R0"), densitynames),
                    mcmcVec = plot_mcmc,
                    xaxs = ifelse(all(subplots %in% 1:2), "r", "i"),
                    new = FALSE,
                    labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                               "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                               "", "", "Spawning output",
                               "Harvest rate"),
                    verbose = verbose)
}
