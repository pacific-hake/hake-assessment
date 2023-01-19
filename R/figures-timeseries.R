make.biomass.plot <- function(model,    ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                              equil.yr, ## Year in which unfished equilibium is assumed
                              start_yr, ## Year the timeseries starts (i.e. first year in model)
                              end_yr,   ## Year the timeseries ends (i.e. last year in model)
                              color = "blue"
                              ){
  oldpar <- par()

  slower <- model$mcmccalcs$slower
  smed <- model$mcmccalcs$smed
  supper <- model$mcmccalcs$supper

  unfished.eq.s <- model$mcmccalcs$sinit

  yrs <- equil.yr:end_yr
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))

  start.equil.diff <- start_yr - equil.yr
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

  axis(1, at = big_ticks)
  axis(1,
       at = small_ticks,
       lab = rep("",length(small_ticks)), tcl = -0.3)
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
                                start_yr, ## Year the timeseries starts (i.e. first year in model)
                                end_yr,   ## Year the timeseries ends (i.e. last year in model)
                                color = "blue"
                                ){
  oldpar <- par()

  dlower <- model$mcmccalcs$dlower
  dmed <- model$mcmccalcs$dmed
  dupper <- model$mcmccalcs$dupper

  yrs <- start_yr:end_yr
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
  axis(1, at = big_ticks)
  axis(1,
       at = small_ticks,
       lab = rep("",length(small_ticks)), tcl = -0.3)
  axis(2,at = c(0.1,0.4), cex.axis = 0.8)
  addpoly(yrs, dlower, dupper, color)
  abline(h = c(0.1,0.4,1), lty = 2,col = gray(0.5))
  mtext("Year", side = 1,cex = 1.1, outer = TRUE, line = 1.4)
  par <- oldpar
}

make.recruitment.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                  equil.yr,         ## Year in which unfished equilibium is assumed
                                  start_yr,         ## Year the timeseries starts (i.e. first year in model)
                                  end_yr,           ## Year the timeseries ends (i.e. last year in model)
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

  #non.equil.yrs <- start_yr:(end_yr - 1) # former setting to exclude final year
  non.equil.yrs <- start_yr:end_yr
  yrs <- equil.yr:end_yr
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
              ylab = "Age-0 recruits (billions)",
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
  axis(1, at = big_ticks)
  axis(1,
       at = small_ticks,
       lab = rep("",length(small_ticks)), tcl = -0.3)
  ##axis(1, at = seq(equil.yr + 1, end_yr, 5))
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
                                      end_yr  ## Year the timeseries ends (i.e. last year in model). Need this so forecast years aren't shown
                                      ){
  ## Plots the recruitment deviations for the mcmc given by model
  oldpar <- par()

  dev.lower <- model$mcmccalcs$devlower
  dev.med <- model$mcmccalcs$devmed
  dev.upper <- model$mcmccalcs$devupper
  dev.yrs <- as.numeric(names(dev.med))
  dev.yrs <- dev.yrs[dev.yrs <= end_yr]
  y <- data.frame(value = dev.med, lo = dev.lower, hi = dev.upper)
  y <- y[as.numeric(rownames(y)) <= end_yr,]

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
              xlim = c(min(dev.yrs), end_yr))
  ##axis(1, at = seq(min(dev.yrs) - 1, end_yr, 5))
  axis(1, at = seq(min(dev.yrs) - 1, end_yr+4, 5))
  axis(1,
       at = seq(min(dev.yrs) - 1, end_yr+4, 1),
       lab = rep("",length(seq(min(dev.yrs) - 1, end_yr+4, 1))), tcl = -0.3)
  abline(h = 0, col = rgb(0, 0, 0, 0.5))
  abline(h = seq(-4, 4, 2), col = rgb(0, 0, 0, 0.5), lty = "13", lwd = 0.5)

  par <- oldpar
}

make.fishing.intensity.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                        start_yr,         ## Year the timeseries starts (i.e. first year in model)
                                        end_yr,           ## Year the timeseries ends (i.e. last year that values are required for; note that SS calculates a fishing intensity for the final year of the model, but the number is meaningless).
                                        color = "blue",
                                        upper.lim = 1.4){
  ## Plots the 1-SPR / 1-SPR40% for the mcmc given by model
  oldpar <- par()

  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  yrs <- start_yr:end_yr

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
  axis(1, at = big_ticks)
  axis(1, at = small_ticks, lab = rep("",length(small_ticks)), tcl = -0.3)
  abline(h = 1, col = rgb(0,0,0,0.4))
  text(start_yr+4, 1.05, "Management Target", cex = 0.8, col = rgb(0,0,0,0.4))
  par <- oldpar
}

make.exploitation.fraction.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                            start_yr,         ## Year the timeseries starts (i.e. first year in model)
                                            end_yr,           ## Year the timeseries ends (i.e. last year that values are required for; note that SS calculates an exploitation rate for the final year of the model, but the number is meaningless because that year has no catch).
                                            color = "blue",
                                            upper.lim = 0.4){
  ## Plots the exploitation fraction for the mcmc given by model
  oldpar <- par()

  flower <- model$mcmccalcs$flower
  fmed <- model$mcmccalcs$fmed
  fupper <- model$mcmccalcs$fupper

  yrs <- start_yr:end_yr

  ## Only include start year to end year
  flower <- flower[(names(flower) %in% yrs)]
  fmed <- fmed[(names(fmed) %in% yrs)]
  fupper <- fupper[(names(fupper) %in% yrs)]

  y <- data.frame(value=fmed,lo=flower,hi=fupper)

  par(mfrow = c(1,1), las = 1, mar = c(3.5,4.5,1,1), oma = c(0,1,0,0))
  plotBars.fn(yrs, y, ylim = c(0,upper.lim), pch = 20, xlab = "Year", ylab = "",
              cex = 0.8, las = 1, gap = 0.005, xaxt = "n", ciLwd = 1, ciCol = rgb(0,0,1,0.5),
              mgp = c(2.3,1,0), xlim = range(yrs), yaxs="i")
  axis(1, at = big_ticks)
  axis(1, at = small_ticks, lab = rep("",length(small_ticks)), tcl = -0.3)
  mtext("Exploitation fraction", side=2, line=3, las=0)
  par <- oldpar
}

make.phase.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                            start_yr,         ## Year the timeseries starts (i.e. first year in model)
                            end_yr,           ## Year the timeseries ends (i.e. last year in model)
                            cex.lab = 1,
                            x.max = 1.5,      ## maximum of x-axis (1.3 for base
                                              ##  model in 2019 and 2020)
                            y.max = 1.2       ## maximum of y-axis (1.3 for 2019, 2020)
                            ){
  ## Plots the relative fishing intensity in year t-1 against relative spawning
  ## biomass in year t, as a historical look at the fishery for the mcmc given by model
  oldpar <- par()

  yrs <- start_yr:end_yr

  dlower <- model$mcmccalcs$dlower
  dmed   <- model$mcmccalcs$dmed
  dupper <- model$mcmccalcs$dupper

  plower <- model$mcmccalcs$plower
  pmed   <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  ## Only include start year to end year (results contain extra calculations),
  ##  but don't need dmed[start_yr], since no pmed[start_yr-1] to plot it with,
  ##  or pmed[end_yr] since that's meaningless here (yet SS calculates it)
  ## as we don't know catch in end_yr.
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
       col = colvec[1])
  text(dmed[length(dmed)],
       pmed[length(pmed)] - 0.015,
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

#' Make the retrospective recruitment deviations plot (AKA 'squid plot')
#'
#' @param model Model with retrospectives
#' @param subplot 1 = Recruitment deviations, 2 = Rec dev strength relative to most recent estimate
#' @param cohorts Vector of cohort years to plot (for labels)
#' @param plot_mcmc If `TRUE` plot the MCMC version based on medians. If `FALSE` plot MLE version.
#' @param getdevs A logical value specifying if the plot should be of
#' recruitment deviations, which is the default. If `FALSE`, then
#' the squid plot will be made using absolute recruitment instead of
#' deviations.
#'
#' @export
make_squid_plot <- function(model,
                            subplot = 1,
                            cohorts,
                            plot_mcmc = TRUE,
                            getdevs = TRUE){

  oldpar <- par()
  on.exit(par(oldpar))
  mdl_list <- list(model)
  retro_list <- map(model$retros, ~{.x})
  models <- c(mdl_list, retro_list)
  compare_summary <- SSsummarize(models, SpawnOutputUnits = "biomass", verbose = FALSE)
  neg_yr_vec <- seq(0, -(length(models) - 1), -1)
  endyrvec <- compare_summary$endyrs + 1 + neg_yr_vec
  plot_retro_recruits(compare_summary,
                      endyrvec = endyrvec,
                      cohorts = cohorts,
                      ylim = NULL,
                      uncertainty = FALSE,
                      main = "",
                      mcmcVec = plot_mcmc,
                      devs = getdevs,
                      relative = ifelse(subplot == 1, FALSE, TRUE),
                      labelyears = TRUE,
                      legend = FALSE,
                      leg.ncols = 4)
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
#' @param indexfleets See [r4ss::SSplotComparisons()] or if `subplot` == 13 and this == 2,
#' plot age 2+ acoustic survey. If `subplot` == 13 and this == 3, plot age-1 acoustic survey
#' @param is.retro If `TRUE`, it is a retrospective plot
#' @param legend See [r4ss::SSplotComparisons()]
#' @param legendloc See [r4ss::SSplotComparisons()]
#' @param legend_pos For `subplots` 13 and 14 only, legend position in the [ggplot2::ggplot()]
#' @param end_yr End year of the plot. If is.retro is TRUE, this is ignored
#' and the `endyrvec` argument for [r4ss::SSplotComparisons()] is calculated
#' @param plot_mcmc If `TRUE`, plot MCMC values. If `FALSE`, plot MLE values
#' @param verbose Be verbose on output
#' @param fleet If `subplot` == 13 and this == 2, plot age 2+ acoustic survey.
#' If `subplot` == 13 and this == 3, plot age-1 acoustic survey
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
                                 legend_loc = "topright",
                                 legend_pos = c(0.8, 0.9),
                                 legend_order = seq(1, length(models)),
                                 legend_ncol = 1,
                                 end_yr = NULL,
                                 plot_mcmc = TRUE,
                                 verbose = FALSE,
                                 ...){

  if(is.null(model.names)){
    tmp.names <- sapply(models[1:length(models)], "[[", "path")
    model.names <- gsub(".*/", "", tmp.names)
  }

  compare.summary <- SSsummarize(models, SpawnOutputUnits = "biomass", verbose = verbose)
  endyrvec <- "default"
  # If it is a retrospective plot, compute the end year vector of years so the lines end on the correct years
  if(is.retro){
    endyrvec <- compare.summary$endyrs + 1 + (0:-(length(models) - 1))
  }else{
    if(!is.null(end_yr)){
      endyrvec <- end_yr
    }
  }
  if(subplots %in% c(13, 14) & plot_mcmc){
    for(i in seq_along(models)){
      if(is.na(models[[i]]$extra.mcmc[1])){
        stop("All models must have had the extra-mcmc run to make this plot with plot_mcmc == TRUE\n",
             "Model '", model.names[[i]], "' does not have any extra-mcmc information.\n",
             call. = FALSE)
      }
    }
    if(is.null(indexfleets)){
      stop("You chose subplot 13 or 14, so you need to provide an indexfleet to plot")
    }
    # Make a list of data frames of the indices, one data frame for each model
    indices <- map2(models, model.names, ~{.x$dat$CPUE %>%
        mutate(med = .x$extra.mcmc$cpue.median,
               lower = .x$extra.mcmc$cpue.0.025,
               upper = .x$extra.mcmc$cpue.0.975) %>%
        filter(index > 0) %>%
        rename(fleet = index) %>%
        select(-seas) %>%
        mutate(name = .y)}) %>%
      map_df(~{.x})

    legendfun <- function(legend_labels, pch, type = "b", col, lty, lwd) {

      if(is.numeric(legend_loc)){
        usr <- par()$usr
        legendloc <- list(x = usr[1] + legendloc[1] * (usr[2] - usr[1]),
                          y = usr[3] + legendloc[2] * (usr[4] - usr[3]))
      }
      # if type input is "l" then turn off points on top of lines in legend
      legend_pch <- pch
      if(type == "l"){
        legend_pch <- rep(NA, length(pch))
      }
      legend(legend_loc,
             legend = legend_labels[legend_order],
             col = col[legend_order],
             lty = lty[legend_order],
             seg.len = 2,
             lwd = lwd[legend_order],
             pch = legend_pch[legend_order],
             bty = "n",
             ncol = legend_ncol)
    }

    # Plot using base R
    indices <- indices %>%
      filter(fleet %in% indexfleets) %>%
      mutate_at(.vars = vars(lower, upper, med, obs), ~{.x / 1e6})

    xlim <- range(min(indices$year), max(indices$year))
    ylim <- range(0, indices$upper)
    plot(0,
         type = "n",
         yaxs = "i",
         ylim = ylim,
         xlim = xlim,
         xlab = "Year",
         ylab = ifelse(indexfleets == 3, "Relative age-1 index (billions of fish)", "Biomass (million t)"),
         axes = FALSE)
    index_names <- unique(indices$name)
    first_index <- indices %>% filter(name == index_names[1])
    # Get the plot mask so colors match
    is_plotted <- NULL
    models_plotted <- unique(indices$name)
    for(nm in seq_along(model.names)){
      is_plotted[nm] <- ifelse(any(`==`(model.names[nm], models_plotted)), TRUE, FALSE)
    }
    if(length(models) == 2){
      cols <- rich.colors.short(length(models))[is_plotted]
    }else{
      cols <- rich.colors.short(length(models) + 1)[-1][is_plotted]
    }

    for(i in 1:length(models)){
      rows <- indices %>% filter(name == index_names[i])
      x <- rows$year
      y <- rows$med
      lower <- rows$lower
      upper <- rows$upper
      lines(x + 0.1 * (i - 1), y, pch = 19, lwd = 2, lty = 1, col = cols[i], type = "b")
      arrows(x0 = x + 0.1 * (i - 1), y0 = lower, x1 = x + 0.1 * (i - 1), y1 = upper, lwd = 2, lty = 1,
             col = cols[i], length = 0.025, angle = 90, code = 3)
    }
    points(first_index$year, first_index$obs, pch = 19, cex = 1.5)
    box()
    axis(1, pretty(xlim))
    axis(2, pretty(ylim))
    if(legend){
      legend_labels <- index_names
      legendfun(legend_labels,
                type = "b",
                pch = rep(19, length(index_names)),
                col = cols,
                lty = rep(1, length(index_names)),
                lwd = rep(1, length(index_names)))
    }
    return(invisible(NULL))
  }
  SSplotComparisons(compare.summary,
                    subplots = subplots,
                    legend = legend,
                    legendlabels = model.names,
                    legendloc = legend_loc,
                    indexPlotEach = indexPlotEach,
                    indexUncertainty = indexUncertainty,
                    indexfleets = indexfleets,
                    endyrvec = endyrvec,
                    densitynames = ifelse(is.null(densitynames), c("SSB_Virgin", "R0"), densitynames),
                    densityxlab = densityxlab,
                    mcmcVec = plot_mcmc,
                    xaxs = ifelse(all(subplots %in% 1:2), "r", "i"),
                    new = FALSE,
                    labels = c("Year", "Spawning biomass (t)", "Relative spawning biomass", "Age-0 recruits (1,000s)",
                               "Recruitment deviations", "Index", "Log index", "SPR ratio", "Density",
                               "", "", "Spawning output",
                               "Harvest rate"),
                    verbose = verbose,
                    ...)
}
