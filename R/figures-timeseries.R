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
