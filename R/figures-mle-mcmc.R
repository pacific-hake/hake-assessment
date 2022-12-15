make.mcmc.vs.mle.plot <- function(model,        ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                  end_yr,
                                  subplot = 2,  ## Same as subplots argument for SScomparisons
                                  type = "o",   ## Same as the type argument for SScomparisons
                                  spacepoints = 3000 ## Same as the spacepoints argument for SScomparisons
                                      ){
  ## Plot the MCMC verses the MLE for given model and subplot

  oldpar <- par()

  mod.list <- list(model, model)
  mod.summ <- SSsummarize(mod.list, SpawnOutputUnits="biomass")
  mod.summ$mcmc <- list(model$mcmc, model$mcmc)
  model.names <- c("MLE","MCMC")

  if(subplot == 10){
    type <- "p"
    spacepoints <- 1
  }

  par(mfrow = c(1,1), las = 1, mar = c(3.6, 3.6, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2.5, 1, 0))
  SSplotComparisons(mod.summ,
                    legendlabels = model.names,
                    endyr = end_yr,
                    new = FALSE,
                    minbthresh = 0,
                    subplots = subplot,
                    legend = TRUE,
                    col = c("red", "black"),
                    spacepoints = spacepoints,
                    shadealpha = 0.1,
                    btarg = -0.4,
                    type = type,
                    xlim = c(model$startyr - 3, end_yr + 3),
                    mcmcVec = c(FALSE, TRUE),
                    legendloc = "topleft")
  if(subplot %in% c(3,4)) {axis(2, at = c(0.1, 0.4), cex.axis = 0.8)}
  par <- oldpar
}
