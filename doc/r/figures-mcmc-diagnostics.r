make.mcmc.diag.plot <- function(model,      ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                subplot = 1 ## which parameter to plot. See below for info
                                ){
  ## Plot the diagnostics for the model.
  ## Assumes the model has an mcmc
  ## subplot values:
  ## 1 = natural mortality
  ## 2 = initial recruitment (log(R0))
  ## 3 = steepness
  ## 4 = extra SD in survey

  if(subplot == 1){
    colnames <- c("NatM_p_1_Fem_GP_1")
    label <- "M (natural mortality)"
  }else if(subplot == 2){
    colnames <- c("SR_LN.R0.")
    label <- expression(paste(log(R[0])~"(initial recruitment)"))
  }else if(subplot == 3){
   colnames <- c("SR_BH_steep")
   label <- "h (steepness)"
  }else if(subplot == 4){
    colnames <- c("Q_extraSD_2_Acoustic_Survey")
    label <- "Extra SD in survey"
  }
  oldpar <- par()
  par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
  mcmc.out(model$mcmcpath,
           run = "",
           numparams = 1,
           closeall = FALSE,
           new = FALSE,
           colNames = colnames)
  mtext(label, side = 2, outer = TRUE, line = 1.3, cex = 1.1)
  par <- oldpar
}

make.mcmc.diag.hists.plot <- function(model ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                      ){
  ## Plot the diagnostic test histograms for the model.
  ## Autocorrelation, Effective sample size, Geweke statistic, and
  ## Heidelberger and Walsh statistic
  ## Assumes the model has an mcmc

  oldpar <- par()
  par(mar=c(5,4,0,0.5),oma=c(0,0,0.5,0.5))
  mcmc.stats <- mcmc.nuisance(model$mcmcpath,
                              run = "",
                              labelstrings = c(model$parameters$Label,
                                               "SPB_",
                                               "Bratio_"),
                              bothfiles = TRUE)
  par <- oldpar
}

make.mcmc.diag.pairs.plot <- function(model,                 ## model is an mcmc run and is the output of the
                                                             ##  r4ss package's function SSgetMCMC
                                      inc.key.params = TRUE, ## If true, include the key parameters in the plot
                                      recr = NULL,           ## vector of recruitment years to plot params for
                                                             ##  (or rec. devs. if inc.key.params is FALSE)
                                      bratio = NULL,         ## vector of Bratio years to plot params for
                                      forecatch = NULL       ## vector of forecatch years to plot params for
                                      ){
  ## Plot the correlation between parameters via the pairs plot for the mcmc model.
  ## Assumes the model has an mcmc
  ## If inc.key.params is NULL, only the others will be added
  ## If recr is NULL, no recruitment parameters will be included
  ## If forecatch is NULL, no ForeCatch parameters will be included

  oldpar <- par()
  m <- model$mcmc
  if(inc.key.params){
    df <- data.frame(m$Object,
                     m$NatM,
                     m$SR_LN,
                     m$SR_BH_steep,
                     m$Q)
    labels <- c("Objective\nfunction",
                "Natural\nmortality\n(M)",
                "Equilibrium\nrecruitment\nlog(R0)",
                "Steepness\n(h)",
                "Extra SD\nin survey")
  }else{
    if(is.null(recr)){
      stop("make.mcmc.diag.pairs.plot: Error - if inc.key.params is FALSE, recr must not be null.\n\n")
    }
    labs <- model$recruitpars$Label[model$recruitpars$Yr %in% recr]
    df <- m[c(grep("R0",names(m)),
              which(names(m) %in% labs))]
    labels <- c("Equilibrium\nrecruitment\nlog(R0)",
                paste("Recruit\ndev.",recr))
  }
  if(!is.null(recr) & inc.key.params){
    str <- paste0("Recr_", recr)
    df <- cbind(df, m[str])
    labels <- c(labels, paste0("Recruitment\n",recr))
  }
  if(!is.null(bratio) & inc.key.params){
    str <- paste0("Bratio_", bratio)
    df <- cbind(df, m[str])
    labels <- c(labels, paste0("Relative\nspawning\nbiomass\n",bratio))
  }
  if(!is.null(forecatch) & inc.key.params){
    str <- paste0("ForeCatch_", forecatch)
    df <- cbind(df, m[str])
    labels <- c(labels, paste0("Default\nharvest\nin ", forecatch))
  }
  pairs(df,
        labels = labels,
        pch = ".",
        cex.labels = 1.2,
        xaxt = "n",
        yaxt = "n",
        las = 1,
        gap = 0.5,
        oma = c(0,0,0,0))
    par <- oldpar
}
