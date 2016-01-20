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
