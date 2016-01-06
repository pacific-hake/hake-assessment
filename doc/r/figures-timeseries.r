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

  yrs <- equil.yr:end.yr
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
  plot(yrs[-c(1,2)],c(smed[2:length(smed)]),type="l",lwd=3,ylim=c(0,max(supper)+0.1),
       xlab="Year",ylab="Female Spawning Biomass (million t)",
       xlim=range(yrs),cex.axis=0.9,cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
  axis(1,at=end.yr, cex.axis=0.9)
  axis(1,at=yrs[1], labels="Unfished\nequilibrium", cex.axis=0.9, mgp=c(3,1.5,0))
  points(yrs[1],smed[1],pch=16)
  arrows(yrs[1],slower[1],yrs[1],supper[1],angle=90,code=3,length=0.06, col=color)
  addpoly(yrs[-c(1,2)],slower[-1],supper[-1],color)
  par <- oldpar
}

