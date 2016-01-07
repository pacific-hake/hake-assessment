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
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
  plot(yrs,dmed,type="l",lwd=3,ylim=c(0,1.1*max(dupper)),xlab="Year",
       ylab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
       xlim=range(yrs),cex.axis=0.9,
       cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
  addpoly(yrs,dlower,dupper,color)
  abline(h=c(0.1,0.4,1),lty=2,col=gray(0.5))
  axis(2,at=c(0.1,0.4),cex.axis=0.8)
  axis(1,at=end.yr,cex.axis=0.9)
  mtext("Year",side=1,cex=1.1,outer=T,line=1.4)
  par <- oldpar
}

make.recruitment.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                  equil.yr,         ## Year in which unfished equilibium is assumed
                                  start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                  end.yr,           ## Year the timeseries ends (i.e. last year in model)
                                  color = "blue",
                                  add.mean = FALSE,
                                  add.r0   = FALSE  ## show the posterior distribution for initial recruitment as an envelope
                                  ){
  ## Plots the recruitment for the mcmc given by model
  ## If add.mean = TRUE, the means will be shown and the unfished equilibrium recruitment will not be shown
  ## If add.mean = FALSE, the unfished equilibrium recruitment will be shown
  oldpar <- par()

  rlower <- model$mcmccalcs$rlower
  rmed <- model$mcmccalcs$rmed
  rupper <- model$mcmccalcs$rupper
  rmean <- model$mcmccalcs$rmean

  yrs <- equil.yr:end.yr
  y <- data.frame(value=rmed[-1],lo=rlower[-1],hi=rupper[-1])
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
  plotBars.fn(yrs[-c(1,2)],y,scalar=1,ylim=c(0,35),pch=20,xlab="Year",ylab="Age 0 recruits (billions)",
              cex=0.8,las=1,gap=0,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs))
  if(add.mean){
    points(yrs[-c(1,2)],rmean[-1],pch=4,cex=0.8)
  }else{
    plotBars.fn(yrs[1],data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,
                cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=TRUE)
    legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
  }
  axis(1,at=seq(equil.yr+1,end.yr,5))
  abline(h=0,col=rgb(0,0,0,0.5))

  if(add.r0){
    abline(h=rmed[1],lty=2,col=rgb(0,0,0,0.5))
    polygon(c(0,0,max(yrs+10),max(yrs)+10),c(rlower[1],rupper[1],rupper[1],rlower[1]),col=rgb(0,0,0,0.1),lty=3)
    axis(2,at=rmed[1],label=expression(italic(R)[0]),cex.axis=0.7,mgp=c(1,0.3,0),tcl=-0.2)
  }
  par <- oldpar
}
