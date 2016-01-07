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

make.fishing.intensity.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                        start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                        end.yr,           ## Year the timeseries ends (i.e. last year in model)
                                        color = "blue"
                                        ){
  ## Plots the 1-SPR / 1-SPR40% for the mcmc given by model
  oldpar <- par()

  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  yrs <- start.yr:end.yr

  ## Remove prepended strings from year labels
  names(plower) <- gsub("SPRratio_","",names(plower))
  names(pmed) <- gsub("SPRratio_","",names(pmed))
  names(pupper) <- gsub("SPRratio_","",names(pupper))

  ## Remove any projection years from SPR tables
  plower <- plower[(names(plower) %in% yrs)]
  pmed <- pmed[(names(pmed) %in% yrs)]
  pupper <- pupper[(names(pupper) %in% yrs)]

  y <- data.frame(value=pmed,lo=plower,hi=pupper)
  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
  plotBars.fn(yrs,y,scalar=1,ylim=c(0,1.3),pch=20,
            ## xlab="Year",ylab="Fishing intensity (1-SPR)/(1-SPR_40%)",
            xlab="Year",
            ylab=expression(paste("Fishing intensity",~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
            cex=0.8,las=1,gap=0.02,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),
            mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
  axis(1,at=c(seq(start.yr+4,end.yr-1,5), end.yr-1))
  axis(1,at=start.yr:(end.yr-1), lab=rep("",length(start.yr:(end.yr-1))), tcl=-0.3)
  ##axis(1,at=seq(1965,lastCatchYr,2))
  abline(h=1,col=rgb(0,0,0,0.4))
  text(start.yr+4,1.05,"Management Target",cex=0.8,col=rgb(0,0,0,0.4))
  par <- oldpar
}

make.exploitation.fraction.plot <- function(model,            ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                            start.yr,         ## Year the timeseries starts (i.e. first year in model)
                                            end.yr,           ## Year the timeseries ends (i.e. last year in model)
                                            color = "blue"
                                            ){
  ## Plots the exploitation fraction for the mcmc given by model
  oldpar <- par()

  flower <- model$mcmccalcs$flower
  fmed <- model$mcmccalcs$fmed
  fupper <- model$mcmccalcs$fupper

  yrs <- start.yr:end.yr

  ## Remove prepended strings from year labels
  names(flower) <- gsub("F_","",names(flower))
  names(fmed) <- gsub("F_","",names(fmed))
  names(fupper) <- gsub("F_","",names(fupper))

  ## Remove any projection years from F tables
  flower <- flower[(names(flower) %in% yrs)]
  fmed <- fmed[(names(fmed) %in% yrs)]
  fupper <- fupper[(names(fupper) %in% yrs)]

  y <- data.frame(value=fmed,lo=flower,hi=fupper)

  par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
  plotBars.fn(yrs,y,scalar=1,ylim=c(0,0.4),pch=20,xlab="Year",ylab="Exploitation fraction",
              cex=0.8,las=1,gap=0.005,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
  ## axis(1,at=seq(1965,lastCatchYr,2))
  axis(1,at=c(seq(start.yr+4,end.yr-1,5), end.yr-1))
  axis(1,at=start.yr:(end.yr-1), lab=rep("",length(start.yr:(end.yr-1))), tcl=-0.3)
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

  slower <- model$mcmccalcs$slower
  smed <- model$mcmccalcs$smed
  supper <- model$mcmccalcs$supper

  ## Remove prepended strings from year labels
  names(slower) <- gsub("SPB_","",names(slower))
  names(smed) <- gsub("SPB_","",names(smed))
  names(supper) <- gsub("SPB_","",names(supper))

  sb40 <- smed["Initial"] * 0.4
  sb0 <- smed["Initial"]

  ## Remove Initial value from biomass series
  slower <- slower[-grep("Initial",names(slower))]
  smed <- smed[-grep("Initial",names(smed))]
  supper <- supper[-grep("Initial",names(supper))]

  plower <- model$mcmccalcs$plower
  pmed <- model$mcmccalcs$pmed
  pupper <- model$mcmccalcs$pupper

  ## Remove prepended strings from year labels
  names(plower) <- gsub("SPRratio_","",names(plower))
  names(pmed) <- gsub("SPRratio_","",names(pmed))
  names(pupper) <- gsub("SPRratio_","",names(pupper))

  ## Remove any projection years from SPR tables
  plower <- plower[(names(plower) %in% yrs)]
  pmed <- pmed[(names(pmed) %in% yrs)]
  pupper <- pupper[(names(pupper) %in% yrs)]

  sb <- smed[yrs %in% c(start.yr:(end.yr-1))]/sb0
  sb.hi <- supper[yrs %in% (end.yr-1)]/sb0
  sb.lo <- slower[yrs %in% (end.yr-1)]/sb0

  spr <- pmed[yrs %in% c(start.yr:(end.yr-1))]
  spr.hi <- pupper[yrs %in% (end.yr-1)]
  spr.lo <- plower[yrs %in% (end.yr-1)]

  par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0))
  plot(sb,spr,type="n",pch=20,xlim=c(0,1.3),ylim=c(0,1.3),
       #xlab="Spawning depletion (SB/SB0)",
       xlab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
       #ylab="Relative fishing intensity (1-SPR)/(1-SPR_40%)",
       ylab=expression(paste("Relative fishing intensity",~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
       xaxs="i",yaxs="i",mgp=c(2.4,1,0))
  colvec <- rev(rich.colors.short(n=length(sb))[-1])
  arrows(sb[-length(sb)],spr[-length(spr)],sb[-1],spr[-1],length=0.09,
         #col=rgb(0,0,0,0.4))
         col=colvec)
  points(sb,spr,type="p",pch=20)
  points(sb[length(sb)],spr[length(spr)],pch=16,col=1,cex=1.2)
  points(sb[1],spr[1],pch=16,col=1,cex=1.2)
  text(sb[1],spr[1]-0.025,"1966",cex=0.6,pos=2,offset=0.15)
  segments(sb[length(sb)],spr.lo,sb[length(sb)],spr.hi,col=rgb(0,0,0,0.5))
  segments(sb.lo,spr[length(spr)],sb.hi,spr[length(spr)],col=rgb(0,0,0,0.5))
  text(sb[length(sb)],spr[length(spr)]+0.045,end.yr-1,pos=4,cex=0.6)
  abline(h=1,v=1,lty=2,col=rgb(0,0,0,0.4))
  abline(h=1,v=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.4))

  par <- oldpar
}
