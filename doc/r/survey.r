make.survey.biomass.plot <- function(models,      ## models is the list returned by load.models.
                                     modelnum = 1 ## the index of the model to show. It shouldn't
                                                  ## really matter on the same assessment though.
                                     ){
  ## It is assumed that the data file has been read in correctly.
  ## Assumes that there is only one 'CPUE' index and it is the acoustic survey.
  ## There is no error checking to warn you if there is more than one index.

  oldpar <- par()
  base <- models[[modelnum]]
  tmp <- base$dat
  numObs <- tmp$N_cpue
  survey.dat <- tmp$CPUE
  ## Remove non-survey years from the data frame
  survey.dat <- survey.dat[survey.dat$index > 0,]

  ## ind <- grep("Acoustic survey",tmp)[1]+1
  ## ind <- ind:(ind+numObs-1)
  ## ests <- strsplit(gsub("\t"," ",tmp[ind])," +")
  ## ests <- t(as.data.frame(lapply(ests,function(x){as.numeric(x[1:5])})))
  ## dimnames(ests) <- list(NULL,c("year","seas","index","obs","SElog"))
  ## ests <- as.data.frame(ests)
  ## tmpSE <- ests[ests$year==2009,"SElog"]
  ests <- survey.dat
  ## Hard-coded 2009 for Hake survey squid year
  tmpSE <- ests[ests$year==2009, "se_log"]
  ests[ests$year==2009, "se_log"] <- 0.0682 ## se without squid inflation
  ests$lo <- exp(log(ests$obs)-1.96*ests$se_log)
  ests$hi <- exp(log(ests$obs)+1.96*ests$se_log)
  ests$value <- ests$obs

  ests2 <- ests
  ests2$se_log <- NA
  ests2[ests2$year==2009,"se_log"] <- tmpSE ## se without squid inflation
  ests2$lo <- exp(log(ests2$obs)-1.96*ests2$se_log)
  ests2$hi <- exp(log(ests2$obs)+1.96*ests2$se_log)
  ests2$value <- ests2$obs
  par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
  plotBars.fn(ests2$year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million t)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
  plotBars.fn(ests$year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
  axis(1,at=ests$year,cex.axis=0.8)

  ## Acoustic survey fits
  ## par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
  ## plot(0, type='n', xlim=c(1994,2014), xaxs='i', ylim=c(0,5.5e6), yaxs='i', axes=FALSE,
  ##      xlab="year",ylab="Biomass index (million t)")
  ## cpue <- tmp$CPUE[tmp$CPUE$index > 0,]
  ## segments(x0 = cpue$year,
  ##          y0=qlnorm(.025,meanlog=log(cpue$ob),sdlog=cpue$se_log),
  ##          y1=qlnorm(.975,meanlog=log(cpue$ob),sdlog=cpue$se_log),
  ##          lwd=3, lend=1)
  ## SSplotIndices(base, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
  ## ##plotBars.fn(ests2$year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="year",ylab="Biomass Index Estimate (million mt)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
  ## ##plotBars.fn(ests$year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
  ## axis(1, at=base$cpue$Yr[base$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
  ## axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.3)
  ## box()
  ## axis(2, at=(0:5)*1e6, lab=0:5, las=1)
  par <- oldpar
}
