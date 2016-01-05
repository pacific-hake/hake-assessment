### code for figures in 2015 hake stock assessment
### modified from Figures.R by Allan Hicks created for 2014 hake assessment

stop("\n  This file should not be sourced!")

# paths on Ian's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724920","NWCLW01724829","ian-THINK") ){
  hakedir <- "C:/SS/hake/Hake_2015/"
  rDir    <- "c:/GitHub/hakeAssess/hake2015/Rcode/"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  SSdir   <- file.path(hakedir, "Models")
  doMaps  <- FALSE
}

#source("C:/NOAA2014/Hake/WriteUp/Rcode/WriteUpFunctions.R")
source(file.path(rDir, "WriteUpFunctions.R"))

## devtools::install_github("nwfsc-assess/nwfscSurvey")
## devtools::install_github("r4ss/r4ss")
library(nwfscSurvey)
library(date)
library(r4ss)

# load more packages and get mapping data only if map will be updated
if(doMaps){
  data(westCoastLL)
  data(WCstatesInlandPBS)
  devtools::install_github("nwfsc-assess/nwfscMapping")
  library(nwfscMapping)
  library(PBSmapping)

  source("C:/Mapping/WestCoastMapping.R")
  LMEoffshore <- importShapefile("C:/Mapping/Shapefiles/LME66_Offshore/LME66_Offshore.shp",readDBF=T)
  LME <- importShapefile("C:/Mapping/Shapefiles/LME66/LME66.shp",readDBF=T)
  province <- importShapefile("C:/Mapping/Shapefiles/province/province.shp",readDBF=T)
  alberta <- attributes(province)$PolyData[attributes(province)$PolyData$NAME == "Alberta","PID"]
  CCLME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "California Current","PID"]
  GOALME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "Gulf of Alaska","PID"]
  data(nepacLL)
}

# important stuff to load for any plotting
base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"))
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=FALSE)
if(nrow(mcmc$model1)!=999){
  stop("MCMC is not 999 rows! Make sure you did this on purpose.")
}
base$mcmc <- data.frame(mcmc$model1)
# data file for use in a few places
dat <- SS_readdat(file.path(base$inputs$dir, "2015Hake_data.ss"))
endYr <- 2015
lastCatchYr <- endYr-1



spb2015 <- base$mcmc[,grep("SPB",names(base$mcmc))]/2e6
# add more years to list each time (or generalize in the future)
spb2015 <- spb2015[,!names(spb2015) %in% c("SPB_Virgin",
                                           paste0("SPB_",endYr+1:20))]
slower <- apply(spb2015,2,quantile,prob=0.025)   #hard-wired probability
smed   <- apply(spb2015,2,quantile,prob=0.5)   #hard-wired probability
supper <- apply(spb2015,2,quantile,prob=0.975)   #hard-wired probability

depl2015 <- t(apply(spb2015,1,function(x){x/x[1]}))[,-1]
dlower <- apply(depl2015,2,quantile,prob=0.025)   #hard-wired probability
dmed   <- apply(depl2015,2,quantile,prob=0.5)   #hard-wired probability
dupper <- apply(depl2015,2,quantile,prob=0.975)   #hard-wired probability

recr2015 <- base$mcmc[,grep("Recr_",names(base$mcmc))]/1e6
recr2015 <- recr2015[,-grep("Fore",names(recr2015))]
yrs <- unlist(lapply(strsplit(names(recr2015),"_"),function(x){x[2]}))
recr2015 <- recr2015[,yrs%in%c("Virgin",1966:2015)]
yrs <- as.numeric(unlist(lapply(strsplit(names(recr2015),"_"),function(x){x[2]})))
rmed   <- apply(recr2015,2,quantile,prob=0.5)   #hard-wired probability
rmean   <- apply(recr2015,2,mean)   #hard-wired probability
rlower <- apply(recr2015,2,quantile,prob=0.025)   #hard-wired probability
rupper <- apply(recr2015,2,quantile,prob=0.975)   #hard-wired probability

dev2015 <- base$mcmc[,grep("Early_InitAge_20",names(base$mcmc)):
                     grep(paste0("ForeRecr_",endYr+2),names(base$mcmc))]
devlower <- apply(dev2015,2,quantile,prob=0.025)
devmed <- apply(dev2015,2,quantile,prob=0.5)
devupper <- apply(dev2015,2,quantile,prob=0.975)

# load stuff for maps (if requested)
if(doMaps){
  ##########################################################################
  ## Map of area
  portLats <- read.csv("Data/Rcode/portLats.csv")
  theCities <- c("Newport","Westport","Astoria","Eureka","Charleston (Coos Bay)")
  theCities <- portLats[portLats$Name%in%theCities,]
  doPNG <- T
  doTIFF <- F
  ht <- 10; wd<- 6.5
  if(doPNG) {png(filename = paste(figDir,"overviewMap.png",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
  if(doTIFF) {tiff(filename = paste(figDir,"overviewMap.tiff",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
  if(!doPNG & !doTIFF) {windows(height=ht,width=wd)}
  par(mfrow=c(1,1),mar=c(4,4,0,0)+0.1,las=1)
  plotMap(westCoastLL, tck = c(-0.02), xlim=c(-140,-113.0), ylim=c(29.9,59.1),col=gray(0.8))
  addLines(WCstatesInlandPBS)
  map('state',add=T,region=c("Idaho","Montana","Nevada","Arizona"))
  addLines(province,polyProps=data.frame(PID=alberta))
  addLines(province,polyProps=data.frame(PID=12,SID=c(123)))
  addLines(LMEoffshore,polyProps=data.frame(PID=76)) #foudn this by x <- addLines(LMEoffshore) then looking at x
  addLines(LMEoffshore,polyProps=data.frame(PID=169))
  points(-theCities$Lon,theCities$Lat,pch=16,col=gray(0.4))
  text(-theCities$Lon,theCities$Lat,theCities$Name,pos=4,cex=0.7,col=gray(0.4))

  text(-123,56,"BC",cex=1.2)
  text(-120,47.1,"WA",cex=1.1)
  text(-120,44.3,"OR",cex=1.2)
  text(-118.8,35.9,"CA",cex=1.2)
  text(-122.8,51,"Strait of\nGeorgia",adj=0,cex=0.7)
  arrows(-122.8,51,-123.55,49.67,length=0.05)
  text(-120.7,48.4,"Puget\nSound",adj=0,cex=0.7)
  arrows(-120.7,48.4,-122.1,47.7,length=0.05)
  text(-133.2,52,"Haida\nGwaii",adj=1,cex=0.7)
  arrows(-133.2,52,-132.2,52.5,length=0.05)
  text(-117.8,30.5,"Baja\nCalifornia",adj=1,cex=0.7)
  arrows(-117.8,30.5,-115.5,30.5,length=0.05)
  text(-131.2,58,"SE\nAlaska",adj=0,cex=0.7)
  arrows(-131.2,58,-132.3,57.5,length=0.05)
  text(-128.3,49.9,"Vancouver\nIsland",adj=1,cex=0.7)
  arrows(-128.3,49.9,-127.4,49.8,length=0.05)
  text(-129.5,51.6,"Queen\nCharlotte\nSound",adj=0.5,cex=0.6)
  text(-133.5,54.5,"Dixon Entrance",adj=1,cex=0.7)
  arrows(-133.5,54.45,-132.5,54.45,length=0.05)
  text(-126.2,48.5,"Strait of\nJuan de Fuca",adj=1,cex=0.7)
  arrows(-126.2,48.5,-124.7,48.5,length=0.05)

  text(-128,39,"California Current LME",cex=1.4,srt=285)
  text(-138,56.8,"Gulf Of Alaska\nLME",cex=0.9,srt=300)
  if(doPNG | doTIFF) {dev.off()}

}



#################################
# Acoustic survey estimates
# Acoustic survey
# Biomass estimate
#
# NOTE: not updated for 2015 document because there was no new survey
#
tmp <- readLines("Models/2014hake_21_TVselex1991start/2014Hake_data.ss")
ind <- grep("Number of index observations",tmp)
numObs <- as.numeric(strsplit(tmp[ind],"#")[[1]][1])
ind <- grep("Acoustic survey",tmp)[1]+1
ind <- ind:(ind+numObs-1)
ests <- strsplit(gsub("\t"," ",tmp[ind])," +")
ests <- t(as.data.frame(lapply(ests,function(x){as.numeric(x[1:5])})))
dimnames(ests) <- list(NULL,c("Year","seas","index","obs","SElog"))
ests <- as.data.frame(ests)
tmpSE <- ests[ests$Year==2009,"SElog"]
ests[ests$Year==2009,"SElog"] <- 0.0682 #se without squid inflation
ests$lo <- exp(log(ests$obs)-1.96*ests$SElog)
ests$hi <- exp(log(ests$obs)+1.96*ests$SElog)
ests$value <- ests$obs

ests2 <- ests
ests2$SElog <- NA
ests2[ests2$Year==2009,"SElog"] <- tmpSE #se without squid inflation
ests2$lo <- exp(log(ests2$obs)-1.96*ests2$SElog)
ests2$hi <- exp(log(ests2$obs)+1.96*ests2$SElog)
ests2$value <- ests2$obs

doPNG <- T
ht<-5;wd=8
if(doPNG) {png("WriteUp/Figures/acousticBio.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plotBars.fn(ests2$Year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million t)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
plotBars.fn(ests$Year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
axis(1,at=ests$Year,cex.axis=0.8)
if(doPNG) {dev.off()}

#################################
## Acoustic survey fits

doPNG <- T
ht<-4;wd=6.5
if(doPNG) {png(file.path(figDir,"acousticBioFit.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plot(0, type='n', xlim=c(1994,2014), xaxs='i', ylim=c(0,5.5e6), yaxs='i', axes=FALSE,
     xlab="Year",ylab="Biomass index (million t)")
cpue <- dat$CPUE[dat$CPUE$index > 0,]
segments(x0 = cpue$year,
         y0=qlnorm(.025,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         y1=qlnorm(.975,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         lwd=3, lend=1)
SSplotIndices(base, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
#plotBars.fn(ests2$Year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million mt)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
#plotBars.fn(ests$Year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
axis(1, at=base$cpue$Yr[base$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.3)
box()
axis(2, at=(0:5)*1e6, lab=0:5, las=1)
if(doPNG) {dev.off()}


#################################
## Acoustic survey fits with MCMC
## (requires lots of additional calculations to create cpue.table

doPNG <- T
ht<-4;wd=6.5
if(doPNG) {png(file.path(figDir,"acousticBioFit_withMCMC.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plot(0, type='n', xlim=c(1994,2014), xaxs='i', ylim=c(0,5.5e6), yaxs='i', axes=FALSE,
     xlab="Year",ylab="Biomass index (million t)")
cpue <- dat$CPUE[dat$CPUE$index > 0,]
segments(x0 = cpue$year,
         y0=qlnorm(.025,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         y1=qlnorm(.975,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         lwd=3, lend=1)
matplot(x=1994:2014, y=cpue.table, col=rgb(0, 0, 1, .03), type='l', add=TRUE, lty=1)
lines(  x=1994:2014, y=apply(cpue.table,1,median), col=rgb(0, 0, .5, .7), lty=1, lwd=3)
legend('topleft',
       legend=c("Observed survey biomass (with MLE estimates of 95% intervals)",
           "MLE estimates of expected survey biomass",
           "Median MCMC estimate of expected survey biomass",
           "MCMC samples of estimates of expected survey biomass"),
       lwd=c(NA,3,3,1),
       pch=c(21,NA,NA,NA),
       bg='white',
       text.col=gray(.6),
       col=c(1, rgb(1,0,0,.7), rgb(0, 0, .5, .7), rgb(0, 0, 1, .4)),
       bty='n')
SSplotIndices(base, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
#plotBars.fn(ests2$Year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million mt)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
#plotBars.fn(ests$Year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
axis(1, at=base$cpue$Yr[base$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.3)
box()
axis(2, at=(0:5)*1e6, lab=0:5, las=1)
if(doPNG) {dev.off()}



###############################################################################################################

doPNG <- TRUE

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "biomass.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
yrs <- 1964:endYr
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
plot(yrs[-c(1,2)],c(smed[2:length(smed)]),type="l",lwd=3,ylim=c(0,max(supper)+0.1),
     xlab="Year",ylab="Female Spawning Biomass (million t)",
     xlim=range(yrs),cex.axis=0.9,cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
axis(1,at=endYr, cex.axis=0.9)
axis(1,at=yrs[1], labels="Unfished\nequilibrium", cex.axis=0.9, mgp=c(3,1.5,0))
points(yrs[1],smed[1],pch=16)
arrows(yrs[1],slower[1],yrs[1],supper[1],angle=90,code=3,length=0.06)
addpoly(yrs[-c(1,2)],slower[-1],supper[-1],"black")
if(doPNG) {dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "biomassColor.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
yrs <- 1964:endYr
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
plot(yrs[-c(1,2)],c(smed[2:length(smed)]),type="l",lwd=3,ylim=c(0,max(supper)+0.1),xlab="Year",ylab="Female Spawning Biomass (million t)",xlim=range(yrs),cex.axis=0.9,cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
points(yrs[1],smed[1],pch=16)
arrows(yrs[1],slower[1],yrs[1],supper[1],angle=90,code=3,length=0.06,col=rgb(0,0,1,0.5))
axis(1,at=endYr, cex.axis=0.9)
axis(1,at=yrs[1], labels="Unfished\nequilibrium", cex.axis=0.9, mgp=c(3,1.5,0))
addpoly(yrs[-c(1,2)],slower[-1],supper[-1],"blue")
if(doPNG) {dev.off()}



ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "relativeSpawningBio.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
yrs <- 1966:endYr
plot(yrs,dmed,type="l",lwd=3,ylim=c(0,1.1*max(dupper)),xlab="Year",
     ylab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
     xlim=range(yrs),cex.axis=0.9,
     cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
addpoly(yrs,dlower,dupper,"black")
abline(h=c(0.1,0.4,1),lty=2,col=gray(0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
axis(1,at=endYr,cex.axis=0.9)
mtext("Year",side=1,cex=1.1,outer=T,line=1.4)
if(doPNG) {dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"relativeSpawningBioColor.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
yrs <- 1966:endYr
plot(yrs,dmed,type="l",lwd=3,ylim=c(0,1.1*max(dupper)),xlab="Year",
     ylab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
     xlim=range(yrs),cex.axis=0.9,
     cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
addpoly(yrs,dlower,dupper,"blue")
abline(h=c(0.1,0.4,1),lty=2,col=gray(0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
axis(1,at=endYr,cex.axis=0.9)
mtext("Year",side=1,cex=1.1,outer=T,line=1.4)
if(doPNG) {dev.off()}


#Recruitment
y <- data.frame(value=rmed[-1],lo=rlower[-1],hi=rupper[-1])
#y <- data.frame(value=c(rmed[1],NA,rmed[-1]),lo=c(rlower[1],NA,rlower[-1]),hi=c(rupper[1],NA,rupper[-1]))

ht <- 3.25; wd<- 6.5
yrs <- 1964:endYr
if(doPNG) {png(file.path(figDir,"recruitment.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs[-c(1,2)],y,scalar=1,ylim=c(0,35),pch=20,xlab="Year",ylab="Age 0 recruits (billions)",cex=0.8,las=1,gap=0,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs))
plotBars.fn(yrs[1],data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=T)
legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
axis(1,at=seq(1965,2015,5))
abline(h=0,col=rgb(0,0,0,0.5))
if(doPNG){dev.off()}

#Recruitment with mean added
ht <- 3.25; wd<- 6.5
yrs <- 1964:endYr
if(doPNG) {png(file.path(figDir,"recruitmentWithMean.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs[-c(1,2)],y,scalar=1,ylim=c(0,35),pch=20,
            xlab="Year",ylab="Age 0 recruits (billions)",cex=0.8,las=1,gap=0,
            xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),
            xlim=range(yrs[-c(1,2)]))
points(yrs[-c(1,2)],rmean[-1],pch=4,cex=0.8)
#R0
abline(h=rmed[1],lty=2,col=rgb(0,0,0,0.5))
polygon(c(0,0,max(yrs+10),max(yrs)+10),c(rlower[1],rupper[1],rupper[1],rlower[1]),col=rgb(0,0,0,0.1),lty=3)
#plotBars.fn(yrs[1]-3,data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=T)
#legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
axis(1,at=seq(1965,2015,5))
axis(2,at=rmed[1],label=expression(italic(R)[0]),cex.axis=0.7,mgp=c(1,0.3,0),tcl=-0.2)
abline(h=0,col=rgb(0,0,0,0.5))
#arrows(2010,30.9,2010,31,length=0.05,col=rgb(0,0,1,0.5))
if(doPNG){dev.off()}

#Recruit deviations
y <- data.frame(value=devmed,lo=devlower,hi=devupper)
#y <- data.frame(value=c(rmed[1],NA,rmed[-1]),lo=c(rlower[1],NA,rlower[-1]),hi=c(rupper[1],NA,rupper[-1]))

ht <- 3.25; wd<- 6.5

devyrs <- 1946:2017
show <- devyrs<=2015
if(doPNG) {png(file.path(figDir,"recruitment_deviations.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(devyrs[show],y[show,],scalar=1,ylim=c(-4.5,4.5),pch=20,
            xlab="Year",ylab="Log-scale recruitment deviations ",cex=0.8,las=1,gap=0,
            xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),
            xlim=range(devyrs[show]))
#plotBars.fn(yrs[1],data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=T)
#legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
axis(1,at=seq(1945,2015,5))
abline(h=0,col=rgb(0,0,0,0.5))
abline(h=seq(-4,4,2), col=rgb(0,0,0,0.5), lty='13', lwd=0.5)
if(doPNG){dev.off()}

#Fishing Intensity
spr2015 <- base$mcmc[,grep("SPRratio_",names(base$mcmc))]
yrs <- unlist(lapply(strsplit(names(spr2015),"_"),function(x){x[2]}))
spr2015 <- spr2015[,yrs%in%c(1966:lastCatchYr)]
yrs <- as.numeric(unlist(lapply(strsplit(names(spr2015),"_"),function(x){x[2]})))
pmed   <- apply(spr2015,2,quantile,prob=0.5)   #hard-wired probability
plower <- apply(spr2015,2,quantile,prob=0.025)   #hard-wired probability
pupper <- apply(spr2015,2,quantile,prob=0.975)   #hard-wired probability

y <- data.frame(value=pmed,lo=plower,hi=pupper)

ht <- 3.25; wd<- 6.5
yrs <- 1966:lastCatchYr
if(doPNG) {png(file.path(figDir,"SPRratio.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs,y,scalar=1,ylim=c(0,1.3),pch=20,
            #xlab="Year",ylab="Fishing intensity (1-SPR)/(1-SPR_40%)",
            xlab="Year",
            ylab=expression(paste("Fishing intensity",~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
            cex=0.8,las=1,gap=0.02,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),
            mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
axis(1,at=c(seq(1970,endYr-1,5), endYr-1))
axis(1,at=1966:lastCatchYr, lab=rep("",length(1966:lastCatchYr)), tcl=-0.3)
#axis(1,at=seq(1965,lastCatchYr,2))
abline(h=1,col=rgb(0,0,0,0.4))
text(1969.9,1.05,"Management Target",cex=0.8,col=rgb(0,0,0,0.4))
if(doPNG){dev.off()}


#Expoitation rate
f2014 <- base$mcmc[,grep("F_",names(base$mcmc))]
yrs <- unlist(lapply(strsplit(names(f2014),"_"),function(x){x[2]}))
f2014 <- f2014[,yrs%in%c(1966:lastCatchYr)]
yrs <- as.numeric(unlist(lapply(strsplit(names(f2014),"_"),function(x){x[2]})))
fmed   <- apply(f2014,2,quantile,prob=0.5)   #hard-wired probability
flower <- apply(f2014,2,quantile,prob=0.025)   #hard-wired probability
fupper <- apply(f2014,2,quantile,prob=0.975)   #hard-wired probability

y <- data.frame(value=fmed,lo=flower,hi=fupper)

ht <- 3.25; wd<- 6.5
yrs <- 1966:lastCatchYr
if(doPNG) {png(file.path(figDir,"ExpRate.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs,y,scalar=1,ylim=c(0,0.4),pch=20,xlab="Year",ylab="Exploitation fraction",cex=0.8,las=1,gap=0.005,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
#axis(1,at=seq(1965,lastCatchYr,2))
axis(1,at=c(seq(1970,endYr-1,5), endYr-1))
axis(1,at=1966:lastCatchYr, lab=rep("",length(1966:lastCatchYr)), tcl=-0.3)
if(doPNG){dev.off()}


#phase plot (needs values from phase plots above)
yrs <- unlist(lapply(strsplit(names(smed),"_"),function(x){x[2]}))
sb40 <- smed["SPB_Initial"]*0.4
sb0 <- smed["SPB_Initial"]
sb <- smed[yrs%in%c(1966:lastCatchYr)]/sb0
sb.hi <- supper[yrs%in% lastCatchYr]/sb0
sb.lo <- slower[yrs%in% lastCatchYr]/sb0
yrs <- unlist(lapply(strsplit(names(pmed),"_"),function(x){x[2]}))
spr <- pmed[yrs%in%c(1966:lastCatchYr)]
spr.hi <- pupper[yrs%in% lastCatchYr]
spr.lo <- plower[yrs%in% lastCatchYr]


phase.fn <- function(){
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
  text(sb[length(sb)],spr[length(spr)]+0.045,lastCatchYr,pos=4,cex=0.6)
  #abline(h=1,v=1,lty=2,col=rgb(0,0,0,0.4))
  abline(h=1,v=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.4))
}
ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"phasePlot_tall.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
phase.fn()
if(doPNG){dev.off()}
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"phasePlot_short.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
phase.fn()
if(doPNG){dev.off()}




#############################################################
#############################################################
#############################################################
#
#Data plot
doPNG <- T
#hake2014.06 <- SS_output(dir=paste(SSdir,"2014hake_06_final2013Data",sep="/"),covar=F,verbose=F)
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"data.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0))
SSplotData(base,fleetnames=c("Fishery","Survey"))
if(doPNG){dev.off()}


############### Ian says: Allan will need to do this one
#Percent Seasonal Catch
doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"percSeasCatch.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
catchdat <- read.csv("Data/Catches/percentSeasonalCatch.csv")
plot(catchdat$Year,100*catchdat$Spring,type="b",pch=20,col="green2",ylim=c(0,70),las=1,xlab="Year",ylab="Percent of total catch",xaxt="n")
lines(catchdat$Year,100*catchdat$Summer,type="b",pch=17,col="blue")
lines(catchdat$Year,100*catchdat$Fall,type="b",pch=15,col="red")
legend("topright",c("Apr-Jun","Jul-Sep","Oct-Dec"),pch=c(20,17,15),col=c("green2","blue","red"),lty=1)
axis(1,at=seq(1991,2013,2),cex.axis=0.9)
if(doPNG){dev.off()}





##################################################################
## Combined age comps

## # code from 2014, modified for 2015
## tmp <- readLines(file.path(base$inputs$dir, "2015Hake_data.ss"))
## ind <- grep("Aggregate marginal fishery age comps",tmp)+2
## numObs <- 40   #as.numeric(strsplit(tmp[ind],"=")[[1]][2])
## ind <- ind:(ind+numObs-1)
## ages <- strsplit(gsub("\t"," ",tmp[ind])," +")
## ages <- t(as.data.frame(lapply(ages,function(x){as.numeric(x)})))
## dimnames(ages) <- list(NULL,c("Year","seas","fleet","gender","partition","ageError","L1","L2","nTrips",paste("a",1:15,sep="")))
## commComps <- as.data.frame(ages)
##
## ind <- grep("Number of index observations",tmp)
## numObs <- 10
## ind <- grep("Acoustic survey",tmp)[2]+1
## ind <- ind:(ind+numObs-1)
## ages <- strsplit(gsub("\t"," ",tmp[ind])," +")
## ages <- t(as.data.frame(lapply(ages,function(x){as.numeric(x)})))
## dimnames(ages) <- list(NULL,c("Year","seas","fleet","gender","partition","ageError","L1","L2","nTrips",paste("a",1:15,sep="")))
## acComps <- as.data.frame(ages)

# alternative approach in 2015
dat <- SS_readdat(file.path(base$inputs$dir, "2015Hake_data.ss"))
commComps <- dat$agecomp[dat$agecomp$FltSvy==1,]
acComps <- dat$agecomp[dat$agecomp$FltSvy==2,]

maxProp <- 0.77  #make sure that this is bigger than any actual proportion
max(commComps[,-(1:9)])
#[1] 0.701
max(acComps[,-(1:9)])
#[1] 0.762

doPNG <- T
ht<-8;wd=8
xlim <- c(1975,2014)
inches <- 0.12
fg <- gray(level=0.1, alpha=0.5)
bg <- gray(level=0.5, alpha=0.5)
if(doPNG) {png(file.path(figDir, "bothAgeComps.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(2,1),las=1,mar=c(0, 4, 0, 4) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=inches,ylim=c(1,15),xlim=xlim,xlab="",ylab="Acoustic Ages",xaxt="n",
        fg=fg, bg=bg)
axis(4)
symbols(.2+c(1990,1994,1998,2002,-1),c(16.2,16.2,16.2,16.2,-1),circles=sqrt(c(0.01,0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA,
        fg=fg, bg=bg)
text(c(1990,1994,1998,2002)+1.1,c(16.2,16.2,16.2,16.2),c("0.01","0.1","0.2","0.4"),xpd=NA,cex=0.8)
x <- data.frame(expand.grid(commComps$Yr,1:15),prop=unlist(commComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=inches,ylim=c(1,15),xlim=xlim,xlab="Year",ylab="Commercial Ages",xaxt="n",
        fg=fg, bg=bg)
axis(4)
axis(1,at=seq(1975,2020,5))
if(doPNG) dev.off()


doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "AcAgeComps_superImpose.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic"),pch=16,col=c("blue"),bty="n")
abline(h=15.5)
if(doPNG) dev.off()

doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "bothAgeComps_superImpose.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic","Fishery"),pch=16,col=c("blue","red"),bty="n")
abline(h=15.5)
x <- data.frame(expand.grid(tmp$Yr,1:15),prop=unlist(tmp[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,15),xlim=range(x$Yr),xlab="Year",ylab="Commercial Ages",xaxt="n",fg=rgb(1,0,0,0.5),lwd=2,bg=rgb(1,0,0,0.5),add=T)
if(doPNG) dev.off()


doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "bothAgeComps_superImpose_shift.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic","Fishery"),pch=16,col=c("blue","red"),bty="n")
abline(h=15.5)
x <- data.frame(expand.grid(tmp$Yr,1:15),prop=unlist(tmp[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(0.2+x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,15),xlim=range(x$Yr),xlab="Year",ylab="Commercial Ages",xaxt="n",fg=rgb(1,0,0,0.5),lwd=2,bg=rgb(1,0,0,0.5),add=T)
if(doPNG) dev.off()


##################################################################
## Fit to age comps
doPNG <- T
ht <- 6; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsFishery.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=1,fleets=1,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=7,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}
ht <- 2.7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsSurvey.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=1,fleets=2,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=2,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}

##################################################################
## Fit to age comps with MCMC values
## (requires lots of additional calculations to modify base.mcmc$agedbase
doPNG <- T
ht <- 6; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsFishery_MCMC.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base.mcmc,kind="AGE",subplot=1,fleets=1,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=7,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}
ht <- 2.7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsSurvey_MCMC.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base.mcmc,kind="AGE",subplot=1,fleets=2,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=2,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}

##################################################################
## Alternative view of age comp with fit
source(file.path(rDir, "agecomp_rainbows.R"))
ht <- 6; wd<- 8
if(doPNG) {png(file.path(figDir,"ageCompFitsFisery_rainbows.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
AgeFits(dat=base, ncol=4)
dev.off()

##################################################################
## Alternative view of age comp with fit MCMC
## (requires lots of additional calculations to modify base.mcmc$agedbase
source(file.path(rDir, "agecomp_rainbows.R"))
ht <- 6.5; wd<- 7.5
if(doPNG) {png(file.path(figDir,"ageCompFitsFisery_rainbows_MCMC.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
AgeFits(dat=base.mcmc, ncol=4, f=1, uncertainty=TRUE, title.text="Fishery age composition")
dev.off()

ht <- 6.5; wd<- 2.2
if(doPNG) {png(file.path(figDir,"ageCompFitsSurvey_rainbows_MCMC.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
AgeFits(dat=base.mcmc, ncol=1, f=2, uncertainty=TRUE, title.text="Survey age composition", legend=FALSE,
        start.color=1)
dev.off()



##################################################################
## Pearson residuals
doPNG <- T
ht <- 7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompPearsons.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=24,printmkt=FALSE,printsex=FALSE,fleetnames=c("Fishery","Survey"))
if(doPNG){dev.off()}

# data only
doPNG <- T
ht <- 7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompDataOnly.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=24,datonly=TRUE,
            printmkt=FALSE,printsex=FALSE,fleetnames=c("Fishery","Survey"),
            cexZ1=3)
if(doPNG){dev.off()}


####################################################################################

#################################################
#Acoustic Age-1 index
#
# Ian: SKIPPING THIS IN 2015
#
doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"age1Index.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3, 4, 1, 1) + 0.1)
x <- read.csv("Data/AcousticSurvey/Age1Index.csv")
plot(x$Year,x$Age1_Index/1e9,pch=16,col="blue",cex=1.5,log="y",ylim=range(c(x$Age1_Index),na.rm=T)*c(0.65,1.5)/1e9,lwd=2,xaxt="n",xlab="Year",ylab="Acoustic Age-1 Index",las=1,yaxt="n")
axis(2,at=c(0.01,0.05,0.10,0.25,1.00,2.00,5.00,10.00),las=1)
axis(1,at=x$Year[!is.na(x$Age1_Index)])
legend("bottomleft",c("Acoustic survey age-1 index"),col=c("blue"),pch=c(16),lty=NA,lwd=2,bty="n")
if(doPNG) dev.off()


doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"age1IndexWithRecr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3, 4, 1, 1) + 0.1)
x <- read.csv("Data/AcousticSurvey/Age1Index.csv")
logAge1 <- log(x$Age.1[!is.na(x$Age1_Index)])
logIndex <- log(x$Age1_Index)
mn <- mean(logAge1)
index <- mn*logIndex/mean(logIndex[!is.na(x$Age1_Index)])
plot(x$Year,x$Age.1/1e6,pch=4,type="b",log="y",ylim=range(c(x$Age.1,exp(index)),na.rm=T)*c(1,1)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1,col=gray(0.7),cex=0.8)
points(x$Year,exp(index)/1e6,pch=16,col="blue",cex=1.5)
points(x$Year[!is.na(x$Age1_Index)],x$Age.1[!is.na(x$Age1_Index)]/1e6,pch=4,col="black",cex=1,lwd=2)
#mn <- mean(x$Age.1[!is.na(x$Age1_Index)])
#index <- exp(mn*x$Age1_Index/mean(x$Age1_Index),na.rm=T)
#plot(x$Year,x$Age.1/1e6,pch=4,type="b",log="y",ylim=range(c(x$Age.1,index),na.rm=T)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1)
#points(x$Year,index/1e6,pch=16,col="blue",cex=1.5)
axis(1,at=x$Year[!is.na(x$Age1_Index)])
legend("bottomleft",c("Estimated age-1 recruitment","Scaled acoustic survey age-1 index"),col=c("black","blue"),pch=c(4,16),lty=NA,lwd=2,bty="n")
if(doPNG) dev.off()









#########
# MCMC diagnostics
## source("WriteUp/Rcode/mcmc.out.R")
## source("WriteUp/Rcode/mcmc.nuisance.R")

### new for 2015 exploring Arni's plotMCMC package
require(plotMCMC)
require(coda)
mcmcPars <- mcmc(read.table(file.path(SSdir,"2015hake_basePreSRG_mcmc12e6",
                                      "posteriors.sso"),header=TRUE))


## doPNG <- T
require(r4ss)
require(coda)
hakeMCMC <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=TRUE,
            keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                      nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
# trying again with all parameters in the nuisance strings list
hakeMCMC <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=TRUE,
            keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                      nuisancestrings = c(base$parameters$Label, "Objective_function", "SPB_"))
ht <- 4; wd<- 4.5

if(doPNG) {png(file.path(figDir,"mcmcM.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("NatM_p_1_Fem_GP_1"))
mtext("M (natural mortality)",side=2,outer=T,line=1.3,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png(file.path(figDir,"mcmcR0.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("SR_LN.R0."))
mtext(expression(paste(log(R[0])~"(initial recruitment)")),side=2,outer=T,line=1.0,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png(file.path(figDir,"/mcmcSteep.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("SR_BH_steep"))
mtext("h (steepness)",side=2,outer=T,line=1.3,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png(file.path(figDir,"mcmcQextra.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("Q_extraSD_2_Acoustic_Survey"))
mtext("Extra SD in survey",side=2,outer=T,line=1.3,cex=1.1)
if(doPNG){dev.off()}


hakeMCMC <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),
                      writecsv=TRUE,
                      keystrings = c("NatM","R0", "steep", "Q_extraSD"),
                      nuisancestrings = c("SPB_","Bratio_","Recr_"))
#mcmc.out(paste(SSdir,"2013hake_19_mcmc/",sep="/"),run="",numparams=4)

ht <- 4.0; wd<- 4.5
if(doPNG) {png(file.path(figDir,"mcmcDiagnostics_expanded.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,4,0,0.5),oma=c(0,0,0.5,0.5))
mcmc.stats <- mcmc.nuisance(file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),run="",
                            labelstrings=c(base$parameters$Label,
                                "SPB_","Bratio_"),bothfiles=T)
if(doPNG){dev.off()}
# which parameters (if any) failed test (NULL = none in 2015)
mcmc.stats$Label[mcmc.stats$heidelwelsch=="Failed"]

#scatterplot of key params and derived quants
ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"mcmcPairs.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
pairs(data.frame(base$mcmc$Object,base$mcmc$NatM,base$mcmc$SR_LN,base$mcmc$SR_BH_steep,
                 base$mcmc$Q,base$mcmc$Recr_2008,base$mcmc$Recr_2010,
                 base$mcmc$Bratio_2015,base$mcmc$ForeCatch_2015),
    labels=c("Objective\nfunction","Natural\nmortality\n(M)",
        "Equilibrium\nrecruitment\nlog(R0)",
        "Steepness\n(h)","Extra SD\nin survey","Recruitment\n2008","Recruitment\n2010",
        "Relative\nspawning\nbiomass\n2015","Default\nharvest\nin 2015"),
      pch=".",cex.labels=1.2,xaxt="n",yaxt="n",las=1,gap=0.5,oma=c(0,0,0,0))
if(doPNG){dev.off()}

ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"mcmcPairsRecruits.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
pairs(base$mcmc[c(paste0("Recr_",2005:2014))],
    ## labels=c("Objective\nfunction","Natural\nmortality\n(M)",
    ##     "Equilibrium\nrecruitment\nlog(R0)",
    ##     "Steepness\n(h)","Extra SD\nin survey","Recruitment\n2008","Recruitment\n2010",
    ##     "Relative\nspawning\nbiomass\n2015","Default\nharvest\nin 2015"),
      pch=".",cex.labels=1.2,xaxt="n",yaxt="n",las=1,gap=0.5,oma=c(0,0,0,0))
if(doPNG){dev.off()}

ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"mcmcPairsRecDevs.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
# figure out which recruit deviation labels (either "Main_RecrDev" or "Late_RecrDev")
# correspond to a range of years
labs <- base$recruitpars$Label[base$recruitpars$Yr %in% 2005:2013]
# make pairs plot
pairs(base$mcmc[c(grep("R0",names(base$mcmc)),
                  which(names(base$mcmc) %in% labs))],
      labels=c("Equilibrium\nrecruitment\nlog(R0)",
          paste("Recruit\ndev.",2005:2013)),
      pch=".",cex.labels=1.2,xaxt="n",yaxt="n",las=1,gap=0.5,oma=c(0,0,0,0))
if(doPNG){dev.off()}

# individual plot of virgin spawning biomass vs. spawning bio in 2015 
ht <- 3.5; wd<- 3.5
if(doPNG) {png(file.path(figDir,"spawn_bio_pair_virg_vs_2015.png"),height=ht,width=wd,
               pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
x <- 1e-6*base$mcmc$SPB_Virgin/2
y <- 1e-6*base$mcmc$SPB_2015/2
par(mar=c(4,4,.3,.3))
plot(x, y,
     pch=16, col=gray(0,.2), las=1,
     xlim=c(0, 1.05*max(x,y)), xaxs='i',
     ylim=c(0, 1.05*max(x,y)), yaxs='i',
     xlab="Equilibrium spawning biomass (million t)",
     ylab="Spawning biomass in 2015 (million t)")
abline(0, 1, lty=3)
dev.off()

# SD of recruit devs
labs <- base$recruitpars$Label[base$recruitpars$Yr %in% 1971:2011]
recDevMeds <- apply(base$mcmc[names(base$mcmc) %in% labs], 2, median)
sd(recDevMeds)
# [1] 1.508223
labs <- base$recruitpars$Label[base$recruitpars$Yr %in% 1946:2014]
sd(as.matrix(base$mcmc[names(base$mcmc) %in% labs]))
# [1] 1.509564

#################################################
# Selex
#################################################
# Fishery selectivity
yrs <- 1991:2014
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(base$mcmc),ncol=16)
for(i in 1:nrow(base$mcmc)) {
    ind <- grep("AgeSel_1P_[1-9]_Fishery",names(base$mcmc))[1:5]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,base$mcmc[i,ind],0,0,0,0,0,0,0,0,0)))
}

for(i in yrs) {
    selex[[as.character(i)]] <- selexYear.fn(base$mcmc,i)
}

selexMed2015 <- selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp2015 <- selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow2015 <- selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

doPNG <- T
ht <- 8; wd<-4.5
if(doPNG) {png(file.path(figDir,"TVselexAll.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
ind <- 1:9
tmp <- t(selexMed[ind,])
par(mar=c(4,4,1,1))
mountains(tmp,xvec=ind-1,yvec=as.numeric(substring(row.names(tmp),2)),rev=T,cex.axis=0.8)
mtext(side=1,line=2,"Age")
mtext(side=2,line=3,"Selectivity by year")
if(doPNG) {dev.off()}



### function for showing uncertainty across years in time-varying selectivity
selPoly <- function(year,ages=1:8,yAdjust){
## selexUpp
## selexLow
## selexMed
  column <- which(names(selexMed)==paste0("X",year))
  lines((0:15)[ages+1], yAdjust + selexMed[ages+1,column], type="b", pch=20)
  segments(x0=(0:15)[ages+1], y0=yAdjust + selexUpp[ages+1,column],
           x1=(0:15)[ages+1], y1=yAdjust + selexLow[ages+1,column])
  polygon(x=c((0:15)[ages+1], rev((0:15)[ages+1])),
          y=yAdjust + c(selexUpp[ages+1,column],rev(selexLow[ages+1,column])),
          col=rgb(0,0,1,0.2),lty=3)
}

### use function above to plot uncertainty across years in time-varying selectivity
doPNG <- T
ht <- 10; wd<-8
if(doPNG) {png(file.path(figDir,"TVselexAllUncertainty.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
# range of years for each of 2 panels
yrs1 <- 1990:2001
yrs2 <- 2002:2014
# maximum number (in case they are unequal in length)
nYrsMax <- max(length(yrs1),length(yrs2))
par(mfrow=c(1,2))
# left-hand set of panels
plot(0, type="n", xlim=c(1,8), ylim=-1*(max(yrs1) - c(0, nYrsMax)),
     yaxt="n", pch=20, xlab="", ylab="")
lab1 <- yrs1
axis(2, las=1, at=-yrs1+0.5, lab=lab1)
for(y in yrs1){
  print(y)
  selPoly(year=y, yAdjust=-y)
}
abline(h=-c(min(yrs1)-1, yrs1),col=rgb(0,0,0,0.2))
# right-hand set of panels
plot(0, type="n", xlim=c(1,8), ylim=-1*(max(yrs2) - c(0, nYrsMax)),
     yaxt="n", pch=20, xlab="", ylab="")
axis(2, las=1, at=-yrs2+0.5, lab=yrs2)
for(y in yrs2){
  print(y)
  selPoly(year=y, yAdjust=-y)
}
abline(h=-c(min(yrs2)-1, yrs2),col=rgb(0,0,0,0.2))
mtext("Age",outer=T,side=1,line=-2)
mtext("Selectivity by year",outer=T,side=2,line=-1)
if(doPNG) {dev.off()}


doPNG <- T
ht <- 6; wd<-10
if(doPNG) {png(file.path(figDir,"TVselexLinesUncertainty.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),mar=c(4,4,1,1))
cols <- rev(rich.colors.short(6))
polyCols <- rev(rich.colors.short(6,alpha=0.1))
plot(1990:2015,c(selexMed2015[2,],NA),col=cols[1],ylim=c(0,1),type="l",lwd=3,xlab="Year",ylab="",xaxt="n",las=1)
for(i in 1:6) {
    polygon(c(1990:2014,rev(1990:2014)),c(selexLow2015[i+1,],rev(selexUpp2015[i+1,])),col=polyCols[i],lty=3,border=cols[i])
}
for(i in 1:6) {
    lines(1990:2014,c(selexMed2015[i+1,]),col=cols[i],lwd=3)
}
axis(1,at=seq(1990,2014,3))
mtext("Proportion Selected",side=2,line=3)
text(rep(2014,6),selexMed2015[2:7,ncol(selexMed2015)],paste("Age",1:6),pos=4)
if(doPNG) {dev.off()}


#####################################################################
##MLE fit to index

# fit to index covered in "Acoustic survey fits" above

## mymodels <- list(base)
## mysummary <- SSsummarize(mymodels)
## modelnames <- c("Base")
## ht <- 3.5; wd<- 6.5
## if(doPNG) {png(file.path(figDir,"fitIndex.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
## if(!doPNG) {windows(width=wd,height=ht)}
## par(mfrow=c(1,1),las=1,mar=c(3.6,4,1,1),oma=c(0,0,0,0),mgp=c(2.7,1,0),cex.axis=0.7)
## SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2012,new=F,minbthresh=0,subplots=11,indexUncertainty=T,legend=F,col=c("red"),shadecol=rgb(0,0,0,alpha=0.1),btarg=-0.4)
## if(doPNG){dev.off()}


# Acoustic selectivity
doPNG <- TRUE
selex <- base$mcmc[,grep("Selex_std_2_Fem_A_",names(base$mcmc))]
selexMed <- apply(selex,2,median)
selexUpp <- apply(selex,2,quantile,prob=0.975)
selexLow <- apply(selex,2,quantile,prob=0.025)
doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"acousticSelex.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:9),selexMed[1:9],type="b",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex)) {
    lines((1:9),selex[i,1:9],col=rgb(0,0,0,0.1))
}
segments((1:9),selexUpp[1:9],(1:9),selexLow[1:9],col=rgb(0.1,0.1,1,0.8),lwd=3)
points((1:9),selexMed[1:9],pch=16,cex=1.5,col=rgb(0.1,0.1,1,0.7))
axis(1,at=1:9)
if(doPNG) {dev.off()}

tmp <- t(apply(selex,1,function(x){diff(as.numeric(x))}))
xx <- apply(tmp<0,1,any)
cat("MCMC samples with some dome in acoustic selectivity (out of ",nrow(tmp),"): ",sum(xx),"\n", sep="")
## MCMC samples with some dome in acoustic selectivity (out of 999): 939
xxx <- apply(tmp<0,2,sum)
data.frame(label=paste0("decline from age",1:14,"-",2:15), count=xxx)
##                    label count
## 1    decline from age1-2     0
## 2    decline from age2-3    22
## 3    decline from age3-4   519
## 4    decline from age4-5   692
## 5    decline from age5-6    12
## 6    decline from age6-7     0
## 7    decline from age7-8     0
## 8    decline from age8-9     0
## 9   decline from age9-10     0
## 10 decline from age10-11     0
## 11 decline from age11-12     0
## 12 decline from age12-13     0
## 13 decline from age13-14     0
## 14 decline from age14-15     0

# 2014 Commercial selectivity
yrs <- 2014
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(base$mcmc),ncol=16)
for(i in 1:nrow(base$mcmc)) {
    ind <- grep("AgeSel_1P_[1-9]_Fishery",names(base$mcmc))[1:5]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,base$mcmc[i,ind],0,0,0,0,0,0,0,0,0)))
}
for(i in yrs) {
    selex[[as.character(i)]] <- selexYear.fn(base$mcmc,i)
}
selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"commSelex1990.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:9),selexMed[2:10,1],type="n",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex[["1990"]])) {
    lines((1:9),selex[["1990"]][i,2:10],col=rgb(0,0,0,0.1))
}
segments((1:9),selexUpp[2:10,"X1990"],(1:9),selexLow[2:10,"X1990"],col=rgb(1,0.1,0.1,0.8),lwd=3)
points((1:9),selexMed[2:10,"X1990"],pch=16,cex=1.2,col=rgb(1,0.1,0.1,0.7), type="b")
axis(1,at=1:9)
if(doPNG) {dev.off()}

doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"commSelex2014.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:9),selexMed[2:10,1],type="n",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex[["2014"]])) {
    lines((1:9),selex[["2014"]][i,2:10],col=rgb(0,0,0,0.1))
}
segments((1:9),selexUpp[2:10,"X2014"],(1:9),selexLow[2:10,"X2014"],col=rgb(1,0.1,0.1,0.8),lwd=3)
points((1:9),selexMed[2:10,"X2014"],pch=16,cex=1.2,col=rgb(1,0.1,0.1,0.7))
axis(1,at=1:9)
if(doPNG) {dev.off()}





#####################################
##
ht <- 5; wd<- 5
if(doPNG) {png(file.path(figDir,"plotPars.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(2,2),mar=c(3,3,1,1))
## SSplotPars(paste(SSdir,c("2015hake_basePreSRG_mcmc12e6"),sep="/"),
##            strings=c("SR_BH","SR_LN","NatM","Q_extraSD"),
##            newheaders=c("Steepness","LN(R0)","Natural mortality","Survey extra SD"),
##            nrows=2,ncols=2,new=F)
SSplotPars(paste(SSdir,c("2015hake_basePreSRG_plotPars"),sep="/"),
           strings=c("SR_BH","SR_LN","NatM","Q_extraSD"),
           newheaders=c("Steepness","LN(R0)","Natural mortality","Survey extra SD"),
           nrows=2,ncols=2,new=F)
if(doPNG){dev.off()}

####################################
#MLE vs MCMC
mymodels <- list(base,base)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- list(base$mcmc,base$mcmc)
modelnames <- c("MLE","MCMC")
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=endYr,new=F,minbthresh=0,
                  subplots=4,legend=T,col=c("red","black"),
                  spacepoints=3000,
                  shadealpha=0.1,btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
abline(h=c(0.1,0.4),lty=2)
axis(2,at=c(0.1,0.4),cex.axis=0.8)
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_spb.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=endYr,new=F,minbthresh=0,
                  subplots=2,legend=T,col=c("red","black"),
                  spacepoints=3000,
                  shadealpha=0.1,btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=endYr,new=F,minbthresh=0,
                  subplots=8,legend=T,col=c("red","black"),
                  shadealpha=0.1, spacepoints=1, type='p',
                  btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
if(doPNG){dev.off()}



############################################
### FITS
#To get aggregated fit to age
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T)
#fishery comps
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=8,rows=7,cols=6,maxrows=7,maxcols=6,showsampsize=F,showeffN=F)
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=25,pheight=5,rows=2,cols=19,maxrows=2,maxcols=19,showsampsize=F,showeffN=F)
#survey comps
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=5,rows=2,cols=5,maxrows=2,maxcols=5,showsampsize=F,showeffN=F)
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=10,pheight=2.2,rows=1,cols=9,maxrows=1,maxcols=9,showsampsize=F,showeffN=F)
#comp resids
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=8)


#numbers at age
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=12,png=T,pwidth=6.5,pheight=7)
SSplotNumbers(base,subplot=1,plot=FALSE,print=TRUE,pwidth=6.5,pheight=6,plotdir=figDir)



medCatch <- median(base$mcmc$ForeCatch_2015)/1000
ht <- 3; wd<- 6.5
if(doPNG) {png(file.path(figDir,"catch2015.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,1.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
origy <- y <- density(base$mcmc$ForeCatch_2015/1000,from=min(base$mcmc$ForeCatch_2015/1000))
plot(y,yaxt="n",ylab="Density",xlab="Projected 2015 catch based on the default harvest policy ('000 t)",type="l",lty=1,pch=16,xlim=c(0,2500),xaxs="i",yaxs="i",ylim=c(0,max(y$y)*1.02),lwd=3,main="",xaxt="n")
axis(1,at=seq(0,2600,200))
mtext("Density",side=2,line=0.5,las=0)
yy <- y
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=gray(0.9),lwd=3)
ind <- y$x>=quantile(base$mcmc$ForeCatch_2015/1000,0.025) & y$x<=quantile(base$mcmc$ForeCatch_2015/1000,0.975)
y$x <- y$x[ind]
y$y <- y$y[ind]
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=rgb(0,0,1,0.3),lty=0)
lines(origy,lwd=3)
tmpy <- y$y[min(abs(y$x-medCatch))==abs(y$x-medCatch)]
lines(c(medCatch,medCatch),c(0,tmpy),lwd=2)
text(medCatch,mean(c(0,tmpy)),paste("Median",round(medCatch,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
box()
if(doPNG){dev.off()}



medCatch <- median(base$mcmc$ForeCatch_2016)/1000
ht <- 3; wd<- 6.5
if(doPNG) {png(file.path(figDir,"catch2016.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,1.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
origy <- y <- density(base$mcmc$ForeCatch_2016/1000,from=min(base$mcmc$ForeCatch_2016/1000))
plot(y,yaxt="n",ylab="Density",xlab="Projected 2016 catch based on the default harvest policy ('000 t)",type="l",lty=1,pch=16,xlim=c(0,2500),xaxs="i",yaxs="i",ylim=c(0,max(y$y)*1.02),lwd=3,main="",xaxt="n")
axis(1,at=seq(0,2600,200))
mtext("Density",side=2,line=0.5,las=0)
yy <- y
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=gray(0.9),lwd=3)
ind <- y$x>=quantile(base$mcmc$ForeCatch_2016/1000,0.025) & y$x<=quantile(base$mcmc$ForeCatch_2016/1000,0.975)
y$x <- y$x[ind]
y$y <- y$y[ind]
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=rgb(0,0,1,0.3),lty=0)
lines(origy,lwd=3)
tmpy <- y$y[min(abs(y$x-medCatch))==abs(y$x-medCatch)]
lines(c(medCatch,medCatch),c(0,tmpy),lwd=2)
text(medCatch,mean(c(0,tmpy)),paste("Median",round(medCatch,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
box()
if(doPNG){dev.off()}


medCatch <- median(base$mcmc$ForeCatch_2017)/1000
ht <- 3; wd<- 6.5
if(doPNG) {png(file.path(figDir,"catch2017.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,1.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
origy <- y <- density(base$mcmc$ForeCatch_2017/1000,from=min(base$mcmc$ForeCatch_2017/1000))
plot(y,yaxt="n",ylab="Density",xlab="Projected 2017 catch based on the default harvest policy ('000 t)",type="l",lty=1,pch=16,xlim=c(0,2500),xaxs="i",yaxs="i",ylim=c(0,max(y$y)*1.02),lwd=3,main="",xaxt="n")
axis(1,at=seq(0,2600,200))
mtext("Density",side=2,line=0.5,las=0)
yy <- y
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=gray(0.9),lwd=3)
ind <- y$x>=quantile(base$mcmc$ForeCatch_2017/1000,0.025) & y$x<=quantile(base$mcmc$ForeCatch_2017/1000,0.975)
y$x <- y$x[ind]
y$y <- y$y[ind]
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=rgb(0,0,1,0.3),lty=0)
lines(origy,lwd=3)
tmpy <- y$y[min(abs(y$x-medCatch))==abs(y$x-medCatch)]
lines(c(medCatch,medCatch),c(0,tmpy),lwd=2)
text(medCatch,mean(c(0,tmpy)),paste("Median",round(medCatch,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
box()
if(doPNG){dev.off()}




