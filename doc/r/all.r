## Pacific Hake Joint Technical Committee, January 2016
## all.r - Source this file to load all data and functions,
## then save the R environment to .RData in this directory. This
## will be read in by knitr as a binary file so that multiple
## loads don't happen during the latex/knitr build ( they are
## very slow compared to loading the binary once at the beginning).

## Check for specific packages and install them if they
## are not yet installed. Note this will be really slow the
## first time you run it, especially if they have dependencies.

rm(list=ls(all=TRUE))

if(!("nwfscSurvey" %in% rownames(installed.packages()))){
  devtools::install_github("nwfsc-assess/nwfscSurvey")
}

if(!("nwfscMapping" %in% rownames(installed.packages()))){
  devtools::install_github("nwfsc-assess/nwfscMapping")
}

if(!("r4ss" %in% rownames(installed.packages()))){
  devtools::install_github("r4ss/r4ss")
}

## if(!("PBSmapping" %in% rownames(installed.packages()))){
##  install.packages("PBSmapping")
##}

require(nwfscSurvey)
require(nwfscMapping)
require(date)
require(r4ss)
require(xtable)
## require(PBSmapping)

source("catches.r")
source("load-models.r")
source("survey.r")
source("figures-timeseries.r")
source("tables-timeseries.r")
source("tables-reference-points.r")

yr <- 2015

data.path <- file.path("..","..","data")
models.path <- file.path("..","..","models")

catches <- load.catches(file.path(data.path,"2016HakeCatches_preliminary_2016.01.04.csv"))
landings.vs.tac <- load.landings.tac(file.path(data.path,"Landings_vs_TAC.csv"))
models <- load.models(models.path, yr = yr)




rc <- rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

plotBars.fn <- function(x,y,gap=0,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value,...)
  if(add) points(x,y$value,...)
  segments(x,y$lo,x,y$value-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi,x,y$value+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

plotBars.fn <- function(x,y,gap=0,scalar=1e6,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value/scalar,...)
  if(add) points(x,y$value/scalar,...)
  segments(x,y$lo/scalar,x,y$value/scalar-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi/scalar,x,y$value/scalar+gap,col=ciCol,lty=ciLty,lwd=ciLwd) 
}

addpoly <- function(yrvec, lower, upper, color){ # add shaded uncertainty intervals behind line
  lower[lower<0] <- 0 # max of value or 0
  shadeCol <- rgb(t(col2rgb(color)),alpha=0.2*255,maxColorValue=255)
  polygon(x=c(yrvec,rev(yrvec)),y=c(lower,rev(upper)),
          border=NA,col=shadeCol)
  lines(yrvec,lower,lty=3,col=color)
  lines(yrvec,upper,lty=3,col=color)
}

randWalkSelex.fn <- function(pars,devs=NULL,bounds=NULL) {
  ## calculates the selectivity from the random walk parameters in SS (option 17)
  ## -1000 means to set equal to 0
  ## assumes that this is all pars from age 0 to max age

  logS <- rep(NA,length(pars))
  logS[1] <- 0 #first value is never estimated (age 0)
  if(!is.null(devs)) {
    ## transform parameters based on bounds
    for(a in 2:length(pars)) {
      if(!is.na(devs[a])) {
        tmp <- log((bounds[2]-bounds[1]+0.0000002)/(pars[a]-bounds[1]+0.0000001)-1)/(-2)
        tmp <- tmp + devs[a]
        pars[a] <- bounds[1]+(bounds[2]-bounds[1])/(1+exp(-2*tmp))
      }
    }
  }
  for(a in 2:length(pars)) {
    ifelse(pars[a] == -1000, logS[a] <- 0, logS[a] <- logS[a-1]+pars[a])
  }

  selex <- exp(logS-max(logS))
  selex[pars== -1000] <- 0
  return(selex)
}

#randWalkSelex.fn(c(-1000,-1000,0,0.317379,0.00653887,-0.0244099,0.449238,0,0,0,0,0,0,0,0))
#randWalkSelex.fn(c(-1000,0,4.02533, 1.65537,   0.49088, 0.264563, 0.330266, 0,0,0,0,0,0,0,0,0),
#             devs=c(NA, NA,0.203144,-0.102409,-0.020993,0.0786477,0.0492123,NA,NA,NA,NA,NA,NA,NA,NA,NA),
#             bounds=c(-5,9))

selexYear.fn <- function(x,yr,bnds=c(-5,9)) {
    ## specific for hake 2013 and 2014
    selexPars <- matrix(c(-1000,0,NA,NA,NA,NA,NA,0,0,0,0,0,0,0,0,0),nrow=nrow(x),ncol=16,byrow=T)
    devsPars  <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))

    tmp <- grep("AgeSel_1P_[1-9]_Fishery",names(x))
    devsInd <- grep("AgeSel_1P_[1-9]_Fishery_DEVadd",names(x))
    allDevsPars <- x[,devsInd]
    selexPars[,3:7] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
    devsInd <- grep(as.character(yr),names(x)[devsInd])
    devsPars[,3:7] <- as.matrix(allDevsPars[,devsInd])

    selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
    for(i in 1:nrow(selexPars)) {
        selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
    }

    return(selex)
}

selexYear10.fn <- function(x,yr,bnds=c(-5,9)) {
  ## specific for hake 2013 and 2014
  selexPars <- matrix(c(-1000,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,0,0,0),nrow=nrow(x),ncol=16,byrow=T)
  devsPars  <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))

  tmp <- grep("AgeSel_1P_[0-9]+_Fishery",names(x))
  devsInd <- grep("AgeSel_1P_[0-9]+_Fishery_DEVadd",names(x))
  allDevsPars <- x[,devsInd]
  selexPars[,3:11] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
  devsInd <- grep(as.character(yr),names(x)[devsInd])
  devsPars[,3:11] <- as.matrix(allDevsPars[,devsInd])

  selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
  for(i in 1:nrow(selexPars)) {
    selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
  }

  return(selex)
}

biomass_fraction_plots <- function(replist, selected=FALSE){
  ## biomass fraction of ages 4+
  ## get weight at age
  wtatage <- replist$wtatage[replist$wtatage$fleet==1,-(2:6)]
  ## make years positive
  names(wtatage)[1] <- "Yr" # avoid annoying mix of 'yr' and 'Yr'
  wtatage$Yr <- abs(wtatage$Yr)
  ## get equilibrium value (mean across years for hake)
  wtatage.mean <- wtatage[1,]
  ## get numbers at age
  natage <- replist$natage[replist$natage$"Beg/Mid"=="B",-c(1:6,8:11)]
  ## fill in missing years in weight at age with equilibrium value
  for(y in setdiff(natage$Yr, wtatage$Yr)){
    tmp <- wtatage.mean
    tmp$Yr <- y
    wtatage <- rbind(wtatage,tmp)
  }
  wtatage <- wtatage[order(wtatage$Yr),]
  Yrs <- rownames(natage) <- natage$Yr
  ## get fishery selectivity (method would differ if it were time-varying)
  sel <- replist$ageselex[replist$ageselex$fleet==1 &
                          replist$ageselex$factor=="Asel",c("year",paste(0:20))]
  if(nrow(sel)<10){
    ## if not time-varying, repeat vector for all years
    sel <- matrix(as.numeric(sel[1,-1]),ncol=ncol(sel)-1,nrow=length(Yrs),byrow=TRUE)
  }else{
    ## if time-varying, fiddle with years to make them match the numbers at age
    sel.init <- sel[sel$year==1963,]
    sel <- sel[sel$year!=1963,]
    for(y in 1965:1964){
      sel.init$year <- y
      sel <- rbind(sel.init, sel)
    }
    sel.2014 <- sel[sel$year==2014,]
    for(y in 2015:2016){
      sel.2014$year <- y
      sel <- rbind(sel, sel.2014)
    }
    if(any(sel$year!=Yrs)){
      stop("problem with selectivity")
    }else{
      sel <- sel[,-1]
    }
  }

  ## calculate biomass at age
  batage <- natage[,-1]*wtatage[,-1]
  ## selected biomass at age
  batage.sel <- batage*sel

  if(selected){
    B0plus <- as.numeric(apply(batage.sel, 1, sum))
    B4plus <- as.numeric(apply(batage.sel[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage.sel[, 0:20 >= 5], 1, sum))
  }else{
    B0plus <- as.numeric(apply(batage, 1, sum))
    B4plus <- as.numeric(apply(batage[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage[, 0:20 >= 5], 1, sum))
  }

  ## define time-periods
  par(mfrow=c(2,1))
  main.yrs <- Yrs %in% 1966:2014
  fore.yrs <- Yrs >= 2014

  ## plot timeseries of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,ceiling(max(B0plus/1e6))), yaxs='i', las=1,
       xlab='Year', ylab='Biomass (millions of mt)')
  lines(Yrs[main.yrs], B0plus[main.yrs]/1e6, lwd=3, col=1, lty=1)
  lines(Yrs[main.yrs], B4plus[main.yrs]/1e6, lwd=3, col=2, lty=1)
  lines(Yrs[main.yrs], B5plus[main.yrs]/1e6, lwd=3, col=4, lty=1)
  lines(Yrs[fore.yrs], B0plus[fore.yrs]/1e6, lwd=2, col=1, lty='12')
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/1e6, lwd=2, col=2, lty='12')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/1e6, lwd=2, col=4, lty='12')
  points(Yrs[1], B0plus[1]/1e6,lwd=3)
  points(Yrs[1], B4plus[1]/1e6,col=2,lwd=3)
  points(Yrs[1], B5plus[1]/1e6,col=4,lwd=3)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=1:5,lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(1,2,4), ncol=3,
         legend=c("All ages","Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated selected biomass", "Estimated total biomass"))

  ## plot timeseries of fractions of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,1), yaxs='i', las=1,
       xlab='Year', ylab='Fraction of biomass')
  lines(Yrs[main.yrs], B4plus[main.yrs]/B0plus[main.yrs], lwd=3, col=2)
  lines(Yrs[main.yrs], B5plus[main.yrs]/B0plus[main.yrs], lwd=3, col=4)
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=2, lty='11')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=4, lty='11')
  points(Yrs[1], B4plus[1]/B0plus[1], lwd=3, col=2)
  points(Yrs[1], B5plus[1]/B0plus[1], lwd=3, col=4)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=seq(0,0.8,.2),lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(2,4), ncol=2,
         legend=c("Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated fractions of selected biomass", "Estimated fractions of total biomass"))
}

fmt0 <- function(x, dec.points=0){
  ## Format 0
  ## Format x to have supplied number of decimal points
  ## Make thousands seperated by commas and no decimal point
  return(format(round(x,dec.points),big.mark=","))
}

get.align <- function(num,
                      first.left = TRUE, ## Keep the first column left-justified
                                         ## If FALSE, it will be justified according to the 'just' argument
                      just = "r"         ## just is the justification to use for the columns, "r", "l", or "c"
                      ){
  ## Returns a character vector used in the align argument of the xtable command.
  ## e.g. posterior output tables, reference point tables. Most tables really.
  ## num is the number of columns in the table
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  return(align)
}
