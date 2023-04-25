
barfun <- function(x, y, x.pos="left", plot=1, ...){
  #make barplot-like shape which is really a polygon
  if(any(sort(x)!=x)){
    stop("x must be a vector of strictly increasing values")
  }
  if(length(x)!=length(y) | any(!is.numeric(x), !is.numeric(y))){
    stop("x and y must be numeric vectors of the same length")
  }
  n <- length(x)

  if(x.pos=="left"){
    # x-values represent left-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x, tail(x,1) + diff(tail(x,2)))
  }
  if(x.pos=="right"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x[1], x[1] - diff(x[1:2]))
  }
  if(x.pos=="center"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    diff <- diff(head(x,2))
    x.vec <- c(x[1] - diff/2, x + diff/2)
  }
  x.double <- sort(c(x.vec, x.vec))
  y.double <- c(0, y[sort(rep(1:n, 2))], 0)
  if(plot){
    polygon(x.double, y.double, ...)
  }
  return(data.frame(x=x.double, y=y.double))
}

# source atsea.ages as noted in /hake-data/Rcode/AtSeaComps.R
make.size.at.age.plot <- function(df, type='len'){
  # df is a data.frame with columns for month, length, and weight.

  df.early <- df[df$month<8,] # up through July
  df.late <- df[df$month>=8,] # August and after
  #par(mfrow=c(8,1), mar=rep(0,4), oma=c(4,4,1,1))
  par(mar=c(4.1,4.1,1,1))

  if(type=="len"){
    ylim <- c(20, 56)
    breaks <- seq(0,85,1)
    y.axis.ticks <- seq(0, 85, 2)
    y.text <- 25
    scale <- 2.5
    ylab <- "Length (cm)"
  }
  if(type=="wt"){
    ylim <- c(0, 1.2)
    breaks <- seq(0, 7.0, 0.05)
    y.axis.ticks <- seq(0, 2.0, 0.1)
    y.text <- 0.1
    scale <- 0.08
    ylab <- "Weight (kg)"
  }

  plot(0, type='n', xlim=c(2,12), ylim=ylim,
       xaxs='i', yaxs='i', xlab="Age", ylab=ylab, axes=FALSE)
  axis(1, at=1:(round(par()$usr[2])-1))
  axis(2, las=1, at=y.axis.ticks)
  #for(period in c("all","early","late")){
  fillvec <- c(rgb(.6, .6, .6, 1),
               rgb(1, 0, 0, .1),
               rgb(0, 0, 1, .1))
  bordervec <- c(rgb(.6, .6, .6, 1),
                 rgb(.7, 0, 0, .8),
                 rgb(0, 0, .7, .8))

  periods <- 2:3
  for(period in periods){
    if(period==1) df.tmp <- df       # all months
    if(period==2) df.tmp <- df.early # up to month 7
    if(period==3) df.tmp <- df.late  # month 8 and onward

    if(type=='len'){
      variable <- df.tmp$length
    }
    if(type=='wt'){
      variable <- df.tmp$weight
    }

    # loop over ages
    for(age in 2:20){
      hist.info <- hist(variable[df.tmp$age==age],
                        breaks=breaks, plot=FALSE)
      # use barfun function defined at the top of this file
      polygon.outline <- barfun(x=hist.info$breaks[-length(hist.info$breaks)],
                                y=hist.info$density)
      polygon(x=age + scale*polygon.outline$y, y=polygon.outline$x,
              col=fillvec[period], border=bordervec[period])
      n <- sum(df.tmp$age==age, na.rm=TRUE)
      text(x=age, y=y.text - .025*diff(ylim)*period,
           labels=paste0("n=",n), col=bordervec[period], cex=.8, pos=4)
    }
  }
  if(type=='len'){
    abline(h=y.axis.ticks, lty=3, col='grey40')
    abline(h=seq(0, 80, 10), lty=1, col='grey40', lwd=1)
  }
  if(type=='wt'){
    abline(h=y.axis.ticks, lty=3, col='grey40')
    abline(h=seq(0, 1.2, .2), lty=1, col='grey40', lwd=1)
  }
  abline(v=0:10)
  legend('topleft', fill=fillvec[periods], border=bordervec[periods],
         bg='white',
         legend=c("May-November", "May-July", "August-November")[periods])
}

if(FALSE){
  # load atsea.ages containing fish sample information from U.S. at-sea fishery
  # (contains other confidential information not saved here)
  df <- atsea.ages
  df$month <- as.numeric(substring(df$HAUL_OFFLOAD_DATE, 6, 7))
  df$year <- as.numeric(substring(df$HAUL_OFFLOAD_DATE, 1, 4))
  # subset for 2016 only and then look at Spring and Fall values
  df <- df[df$year==2016,]
  df$length=df$LENGTH
  df$weight=df$WEIGHT

  setwd('C:/github/hake-assessment/doc/r')
  dir.SRG.management <- "../../beamer/SRG/Management/"
  cairo_ps(filename = file.path(dir.SRG.management,
                       "Figures/length-at-age-U.S.-atsea-2016.eps"),
                   width = 6, height = 5, pointsize = 10)
  make.size.at.age.plot(df, type='len')
  dev.off()
  cairo_ps(filename = file.path(dir.SRG.management,
                       "Figures/weight-at-age-U.S.-atsea-2016.eps"),
                   width = 6, height = 5, pointsize = 10)
  make.size.at.age.plot(df, type='wt')
  dev.off()

  # load bds.fish.worked containing fish sample information from U.S. shore-based fishery
  # (contains other confidential information not saved here)
  df2 <- bds.fish.worked
  df2 <- df2[df2$SAMPLE_YEAR==2016,]
  df2$month <- df2$SAMPLE_MONTH
  df2$length <- df2$FISH_LENGTH/10
  df2$weight <- df2$FISH_WEIGHT
  df2$age <- df2$AGE
  make.size.at.age.plot(df2, type='len')
  make.size.at.age.plot(df2, type='wt')
}
