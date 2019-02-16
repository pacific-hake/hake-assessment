weight.at.age.heatmap <- function(model,
                                  fleet = 1,
                                  proj.line.color = "royalblue",
                                  proj.line.width = 1,
                                  proj.line.yr = 2018,
                                  # mean ages need to be updated every year
                                  longterm.mean.ages = c(0.02,
                                                         0.09,
                                                         0.25,
                                                         0.38,
                                                         0.49,
                                                         0.53,
                                                         0.58,
                                                         0.65,
                                                         0.71,
                                                         0.78,
                                                         0.86,
                                                         0.92,
                                                         0.96,
                                                         1.06,
                                                         1.00,
                                                         1.03),
                                  font.size = 4,
                                  axis.font.size = 10){
  ## Weight-at-age heatmap plot including extrapolated years using ggplot.
  ## Original code not available during shutdown.
  ## Max age is set to 15 as we don't know what was extrapolated above that
  ##  and the figure in the assessment doc is only to 15
  ## fleet is number as seen in SS wtatage.ss file for fleet column
  ## Years after end of data up to last.yr will be projection years,
  ## font.size is size of font

  ## Toggle data frame for which values are extrapolated values
  last.data.yr <- 2018

  input.yrs <- 1975:last.data.yr
  extrap = list(
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1), #1975
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1976
    c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1977
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1978
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1979
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1980
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1981
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1982
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1983
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1984
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1985
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1986
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1987
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), #1988
    c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1989
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), #1990
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), #1991
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1992
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1993
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1994
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1995
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), #1996
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1997
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1998
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #1999
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2000
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2001
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2002
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2003
    c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2004
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2005
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2006
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2007
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2008
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2009
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2010
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2011
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2012
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2013
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2014
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2015
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2016
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), #2017
    c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) #2018
  names(extrap) <- input.yrs

  extrap <- as_tibble(cbind(input.yrs, t(bind_rows(extrap))))

  if(proj.line.yr != last.data.yr){    # assume only different by one due to SS configuration
      # want years above the blue line to bold:
      extrap[nrow(extrap), 2:ncol(extrap)] <- rep(1, ncol(extrap)-1)
      # warning("Projection line year does not equal the last data year. ",
      #         "Check weight.at.age.heatmap() and make sure values are correct.")
  }


  wa <- as_tibble(model$wtatage) %>%
    dplyr::filter(Fleet == fleet) %>%
    select(-c(Seas, Sex, Bio_Pattern, BirthSeas, Fleet, comment)) %>%
     dplyr::filter(Yr > 0)

  wa <- wa[,1:17]
  names(extrap) <- names(wa)

  ## wa and extrap are in the same format but extrap needs more rows for fully extrapolated years
  ##  before and after

  j <- subset(wa, !(wa$Yr %in% extrap$Yr))
  j[,2:17] <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  extrap <- bind_rows(extrap, j) %>%
    arrange(Yr)

  last.yr <- max(wa$Yr)

  wa1 <- wa2 <- wa[,-1]
  extrap1 <- extrap[,-1]
  wa1[!extrap1] <- NA
  wa2[extrap1 == 1] <- NA
  wa1 <- cbind(Yr = wa$Yr, wa1)
  wa2 <- cbind(Yr = wa$Yr, wa2)

  w <- reshape2::melt(wa, id.vars = "Yr")
  w1 <- reshape2::melt(wa1, id.vars = "Yr")
  w2 <- reshape2::melt(wa2, id.vars = "Yr")

  ages <- as.numeric(levels(unique(w$variable)))
  nage <- length(ages)

  colors <- colorRampPalette(c("red",
                               "yellow",
                               "green",
                               "dodgerblue"))(nage - 1)
  avg <- data.frame(Yr = min(w$Yr) - 2,
                    variable = ages,
                    value = longterm.mean.ages)
  w <- as.data.frame(rbind(w, avg))
  w$Yr <- as.integer(w$Yr)
  w$value <- as.numeric(w$value)

  w1 <- as.data.frame(rbind(w1, avg))
  w1$Yr <- as.integer(w1$Yr)
  w1$value <- as.numeric(w1$value)

  w2 <- as.data.frame(rbind(w2, avg))
  w2$Yr <- as.integer(w2$Yr)
  w2$value <- as.numeric(w2$value)

  g <- ggplot(w)+
    geom_tile(aes(x = variable, y = Yr, fill = value)) +
    scale_fill_gradientn(colors = colors, guide = FALSE) +
    geom_text(aes(x = w2$variable,
                  y = w2$Yr,
                  label = ifelse(is.na(w2$value),
                                 "",
                                 f(w2$value, 2))),
              size = font.size) +
    geom_text(aes(x = w1$variable,
                  y = w1$Yr,
                  label = ifelse(is.na(w1$value),
                                 "",
                                 f(w1$value, 2))),
              fontface = "bold",
              size = font.size) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = axis.font.size),
          axis.text.y = element_text(size = axis.font.size)) +
    scale_y_continuous(breaks = seq(min(w$Yr), max(w$Yr), 1),
                       labels = c("mean",
                                  "",
                                  seq(min(w$Yr),
                                      max(w$Yr),
                                      1)[-c(1,2)])) +
    ylab("Year") +
    xlab("Age") +
    ## Add line separating pre-1975 and data:
    geom_hline(yintercept = input.yrs[1] - 0.5,
               color = proj.line.color,
               size = proj.line.width) +
    coord_cartesian(expand = FALSE)

  if(last.yr > last.data.yr){
    ## Add line separating projections
    g <- g + geom_hline(yintercept = proj.line.yr + 0.5,
                        color = proj.line.color,
                        size = proj.line.width)
  }
  g
}

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
  df2$age <- df2$FISH_AGE_YEARS_FINAL
  make.size.at.age.plot(df2, type='len')
  make.size.at.age.plot(df2, type='wt')
}
