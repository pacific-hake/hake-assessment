#' Plot the history of assessments spawning biomass estimaed trajectory
#'
#' @details The csv file should be set up with blanks for SSB in the most recent year
#' it is read in data-tables.r which is sourced in all.R
##' @param base This year's base model
##' @param assessment.history A data frame read in from the assessment history file
##' @param par.mar par(mar = c(x, x, x, x)) values; default is for doc
make.assessment.history.plot <- function(base,
                                         assessment.history,
                                         par.mar = c(3.1, 3.1, 1.1, 6.1),
                                         cex.legend = 0.5){

  xx <- assessment.history
  xx <- xx[xx$Value == "SB million mt",]
  xx <- xx[!(xx$Model == "TINSS STAR update" ||
             xx$Model == "TINSS Post-STAR" ||
             xx$Model == "Base lowCI" ||
             xx$Model == "Base highCI" ),]
  xx[xx$Model == "TINSS SSC Final","Model"] <- "TINSS"

  yearInd <- grep("X", names(xx))
  years <- as.numeric(substring(names(xx[yearInd]), 2))
  latestAssess <- base$mcmc[, grep("SSB_[0-9]", names(base$mcmc))]
  latestYrs <- as.numeric(substring(names(latestAssess), 5))

  # Change last row only. New model year's median
  yr.colnames <- paste0("X", years[years %in% latestYrs])
  yr.ssb.colnames <- paste0("SSB_", years[years %in% latestYrs])
  xx[nrow(xx), yr.colnames] <- apply(latestAssess, 2, median)[yr.ssb.colnames] / 1e6
  end_yr <- xx[nrow(xx),]$Year

  slower <- base$mcmccalcs$slower
  supper <- base$mcmccalcs$supper

  yrs <- sort(unique(xx$Year))
  # The colors for the assessment years.
  # Need to add a new one every year
  cols <- c(rgb(0.44, 0.1,  0.1),       # 1991
            rgb(0.1,  0.1,  0.44),      # 1992
            rgb(1,    0.1,  0.6),       # 1993
            rgb(1,    0.8,  0),         # 1994
            rgb(0,    1,    1),         # 1995
            rgb(0.5,  0,    0.5),       # 1996
            rgb(0.18, 0.55, 0.34),      # 1997
            rgb(0,    0,    0.8),       # 1999
            rgb(0,    0.8,  0.8),       # 2002
            rgb(0.25, 0.88, 0.82, 0.7), # 2004
            rgb(0.5,  1,    0.8,  0.7), # 2005
            rgb(1,    0.84, 0,    0.7), # 2006
            rgb(0,    0.75, 1,    0.7), # 2007
            rgb(1,    0,    1,    0.7), # 2008
            rgb(0.5,  0.5,  0.2,  1),   # 2009
            rgb(0.85, 0.65, 0.13, 0.7), # 2010
            rgb(0.27, 0.51, 0.71),      # 2011
            rgb(0.13, 0.70, 0.67),      # 2012
            rgb(1,    0,    1),         # 2013
            rgb(1,    0,    0),         # 2014
            rgb(0,    0,    1),         # 2015
            rgb(0,    0.8,  0),         # 2016
            rgb(1,    0.2,  0.2),       # 2017
            rgb(1,    0.5,  0),         # 2018
            rgb(0.7, 0.7, 0.7),         # 2019
            rgb(0.1, 0.1, 1),           # 2020
            rgb(0.5, 0.6, 0.7),         # 2021
            rgb(1, 0.2, 1))             # 2022

  # set line widths and plot characters
  lwds <- c(rep(1,nrow(xx) - 1), 3)
  pchs <- rep(c(18, 15, 17, 4, 20, 3), 4) ## repeat it more than necessary
  legCol <- legPch <- rep(NA, nrow(xx))

  # set parameters for margins and axes
  par(mar = par.mar,
      mgp = c(2, 1, 0))
  ymax <- 1.05 * max(xx[,yearInd], na.rm = TRUE)

  # make empty plot
  plot(range(years),
       range(xx[,yearInd], na.rm = TRUE),
       type = "n",
       xlab = "Year",
       ylab = "Spawning biomass (million t)",
       las = 1,
       xlim = c(min(years), max(years)),
       ylim = c(0, ymax),
       yaxs = "i",
       cex.axis = 0.9,
       cex.lab = 1)
  # loop over rows in the table of values to add lines
  for(i in 1:(nrow(xx) - 1)){
    legCol[i] <- cols[yrs == xx$Year[i]]
    # if there is more than one assessment within a year,
    # put a symbol on it to differentiate years
    if(sum(xx$Year == xx$Year[i]) > 1) {
      legPch[i] <- pchs[i]
    }
    # add lines
    lines(years,
          xx[i, yearInd],
          col = legCol[i],
          lwd = lwds[i],
          pch = legPch[i],
          type = "o",
          cex = 0.7)
  }
  # OLD: add x-axis mark for final year (without label if too close to nearest decade mark)
  # axis(1, at=max(years), label=ifelse(max(years)%%10 %in% c(0:2,8:9), "", max(years)))
  # NEW: add little tick marks for wide range of years
  #      to avoid gap at end (e.g. ticks up to 2018, then gap at 2019)
  axis(1, at = 1950:2030, labels = rep("", length(1950:2030)), tcl = -0.3)

  # add values for base model
  i <- i + 1
  # color in legend
  legCol[i] <- rgb(0, 0, 0, 0.8)
  lwds[i] <- 3
  # add more transparency (0.6 instead of 0.8 to help see other lines)
  tmpCol <- rgb(t(col2rgb(legCol[i])),
                alpha = 0.6 * 255,
                maxColorValue = 255)
  # add line
  lines(years,
        xx[i, yearInd],
        col = tmpCol,
        lwd = lwds[i])
  yrs <- years[years %in% latestYrs]
  # make polygon showing uncertainty around base model
  # addpoly function is defined in utilities.r

  addpoly(yrs,
          slower[names(slower) %in% yrs],
          supper[names(supper) %in% yrs],
          "black")
  # add legend
  legend(par("usr")[2],
         par("usr")[4],
         paste(xx$Year, xx$Model),
         col = legCol,
         lwd = lwds + 1,
         pch = legPch,
         cex = cex.legend,
         bty = "n",
         xpd = NA)

}
##
##' @param base This year's base model
##' @param assessment.history.disp A data frame read in from the assessment history SSBdispersion file
##' @param par.mar par(mar = c(x, x, x, x)) values; default is for doc
make.assessment.history.disp.plot <- function(base,
                                         assessment.history.disp){

  xx <- assessment.history.disp
  # define multi-figure layout and margins
  par(mfrow = c(1, 2), mar=c(4, 4, 1, 1), oma = c(1,1, 0, 0))

  plot(xx$InterQuartileRange~xx$Assessment_Year,type="o",ylim=c(0,2000),
     ylab="Interquartile Range of Spawning Biomass Estimate ('000s)",
     xlab="Assessment Year",
     cex.lab=0.85,
     col="black",
     cex=0.85,
     lwd=2,
     las=1)

  plot(xx$QuartileCoeffDispersion~xx$Assessment_Year,type="o",ylim=c(0,0.6)
     ,ylab="Quartile Coefficient of Dispersion",
     xlab="Assessment Year",
     cex.lab=0.85,
     col="black",
     cex=0.85,
     lwd=2,
     las=1)
}