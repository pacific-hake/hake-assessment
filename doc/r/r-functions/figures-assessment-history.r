make.assessment.history.plot <- function(base,
                                         assessment.history, ## data as read in from the csv file
                                         end.yr){
  ## History of assessments plot
  ## addpoly <- function(yrvec, lower, upper, color){ # add shaded uncertainty intervals behind line
  ## 	lower[lower<0] <- 0 # max of value or 0
  ## 	shadeCol <- rgb(t(col2rgb(color)),alpha=0.2*255,maxColorValue=255)
  ## 	polygon(x=c(yrvec,rev(yrvec)),y=c(lower,rev(upper)), border=NA,col=shadeCol)
  ## 	lines(yrvec,lower,lty=3,col=color)
  ## 	lines(yrvec,upper,lty=3,col=color)
  ## }

  ## The csv file should be set up with blanks for SPB in the most recent year
  ##  it is read in from all.r
  xx <- assessment.history
  xx <- xx[xx$Value == "SB million mt",]
  xx <- xx[!(xx$Model == "TINSS STAR update" |
             xx$Model == "TINSS Post-STAR" |
             xx$Model == "Base lowCI" |
             xx$Model == "Base highCI" ),]
  xx[xx$Model == "TINSS SSC Final","Model"] <- "TINSS"

  yearInd <- grep("X", names(xx))
  years <- as.numeric(substring(names(xx[yearInd]), 2))
  latestAssess <- base$mcmc[,grep("SPB", names(base$mcmc))][,-c(1, 2)]
  latestYrs <- as.numeric(substring(names(latestAssess), 5))

  xx[nrow(xx), paste0("X", years[years %in% latestYrs])] <-
    apply(latestAssess, 2, median)[paste0("SPB_", years[years %in% latestYrs])] / 2e6

  slower <- base$mcmccalcs$slower
  supper <- base$mcmccalcs$supper

  yrs <- sort(unique(xx$Year))
  ## The colors for the assessment years.
  ## Need to add a new one at the beginning
  ##  every year
  cols = c(rgb(0.44, 0.1,  0.1),
           rgb(0.1,  0.1,  0.44),
           rgb(1,    0.1,  0.6),
           rgb(1,    0.8,  0),
           rgb(0,    1,    1),
           rgb(0.5,  0,    0.5),
           rgb(0.18, 0.55, 0.34),
           rgb(0,    0,    0.8),
           rgb(0,    0.8,  0.8),
           rgb(0.25, 0.88, 0.82, 0.7),
           rgb(0.5,  1,    0.8,  0.7),
           rgb(1,    0.84, 0,    0.7),
           rgb(0,    0.75, 1,    0.7),
           rgb(1,    0,    1,    0.7),
           rgb(0.5,  0.5,  0.2,  1),
           rgb(0.85, 0.65, 0.13, 0.7),
           rgb(0.27, 0.51, 0.71),
           rgb(0.13, 0.70, 0.67),
           rgb(1,    0,    1),
           rgb(1,    0,    0),
           rgb(0,    0,    1),
           rgb(0,    0.8,  0))

  lwds <- c(rep(1,nrow(xx)-1), 3)
  pchs <- rep(c(18, 15, 17, 4, 20, 3), 4) ## repeat it more than necessary
  legCol <- legPch <- rep(NA, nrow(xx))

  par(mar = c(3, 3, 1, 6) + 0.1, mgp = c(2, 1, 0))
  ymax <- 1.05*max(xx[,yearInd], na.rm = TRUE)
  plot(range(years),
       range(xx[,yearInd], na.rm = TRUE),
       type = "n",
       xlab = "Year",
       ylab = "Spawning Biomass (million t)",
       las = 1,
       xlim = c(min(years), max(years)),
       ylim = c(0, ymax),
       yaxs = "i",
       cex.axis = 0.9,
       cex.lab = 1)
  for(i in 1:(nrow(xx) - 1)){
    legCol[i] <- cols[yrs == xx$Year[i]]
    if(sum(xx$Year == xx$Year[i]) > 1) {  ## put a symbol on it to differentiate years
      legPch[i] <- pchs[i]
    }
    lines(years,
          xx[i, yearInd],
          col = legCol[i],
          lwd = lwds[i],
          pch = legPch[i],
          type = "o",
          cex = 0.7)
  }
  # add x-axis mark for final year (without label if too close to nearest decade mark)
  axis(1, at=max(years), label=ifelse(max(years)%%10 < 2, "", max(years)))
  i <- i + 1
  legCol[i] <- rgb(0, 0, 0, 0.8)
  lwds[i] <- 3
  tmpCol <- rgb(t(col2rgb(legCol[i])),
                alpha = 0.6 * 255,
                maxColorValue = 255)
  lines(years,
        xx[i, yearInd],
        col = tmpCol,
        lwd = lwds[i])
  yrs <- years[years %in% latestYrs]
  addpoly(yrs,
          slower[names(slower) %in% yrs],
          supper[names(supper) %in% yrs],
          "black")
  legend(end.yr + 0.5,
         7.15,
         paste(xx$Year, xx$Model),
         col = legCol,
         lwd = lwds + 1,
         pch = legPch,
         cex = 0.65,
         bty = "n",
         xpd = NA)
}
