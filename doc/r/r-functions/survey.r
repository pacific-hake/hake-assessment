make.survey.history.table <- function(dat,
                                      digits = 3,           ## number of decimal points for biomass and cv
                                      xcaption = "default", ## Caption to use
                                      xlabel   = "default", ## Latex label to use
                                      font.size = 9,        ## Size of the font for the table
                                      space.size = 10){       ## Size of the spaces for the table
  ## dat is a data frame containing the survey history
  ## The vessel names neew to be fixed. They are seperated by spaces, and may or may not have dashes in their names
  ## The dashes will be replaced with spaces, and the spaces will be replaced by newlines in the output

  dat$biomass[!is.na(dat$biomass)] <- f(dat$biomass[!is.na(dat$biomass)], digits)
  dat$cv[!is.na(dat$cv)] <- f(dat$cv[!is.na(dat$cv)], digits)
  dat$vessels <- gsub(" ", "\\\\\\\\", dat$vessels)
  dat$vessels <- gsub("-", " ", dat$vessels)
  dat$vessels <- paste0("\\specialcell{", dat$vessels,"}")
  dat[is.na(dat)] <- "--"

  colnames(dat) <- c("\\specialcell{\\textbf{Year}}",
                     "\\specialcell{\\textbf{Start date}}",
                     "\\specialcell{\\textbf{End date}}",
                     "\\specialcell{\\textbf{Vessels}}",
                     "\\specialcell{\\textbf{Biomass}\\\\\\textbf{index}\\\\\\textbf{(million t)}}",
                     "\\specialcell{\\textbf{Sampling CV}}",
                     "\\specialcell{\\textbf{Number of}\\\\\\textbf{hauls with bio.}\\\\\\textbf{samples}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(dat, caption=xcaption, label=xlabel, align=get.align(ncol(dat), just = "c")),
               caption.placement = "top", include.rownames=FALSE, table.placement="H", sanitize.text.function=function(x){x}, size=size.string))
}

make.survey.extrap.table <- function(dat,
                                     digits = 3,           ## number of decimal points for biomass and cv
                                     xcaption = "default", ## Caption to use
                                     xlabel   = "default", ## Latex label to use
                                     font.size = 9,        ## Size of the font for the table
                                     space.size = 10){       ## Size of the spaces for the table
  ## dat is a data frame containing the survey comparisons.

  ## Remove 2016 columns
  dat <- dat[,-grep("2016", names(dat))]

  ## Format the columns individually, avoiding any NA's
  dat$no.extrap[!is.na(dat$no.extrap)] <- f(dat$no.extrap[!is.na(dat$no.extrap)] / 1000, digits)
  dat$with.extrap[!is.na(dat$with.extrap)] <- f(dat$with.extrap[!is.na(dat$with.extrap)] / 1000, digits)
  dat$design.based[!is.na(dat$design.based)] <- f(dat$design.based[!is.na(dat$design.based)] / 1000, digits)
  dat$cv.no.extrap[!is.na(dat$cv.no.extrap)] <- paste0(f(100 * dat$cv.no.extrap[!is.na(dat$cv.no.extrap)], 1), "\\%")
  dat$cv.with.extrap[!is.na(dat$cv.with.extrap)] <- paste0(f(100 * dat$cv.with.extrap[!is.na(dat$cv.with.extrap)], 1), "\\%")
  dat[is.na(dat)] <- "--"

  colnames(dat) <- c("\\specialcell{\\textbf{Year}}",
                     "\\specialcell{\\textbf{Biomass with}\\\\\\textbf{extrapolation}\\\\\\textbf{(million t)}}",
                     "\\specialcell{\\textbf{Sampling CV}\\\\\\textbf{with}\\\\\\textbf{extrapolation}}",
                     "\\specialcell{\\textbf{Biomass no}\\\\\\textbf{extrapolation}\\\\\\textbf{(million t)}}",
                     "\\specialcell{\\textbf{Sampling CV}\\\\\\textbf{no}\\\\\\textbf{extrapolation}}",
                     "\\specialcell{\\textbf{Biomass}\\\\\\textbf{Design-based}\\\\\\textbf{(million t)}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat), just = "c")),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = "H",
               sanitize.text.function = function(x){x},
               size = size.string))
}

make.survey.summary.table <- function(dat,
                                      digits = 3,           ## number of decimal points for biomass and cv
                                      xcaption = "default", ## Caption to use
                                      xlabel   = "default", ## Latex label to use
                                      font.size = 9,        ## Size of the font for the table
                                      space.size = 10){       ## Size of the spaces for the table
  ## dat is a data frame containing the survey comparisons.

  ## Format the columns individually, avoiding any NA's
  dat$biomass.2016[!is.na(dat$biomass.2016)] <- f(dat$biomass.2016[!is.na(dat$biomass.2016)] / 1000, digits)
  dat$biomass.2017[!is.na(dat$biomass.2017)] <- f(dat$biomass.2017[!is.na(dat$biomass.2017)] / 1000, digits)

  dat$cv.2016[!is.na(dat$cv.2016)] <- paste0(f(dat$cv.2016[!is.na(dat$cv.2016)] * 100, 1), "\\%")
  dat$cv.2017[!is.na(dat$cv.2017)] <- paste0(f(dat$cv.2017[!is.na(dat$cv.2017)] * 100, 1), "\\%")

  dat[is.na(dat)] <- "--"

  colnames(dat) <- c("\\specialcell{\\textbf{Year}}",
                     "\\specialcell{\\textbf{Biomass estimate}\\\\\\textbf{2016}\\\\\\textbf{(million t)}}",
                     "\\specialcell{\\textbf{Sampling CV}\\\\\\textbf{2016}}",
                     "\\specialcell{\\textbf{Biomass estimate}\\\\\\textbf{2017}\\\\\\textbf{(million t)}}",
                     "\\specialcell{\\textbf{Sampling CV}\\\\\\textbf{2017}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat), just = "c")),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = "H",
               sanitize.text.function = function(x){x},
               size = size.string))
}

make.survey.biomass.plot <- function(model,
                                     xlab = "Year",
                                     ylab = "Biomass Index Estimate (million t)"
                                     ){
  ## It is assumed that the data file has been read in correctly.
  ## Assumes that there is only one 'CPUE' index and it is the acoustic survey.
  ## There is no error checking to warn you if there is more than one index.

  oldpar <- par()
  tmp <- model$dat
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
  plotBars.fn(ests2$year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab=xlab,ylab=ylab,
              cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6),yaxs='i')
  plotBars.fn(ests$year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=TRUE,cex=1.5,las=1,
              gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
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
  ## SSplotIndices(model, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
  ## ##plotBars.fn(ests2$year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="year",ylab="Biomass Index Estimate (million mt)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
  ## ##plotBars.fn(ests$year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
  ## axis(1, at=model$cpue$Yr[model$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
  ## axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.3)
  ## box()
  ## axis(2, at=(0:5)*1e6, lab=0:5, las=1)
  par <- oldpar
}

make.survey.age1.plot <- function(age1index,
                                  model){
  ## It is assumed that the data file has been read in correctly.
  ## Assumes that there is only one 'CPUE' index and it is the acoustic survey.

  oldpar <- par()
  par(mar=c(4, 4, 1, 1) + 0.1)

  x <- age1index
  yrs <- x$Year

  recr1 <- model$natage[model$natage$Time %in% yrs,"1"]
  recrAll <- model$natage[model$natage$Time %in% min(yrs):max(yrs),"1"]

  logAge1 <- log(recr1)
  logIndex <- log(x$Index)
  mn <- mean(logAge1)
  index <- mn*logIndex/mean(logIndex[!is.na(x$Index)])
  plot(min(yrs):max(yrs),recrAll/1e6,pch=4,type="b",log="y",ylim=range(c(recr1,exp(index)),na.rm=T)*c(1,1)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1,col=gray(0.7),cex=0.8)
  points(yrs,exp(index)/1e6,pch=16,col="blue",cex=1.5)
  points(x$Year[!is.na(x$Index)],recr1/1e6,pch=4,col="black",cex=1,lwd=2)
  #mn <- mean(x$Age.1[!is.na(x$Age1_Index)])
  #index <- exp(mn*x$Age1_Index/mean(x$Age1_Index),na.rm=T)
  #plot(x$Year,x$Age.1/1e6,pch=4,type="b",log="y",ylim=range(c(x$Age.1,index),na.rm=T)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1)
  #points(x$Year,index/1e6,pch=16,col="blue",cex=1.5)
  axis(1,at=x$Year)
  legend("topleft",c("Estimated age-1 recruitment","Scaled acoustic survey age-1 index"),col=c("black","blue"),pch=c(4,16),lty=NA,lwd=2,bty="n")
  par <- oldpar
}

make.survey.biomass.extrap.plot <- function(dat,        ## data.frame of different indices
                                            show=1:3){  ## vector of which values to show

  oldpar <- par()

  ## Remove non-data years from the data frame
  ## no longer needed because we're comparing cases with different range of years
  ## dat <- dat[complete.cases(dat),]

  # values with extrapolation used in base model
  ests <- data.frame(year = dat$year, obs = dat$with.extrap, se_log = dat$cv.with.extrap)
  ests$lo <- exp(log(ests$obs) - 1.96 * ests$se_log)
  ests$hi <- exp(log(ests$obs) + 1.96 * ests$se_log)
  ests$value <- ests$obs

  # without extrapolation
  ests2 <- data.frame(year = dat$year, obs = dat$no.extrap, se_log = dat$cv.no.extrap)
  ests2$lo <- exp(log(ests2$obs) - 1.96 * ests2$se_log)
  ests2$hi <- exp(log(ests2$obs) + 1.96 * ests2$se_log)
  ests2$value <- ests2$obs

  # values used in 2016 assessment
  ests3 <- data.frame(year = dat$year, obs = dat$with.extrap.2016, se_log = dat$cv.2016)
  ests3$lo <- exp(log(ests3$obs) - 1.96 * ests3$se_log)
  ests3$hi <- exp(log(ests3$obs) + 1.96 * ests3$se_log)
  ests3$value <- ests3$obs
  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)

  if(2 %in% show){
    plotBars.fn(ests$year, ests,scalar = 1e3, ylim = c(0, 3), yaxs='i',
                pch = 20, xlab="Year", ylab = "Biomass index estimate (million t)",
                cex = 1.5, las = 1, gap = 0.05, xaxt = "n", ciLwd = 3, ciCol = rgb(0, 0, 0, 0.5))
  }
  if(1 %in% show){
    plotBars.fn(ests3$year - 0.3, ests3, scalar = 1e3, pch = 18, add = TRUE, cex = 1.0,
                las = 1, gap = 0.05, ciLwd = 3, ciCol = rgb(1, 0, 0, 0.5), col = "red")
  }
  if(3 %in% show){
    plotBars.fn(ests2$year + 0.3, ests2, scalar = 1e3, pch = 17, add = TRUE, cex = 1.0,
                las = 1, gap = 0.05, ciLwd = 3, ciCol = rgb(0, 0, 1, 0.5), col = "blue")
  }
  axis(1, at = ests$year, cex.axis = 0.8)
  legend("topleft", c("Values used in 2016 assessment",
                      "Updated time series (with extrapolation, used in base model)",
                      "Updated time series (no extrapolation)")[show],
         col = c("red", "black", "blue")[show], pch = c(18, 16, 17)[show], bty='n')
  par <- oldpar
}

make.kriging.parameters.table <- function(krig.pars = kriging.pars,
                                          xcaption = "default", ## Caption to use
                                          xlabel   = "default", ## Latex label to use
                                          font.size = 9,        ## Size of the font for the table
                                          space.size = 10,       ## Size of the spaces for the table
                                          placement = "H"       ## Placement of table
                                          ){
  ## Returns an xtable in the proper format for the kriging parameters

  krig.pars[1:10,"SearchRadius"] <- f(as.numeric(krig.pars[1:10,"SearchRadius"]), 2)
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- 0
  addtorow$pos[[3]] <- 10
  addtorow$pos[[4]] <- nrow(krig.pars)
  addtorow$command <- c(paste0("\\toprule \n",
                               "Year", "& Search radius", "& $k$\\subscr{min}", "& $k$\\subscr{max} \\\\ \n",
                               "\\midrule \n"),
                        paste0("\\textbf{", last.assess.yr," assessment}\\\\ \n"),
                        paste0("\\textbf{", assess.yr," assessment}\\\\ \n"),
                        "\\bottomrule \n")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")

  return(print(xtable(krig.pars, caption=xcaption, label=xlabel,
               align=c("l","r","c","r","c")),    # first gets ignored
               caption.placement = "top", include.rownames=FALSE,
               include.colnames=FALSE,
               sanitize.text.function=function(x){x},
               size=size.string, add.to.row=addtorow, table.placement=placement,
               hline.after = NULL, booktabs=TRUE))
}
