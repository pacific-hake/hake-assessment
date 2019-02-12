make.numbers.at.age.plot <- function(model){ ## model is an mle run and is the output of the r4ss package's function SS_output
  ## Number-at-age from the MLE run for the model
  SSplotNumbers(model,
                subplot = 1,
                period = "B",
                pwidth = 6.5,
                pheight = 6)
}

make.age.comp.pearson.plot <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                       subplot = 1,            ## 1) fishery or 2) survey
                                       start.yr = min(dat$Yr), ## First year for age comps - default from the data frame
                                       end.yr = max(dat$Yr),   ## Last year for age comps - default from the data frame
                                       show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                       key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                       fg = gray(level=0.1, alpha=0.5),
                                       bg = gray(level=0.5, alpha=0.5),
                                       inches = 0.12
                                       ){
  ## Plot the Pearson residuals for age composition fits for whatever subplot is set to

  oldpar <- par()
  SSplotComps(model,
              kind = "AGE",
              subplot = 24,
              fleetnames = c("Fishery","Survey"))
  par <- oldpar
}

make.fleet.age.comp.pearson.plot <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                             fleet = 1,            ## 1) fishery or 2) survey
                                             fleetName = "Fishery",
                                             start.yr = min(dat$Yr), ## First year for age comps - default from the data frame
                                             end.yr = max(dat$Yr),   ## Last year for age comps - default from the data frame
                                             show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                             key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                             fg = gray(level = 0.1, alpha = 0.5),
                                             bg = gray(level = 0.5, alpha = 0.5),
                                             inches = 0.12,
                                             cohortLines = c(1980,
                                                             1984,
                                                             1999,
                                                             2008,
                                                             2010,
                                                             2014),
                                             cohortCol = rgb(c(0.5, 0, 1, 0, 0),
                                                             c(0, 0.2, 0, 0.8, 1),
                                                             c(1, 0.8, 0.2, 0.1, 0),
                                                             alpha = 0.6),
                                             cohortAdj = 0.5){
  ## Plot the Pearson residuals for age composition fits for whatever fleet is set to

  oldpar <- par()
  SSplotComps(model,
              kind = "AGE",
              subplot = 24,
              fleets=fleet,
              fleetnames = fleetName,
              cohortlines = cohortLines,
              cohortCol=cohortCol,
              cohAdj=cohortAdj)
  par <- oldpar
}

make.age.comp.bubble.plot <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                      subplot = 1,            ## 1) fishery or 2) survey
                                      start.yr = min(dat$Yr), ## First year for age comps - default from the data frame
                                      end.yr = max(dat$Yr),   ## Last year for age comps - default from the data frame
                                      show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                      key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                      fg = gray(level=0.1, alpha=0.5),
                                      bg = gray(level=0.5, alpha=0.5),
                                      cohortlines = c(),      ## Vector of cohorts for which diagonal lines would be drawn
                                      inches = 0.12,
                                      do.plot = TRUE          ## If FALSE, no plot will be drawn, but the return values will be returned
                                      ){
  ## Plot the age compositions for whatever subplot is set to
  ## Returns a vector of the start.yr, end.yr, max. proportion,
  ##  year of max. proportion, age of max. proportion.
  if(do.plot){
    oldpar <- par()
  }
  if(show.key){
    if(is.null(key.yrs)){
      stop("make.age.comp.bubble.plot: Error - you must supply a key.yrs vector of 4 years when specifying show.key = TRUE.\n")
    }else{
      if(length(key.yrs) != 4){
        stop("make.age.comp.bubble.plot: Error - key.yrs must be a vector of exactly 4 years when specifying show.key = TRUE.\n")
      }
    }
    if(do.plot){
      par(mar = c(2.1, 4.1, 3.1, 4.1), cex.axis = 0.9)
    }
  }else{
    if(do.plot){
      par(mar = c(2.1, 4.1, 1.1, 4.1), cex.axis = 0.9)
    }
  }
  dat <- model$dat$agecomp[model$dat$agecomp$FltSvy == subplot,]
  if(end.yr < start.yr){
    stop("make.age.comp.bubble.plot: Error - end.yr cannot be less than start.yr\n")
  }
  ages.str <- names(dat)[grep("^a[0-9]+$", names(dat))]
  ages <- as.numeric(gsub("a", "", ages.str))
  min.age <- min(ages)
  max.age <- max(ages)
  ## Get the maximum proportion and its location within the data
  age.df <- dat[,names(dat) %in% ages.str]
  age.df <- as.matrix(age.df)
  age.vec <- as.numeric(age.df)
  max.prop <- max(age.vec)
  which.max.prop <- which(age.df == max(age.vec), arr.ind = TRUE)
  ## Convert the locations to year and age for return statement
  which.max.prop <- c(dat$Yr[which.max.prop[1]], ages[which.max.prop[2]])

  if(subplot == 1){
    label <- "Fishery ages"
  }else if(subplot == 2){
    label <- "Survey ages"
  }else{
    cat("make.age.comp.fit.plot: Error - subplot must be either 1 or 2.\n\n")
  }
  x <- data.frame(expand.grid(dat$Yr, min.age:max.age),
                  prop = unlist(dat[,ages.str]))
  names(x) <- c("Yr", "Age", "prop")
  if(do.plot){
    symbols(c(as.numeric(as.character(x[,1])), -1),
            c(as.numeric(as.character(x[,2])), -1),
            circles = sqrt(c(as.numeric(as.character(x[,3])), max.prop)),
            inches = inches,
            ylim = c(min.age, max.age),
            xlim = c(as.numeric(start.yr), as.numeric(end.yr)),
            xlab = "",
            ylab = label,
            xaxt = "n",
            fg = fg,
            bg = bg)
    if(show.key){
      symbols(0.2 + c(key.yrs, -1),
              c(16.2, 16.2, 16.2, 16.2, -1),
              circles = sqrt(c(1, 10, 25, 50, max.prop)),
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = fg,
              bg = bg)
      text(key.yrs + 2.2, c(16.2,16.2,16.2,16.2), c("0.01", "0.1", "0.25", "0.5"), xpd = NA, cex = 0.8)
    }
    if(length(cohortlines > 0)){
      for(icohort in 1:length(cohortlines)){
        lines(c(cohortlines[icohort],cohortlines[icohort]+model$accuage),
              c(0,model$accuage),col="red")
      }
    }
    axis(1, seq(as.numeric(start.yr), as.numeric(end.yr) + 5, 5))
    axis(4)
    par <- oldpar
  }
  ret.vec <- c(as.numeric(start.yr), as.numeric(end.yr), max.prop, which.max.prop)
  names(ret.vec) <- c("start.yr", "end.yr", "max.prop", "max.prop.yr", "max.prop.age")
  return(ret.vec)
}

make.age.comp.compare.bubble.plot <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                              start.yr = min(d1$Yr, d2$Yr), ## First year for age comps - default from the data frame
                                              end.yr = max(d1$Yr, d2$Yr),   ## Last year for age comps - default from the data frame
                                              show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                              key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                              inches = 0.12,
                                              opacity = 80            ## Allows for transparency
                                              ){
  ## Plot the age compositions for fishery and survey overlaid
  oldpar <- par()
  if(show.key){
    if(is.null(key.yrs)){
      stop("make.age.comp.bubble.plot: Error - you must supply a key.yrs vector of 4 years when specifying show.key = TRUE.\n")
    }else{
      if(length(key.yrs) != 4){
        stop("make.age.comp.bubble.plot: Error - key.yrs must be a vector of exactly 4 years when specifying show.key = TRUE.\n")
      }
    }
    par(mar = c(2.1, 4.1, 3.1, 4.1), oma = c(1.1, 1.1, 0, 0), cex.axis = 0.9)
  }else{
    par(mar = c(2.1, 4.1, 1.1, 4.1), oma = c(1.1, 1.1, 0, 0), cex.axis = 0.9)
  }
  d1 <- model$dat$agecomp[model$dat$agecomp$FltSvy == 1,]
  d2 <- model$dat$agecomp[model$dat$agecomp$FltSvy == 2,]
  survey.yrs <- d2$Yr
  if(end.yr < start.yr){
    stop("make.age.comp.bubble.plot: Error - end.yr cannot be less than start.yr\n")
  }
  for(i in 2:1){
    dat <- model$dat$agecomp[model$dat$agecomp$FltSvy == i,]
    if(i == 1){
      dat <- dat[dat$Yr %in% survey.yrs,]
    }
    ages.str <- names(dat)[grep("^a[0-9]+$", names(dat))]
    ages <- as.numeric(gsub("a", "", ages.str))
    min.age <- min(ages)
    max.age <- max(ages)
    ## Get the maximum proportion and its location within the data
    age.df <- dat[,names(dat) %in% ages.str]
    max.prop <- max(age.df)
    which.max.prop <- which(age.df == max(age.df), arr.ind = TRUE)
    ## Convert the locations to year and age for return statement
    which.max.prop <- c(dat$Yr[which.max.prop[1]], ages[which.max.prop[2]])

    x <- data.frame(expand.grid(dat$Yr, min.age:max.age),
                    prop = unlist(dat[,ages.str]))
    names(x) <- c("Yr", "Age", "prop")
    symbols(c(x[,1], -1),
            c(x[,2], -1),
            circles = sqrt(c(x[,3], max.prop)),
            inches = inches,
            ylim = c(min.age, max.age),
            xlim = c(start.yr, end.yr),
            xlab = "",
            ylab = "",
            xaxt = "n",
            add = if(i == 2) FALSE else TRUE,
            fg = if(i == 2) get.shade("darkblue", opacity + 10) else get.shade("darkred", opacity),
            bg = if(i == 2) get.shade("blue", opacity + 10) else get.shade("red", opacity))
    if(i == 2 && show.key){
      symbols(0.2 + c(key.yrs, -1),
              c(16.2, 16.2, 16.2, 16.2, -1),
              circles = sqrt(c(1, 10, 25, 50, max.prop)),
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = get.shade("darkblue", opacity + 10),
              bg = get.shade("darkred", opacity))
      text(key.yrs + 1.1, c(16.2,16.2,16.2,16.2), c("0.01", "0.1", "0.25", "0.5"), xpd = NA, cex = 0.8)
      ## Fishery dot
      symbols(2009.2,
              16.2,
              circles = 0.05,
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = "darkred",
              bg = "red")
      text(2009.6 + 1.1, 16.2, "Fishery", xpd = NA, cex = 0.8)
      ## Survey dot
      symbols(2013.2,
              16.2,
              circles = 0.05,
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = "darkblue",
              bg = "blue")
      text(2013.6 + 1.1, 16.2, "Survey", xpd = NA, cex = 0.8)
    }
  }
  axis(1, at = survey.yrs, labels = survey.yrs)
  axis(4)
  mtext("Year", side = 1, outer = TRUE)
  mtext("Age", side = 2, line = -1, outer = TRUE)
  par <- oldpar
}

make.age.comp.fit.plot <- function(model,       ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                   subplot = 1  ## 1) fishery or 2) survey
                                   ){
  ## Plot the age compositions for whatever subplot is set to
  oldpar <- par()
  if(subplot == 1){
    ncol <- 4
    f <- 1
    label <- "Fishery age composition"
  }else if(subplot == 2){
    ncol <- 1
    f <- 2
    label <- "Survey age composition"
  }else{
    cat("make.age.comp.fit.plot: Error - subplot must be either 1 or 2.\n\n")
  }
  age.fits(dat = model$extra.mcmc,
           ncol = ncol,
           f = f,
           uncertainty = TRUE,
           title.text = label,
           legend = FALSE,
           start.color = 1)
  par <- oldpar
}

age.fits <- function(dat,
                     case_label = "",
                     f = 1,
                     ncol = 3,
                     start.color = 1,
                     title.text = "Fishery age composition data",
                     legend = TRUE,
                     uncertainty = FALSE,
                     verbose = FALSE) {
  ## makes a nice colored bar plot of the age comps for all years.
  agedbase <- dat$agedbase[dat$agedbase$Fleet==f,]
  if(verbose){
    print(names(agedbase))
  }

  if(uncertainty & !"Exp.025" %in% names(agedbase)){
    cat("setting uncertainty=FALSE because intervals for expected values not found\n")
    uncertainty <- FALSE
  }

  first.year <- 1974
  subtle.color <- "gray40"
  ages <- c(1,15) #age range
  ages.list <- ages[1]:ages[2]
  #print(ages.list)
  years <- sort(unique(agedbase$Yr))
  # create data frames of observed and predicted values based on SS output
  obs.data <- NULL
  pred.data <- NULL
  pred.data.025 <- NULL
  pred.data.975 <- NULL
  for(iyr in 1:length(years)){
    obs.data <- rbind(obs.data, agedbase$Obs[agedbase$Yr==years[iyr]])
    pred.data <- rbind(pred.data, agedbase$Exp[agedbase$Yr==years[iyr]])
    if(uncertainty){
      pred.data.025 <- rbind(pred.data.025, agedbase$Exp.025[agedbase$Yr==years[iyr]])
      pred.data.975 <- rbind(pred.data.975, agedbase$Exp.975[agedbase$Yr==years[iyr]])
    }
  }
  # current year
  cyear <- as.numeric(format(Sys.Date(), "%Y"))
  years1 <- seq(first.year,cyear)
  #print(dim(obs.data))
  nyears <- length(years)
  nyears1 <- cyear-first.year

  nages <- length(ages.list)
  mfcol <- c(ceiling(nyears/ncol),ncol)
  mfcol1<- c(ceiling(nyears1/ncol),ncol)
  par(mfcol = mfcol, oma = c(3.5, 4.5, 3.5, 1), mar = c(0,0,0,0))
  #cohort.color <- rainbow(mfcol1[1]+min(10,nages))[-c(1:2)]   #use hideous rainbow colors because they loop more gracefully than rich.colors
  # overriding cohort.color setting above to make it constant across fleets
  cohort.color <- rainbow(22)[-c(1:2)]
  # make starting color the same independent of data years
  #start.color <- (years1[1]%%1950)%%length(cohort.color)
  if(start.color > length(cohort.color)){
    stop("start.color should be less than ",length(cohort.color))
  }
  cohort.color <- cohort.color[c(start.color:length(cohort.color), 1:(start.color-1))]
  cohort.colors <- matrix(ncol=nyears1,nrow=length(cohort.color))
  cohort.colors <- data.frame(cohort.colors)
  ncolors <- length(cohort.color)
  if(verbose){
    cat("ncolors: ", ncolors, "\n")
  }
  for(i in 1:nyears1){
    cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])
    cohort.colors[,i] <- cohort.color
  }

  ylim <- c(0,1.05*max(obs.data,pred.data))
  if(uncertainty){
    ylim <- c(0,1.05*max(obs.data,pred.data.975))
  }
  for (yr in 1:nyears) {
    year1<-years[yr]
    names.arg <- rep("",nages)
    x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=subtle.color,
                 col=cohort.colors[1:nages,year1-first.year],axes=F,ylab="",xlab="")
    if (yr %% mfcol[1] == 0) {
      ## axis(side=1,at=x,lab=ages.list, line=-0.1,col.axis=subtle.color,
      ##      col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      ## # adding axes manually
      axis(side=1,at=x, lab=1:15, line=-0.1,col.axis=subtle.color,
           col=subtle.color,lwd=0.5,lwd.ticks=0.5)  #just use for the labels, to allow more control than names.arg
      axis(side=1,at=x[(1:15)%%2==0], lab=(1:15)[(1:15)%%2==0], line=-0.1,col.axis=subtle.color,
           col=subtle.color,lwd=0.5,lwd.ticks=0.5)  #just use for the labels, to allow more control than names.arg
    }
    if (yr <= mfcol[1]) {
      axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
    }
    par(new=T)
    plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",
         bg="white",fg="brown", type='n',axes=F,ylab="",xlab="")
    if(legend & par()$mfg[2]==par()$mfg[4] & par()$mfg[1]==1){
      par(xpd=NA)
      legend(x=0,y=1.6*ylim[2], legend=c("Observed proportion", "Expected proportion with 95% interval"),
             pch=c(22,23), pt.cex=c(2,1), col=c(subtle.color,1), pt.bg=c(cohort.color[1],"white"), bty='n')
      par(xpd=FALSE)
    }
    #segments(x0=x-.3, x1=x+.3, y0=pred.data[yr,], y1=pred.data[yr,], lwd=3)
    #segments(x0=x, x1=x, y0=0, y1=pred.data[yr,], lwd=1)
    ## rect(xleft=x-.2, xright=x+.2, ybottom=0, ytop=pred.data[yr,], lwd=1,
    ##      col=gray(1,alpha=.5))
    if(uncertainty){
      arrows(x0=x, x1=x,
             y0=pred.data.025[yr,], y1=pred.data.975[yr,],
             #y0=0, y1=pred.data,
             angle=90, length=0.02, code=3)
    }
    points(x=x,y=pred.data[yr,], pch=23, cex=.8, bg="white",lwd=1)
    box(col=subtle.color,lwd=0.5)
    x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
    y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
    text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
    par(xpd=T)
    ## rect(xleft=x-.2, xright=x+.2, ybottom=0, ytop=pred.data[yr,], lwd=1,
    ##     col=gray(1,alpha=.5))
    abline(h=0, col=subtle.color)
  }
  mtext(side=1,outer=T,"Age",line=2)
  mtext(side=2,outer=T,"Proportion",line=3.2)
  mtext(side=3,outer=T,line=1.2,title.text)
  if(nchar(case_label)>0){
    mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
  }
}
