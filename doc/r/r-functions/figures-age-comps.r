make.numbers.at.age.plot <- function(model){ ## model is an mle run and is the output of the r4ss package's function SS_output
  ## Number-at-age from the MLE run for the model
  SSplotNumbers(model,
                subplot = 1,
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
  SSplotComps(model$extra.mcmc,
              kind = "AGE",
              subplot = 24,
              printmkt = FALSE,
              printsex = FALSE,
              main = "",
              fleetnames = c("Fishery","Survey"))
  par <- oldpar
}

make.fleet.age.comp.pearson.plot <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                             fleet = 1,            ## 1) fishery or 2) survey
                                             fleetName = "Fishery",
                                             start.yr = min(dat$Yr), ## First year for age comps - default from the data frame
                                             end.yr = max(dat$Yr),   ## Last year for age comps - default from the data frame
                                             show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                             key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                             fg = gray(level=0.1, alpha=0.5),
                                             bg = gray(level=0.5, alpha=0.5),
                                             inches = 0.12,
                                             cohortLines=c(1980,1984,1999,2008,2010),
                                             cohortCol=rgb(c(0.5,0,1,0,0),c(0,0.2,0,0.8,1),c(1,0.8,0.2,0.1,0),alpha=0.6),
                                             cohortAdj=0.5
                                             ){
  ## Plot the Pearson residuals for age composition fits for whatever fleet is set to

  oldpar <- par()
  SSplotComps(model,
              kind = "AGE",
              subplot = 24,
              printmkt = FALSE,
              printsex = FALSE,
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
  max.prop <- max(age.df)
  which.max.prop <- which(age.df == max(age.df), arr.ind = TRUE)
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
    symbols(c(x[,1], -1),
            c(x[,2], -1),
            circles = sqrt(c(x[,3], max.prop)),
            inches = inches,
            ylim = c(min.age, max.age),
            xlim = c(start.yr, end.yr),
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
      text(key.yrs + 1.1, c(16.2,16.2,16.2,16.2), c("0.01", "0.1", "0.25", "0.5"), xpd = NA, cex = 0.8)
    }
    axis(1, seq(start.yr, end.yr + 5, 5))
    axis(4)
    par <- oldpar
  }
  ret.vec <- c(start.yr, end.yr, max.prop, which.max.prop)
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

## HACK alert.. I had to add the r4ss source for SSplotComps and bubble3 because there is
##  no obvious way to not have the title print, and when it does, it has an ugly format
## with two commas together. This should be fixed when there is more time.

SSplotComps <- function (replist, subplots = c(1:21, 24), kind = "LEN", sizemethod = 1, 
    aalyear = -1, aalbin = -1, plot = TRUE, print = FALSE, fleets = "all", 
    fleetnames = "default", sexes = "all", yupper = 0.4, datonly = FALSE, 
    samplesizeplots = TRUE, compresidplots = TRUE, bub = FALSE, 
    showyears = TRUE, showsampsize = TRUE, showeffN = TRUE, sampsizeline = FALSE, 
    effNline = FALSE, minnbubble = 3, pntscalar = NULL, scalebubbles = FALSE, 
    cexZ1 = 1.5, bublegend = TRUE, colvec = c(rgb(1, 0, 0, 0.7), 
        rgb(0, 0, 1, 0.7), rgb(0.1, 0.1, 0.1, 0.7)), linescol = c(rgb(0, 
        0.5, 0, 0.7), rgb(0.8, 0, 0, 0.7), rgb(0, 0, 0.8, 0.7)), 
    axis1 = NULL, axis2 = NULL, blue = rgb(0, 0, 1, 0.7), red = rgb(1, 
        0, 0, 0.7), pwidth = 6.5, pheight = 5, punits = "in", 
    ptsize = 10, res = 300, plotdir = "default", cex.main = 1, 
    linepos = 1, fitbar = FALSE, do.sqrt = TRUE, smooth = TRUE, 
    cohortlines = c(), labels = c("Length (cm)", "Age (yr)", 
        "Year", "Observed sample size", "Effective sample size", 
        "Proportion", "cm", "Frequency", "Weight", "Length", 
        "(mt)", "(numbers x1000)", "Stdev (Age) (yr)", "Conditional AAL plot, "), 
    printmkt = TRUE, printsex = TRUE, maxrows = 6, maxcols = 6, 
    maxrows2 = 2, maxcols2 = 4, rows = 1, cols = 1, andre_oma = c(3, 
        0, 3, 0), andrerows = 3, fixdims = TRUE, fixdims2 = FALSE, 
    maxneff = 5000, verbose = TRUE, scalebins = FALSE, addMeans = TRUE, 
    ...) 
{
    if (!exists("make_multifig")) 
        stop("you are missing the function 'make_mulitifig'")
    pngfun <- function(file, caption = NA) {
        png(filename = file, width = pwidth, height = pheight, 
            units = punits, res = res, pointsize = ptsize)
        plotinfo <- rbind(plotinfo, data.frame(file = file, caption = caption))
        return(plotinfo)
    }
    plotinfo <- NULL
    SS_versionNumeric <- replist$SS_versionNumeric
    lendbase <- replist$lendbase
    sizedbase <- replist$sizedbase
    agedbase <- replist$agedbase
    condbase <- replist$condbase
    ghostagedbase <- replist$ghostagedbase
    ghostlendbase <- replist$ghostlendbase
    ladbase <- replist$ladbase
    wadbase <- replist$wadbase
    tagdbase1 <- replist$tagdbase1
    tagdbase2 <- replist$tagdbase2
    nfleets <- replist$nfleets
    nseasons <- replist$nseasons
    seasfracs <- replist$seasfracs
    FleetNames <- replist$FleetNames
    nsexes <- replist$nsexes
    accuage <- replist$accuage
    Age_tuning <- replist$Age_comp_Eff_N_tuning_check
    titles <- NULL
    titlemkt <- ""
    if (plotdir == "default") {
        plotdir <- replist$inputs$dir
    }
    if (fleets[1] == "all") {
        fleets <- 1:nfleets
    }
    else {
        if (length(intersect(fleets, 1:nfleets)) != length(fleets)) {
            stop("Input 'fleets' should be 'all' or a vector of values between 1 and nfleets.")
        }
    }
    if (fleetnames[1] == "default") {
        fleetnames <- FleetNames
    }
    if (sexes[1] == "all") {
        sexes <- 0:nsexes
    }
    if (nsexes == 1) {
        sexes <- 0:nsexes
    }
    if (nsexes == 1 | length(sexes) > 1) {
        titlesex <- ""
        filesex <- ""
    }
    if (nsexes > 1 & length(sexes) == 1) {
        if (sexes == 0) {
            titlesex <- "sexes combined, "
            filesex <- "sex0"
        }
        if (sexes == 1) {
            titlesex <- "female, "
            filesex <- "sex1"
        }
        if (sexes == 2) {
            titlesex <- "male, "
            filesex <- "sex2"
        }
    }
    titlesex <- ifelse(printsex, titlesex, "")
    if (kind == "LEN") {
        dbase_kind <- lendbase
        kindlab = labels[1]
        if (datonly) {
            filenamestart <- "comp_lendat_"
            titledata <- "length comp data, "
        }
        else {
            filenamestart <- "comp_lenfit_"
            titledata <- "length comps, "
        }
    }
    if (kind == "GSTLEN") {
        dbase_kind <- ghostlendbase
        kindlab = labels[1]
        if (datonly) {
            filenamestart <- "comp_gstlendat_"
            titledata <- "ghost length comp data, "
        }
        else {
            filenamestart <- "comp_gstlenfit_"
            titledata <- "ghost length comps, "
        }
    }
    if (kind == "SIZE") {
        dbase_kind <- sizedbase[sizedbase$method == sizemethod, 
            ]
        sizeunits <- unique(dbase_kind$units)
        if (length(sizeunits) > 1) 
            stop("!error with size units in generalized size comp plots:\n", 
                "    more than one unit value per method.\n")
        if (sizeunits %in% c("in", "cm")) 
            kindlab <- paste(labels[21], " (", sizeunits, ")", 
                sep = "")
        if (sizeunits %in% c("lb", "kg")) 
            kindlab <- paste(labels[9], " (", sizeunits, ")", 
                sep = "")
        if (datonly) {
            filenamestart <- "comp_sizedat_"
            titledata <- "size comp data, "
        }
        else {
            filenamestart <- "comp_sizefit_"
            titledata <- "size comps, "
        }
    }
    if (kind == "AGE") {
        dbase_kind <- agedbase
        kindlab = labels[2]
        if (datonly) {
            filenamestart <- "comp_agedat_"
            titledata <- "age comp data, "
        }
        else {
            filenamestart <- "comp_agefit_"
            titledata <- "age comps, "
        }
    }
    if (kind == "cond") {
        dbase_kind <- condbase
        kindlab = labels[2]
        if (datonly) {
            filenamestart <- "comp_condAALdat_"
            titledata <- "conditional age-at-length data, "
        }
        else {
            filenamestart <- "comp_condAALfit_"
            titledata <- "conditional age-at-length, "
        }
    }
    if (kind == "GSTAGE") {
        dbase_kind <- ghostagedbase
        kindlab = labels[2]
        if (datonly) {
            filenamestart <- "comp_gstagedat_"
            titledata <- "ghost age comp data, "
        }
        else {
            filenamestart <- "comp_gstagefit_"
            titledata <- "ghost age comps, "
        }
    }
    if (kind == "GSTcond") {
        dbase_kind <- ghostagedbase
        kindlab = labels[2]
        if (datonly) {
            filenamestart <- "comp_gstCAALdat_"
            titledata <- "ghost conditional age-at-length data, "
        }
        else {
            filenamestart <- "comp_gstCAALfit_"
            titledata <- "ghost conditional age-at-length comps, "
        }
    }
    if (kind == "L@A") {
        dbase_kind <- ladbase[ladbase$N != 0, ]
        kindlab = labels[2]
        filenamestart <- "comp_LAAfit_"
        titledata <- "mean length at age, "
        dbase_kind$SD <- dbase_kind$Lbin_lo/dbase_kind$N
    }
    if (kind == "W@A") {
        dbase_kind <- wadbase[wadbase$N != 0, ]
        kindlab = labels[2]
        filenamestart <- "comp_WAAfit_"
        titledata <- "mean weight at age, "
    }
    if (!(kind %in% c("LEN", "SIZE", "AGE", "cond", "GSTAGE", 
        "GSTLEN", "L@A", "W@A"))) {
        stop("Input 'kind' to SSplotComps is not right.")
    }
    if (any(dbase_kind$SuprPer == "Sup" & dbase_kind$Used == 
        "skip")) {
        cat("Note: removing super-period composition values labeled 'skip'\n", 
            "     and designating super-period values with a '*'\n")
        dbase_kind <- dbase_kind[dbase_kind$SuprPer == "No" | 
            dbase_kind$Used != "skip", ]
        dbase_kind$YrSeasName <- paste(dbase_kind$YrSeasName, 
            ifelse(dbase_kind$SuprPer == "Sup", "*", ""), sep = "")
    }
    ageerr_warning <- TRUE
    dbase_kind <- dbase_kind[dbase_kind$sex %in% sexes, ]
    for (f in fleets) {
        if (length(dbase_kind$Obs[dbase_kind$Fleet == f]) > 0) {
            dbasef <- dbase_kind[dbase_kind$Fleet == f, ]
            if (kind %in% c("cond", "GSTcond") && f %in% Age_tuning$Fleet) {
                HarmEffNage <- NULL
                MeanNage <- NULL
            }
            else {
                HarmEffNage <- NULL
                MeanNage <- NULL
            }
            dbase_k <- dbasef
            for (j in unique(dbase_k$Part)) {
                dbase <- dbase_k[dbase_k$Part == j, ]
                max_n_ageerr <- max(apply(table(dbase$Yr.S, dbase$Ageerr) > 
                  0, 1, sum))
                if (max_n_ageerr > 1) {
                  if (ageerr_warning) {
                    cat("Note: multiple samples with different ageing error types within fleet/year.\n", 
                      "     Plots label '2005a3' indicates ageing error type 3 for 2005 sample.\n", 
                      "     Bubble plots may be misleading with overlapping bubbles.\n")
                    ageerr_warning <- FALSE
                  }
                  dbase$Yr.S <- dbase$Yr.S + dbase$Ageerr/1000
                  dbase$YrSeasName <- paste(dbase$YrSeasName, 
                    "a", dbase$Ageerr, sep = "")
                }
                if (j == 0) 
                  titlemkt <- "whole catch, "
                if (j == 1) 
                  titlemkt <- "discard, "
                if (j == 2) 
                  titlemkt <- "retained, "
                titlemkt <- ifelse(printmkt, titlemkt, "")
                if (datonly | fitbar) 
                  bars <- TRUE
                else bars <- FALSE
                title_sexmkt <- paste(titlesex, titlemkt, sep = "")
                filename_fltsexmkt <- paste("flt", f, filesex, 
                  "mkt", j, sep = "")
                if (1 %in% subplots & kind != "cond") {
                  ptitle <- paste(titledata, title_sexmkt, fleetnames[f], 
                    sep = "")
                  titles <- c(ptitle, titles)
                  tempfun <- function(ipage, ...) {
                    sexvec <- dbase$sex
                    if (!(kind %in% c("GSTAGE", "GSTLEN", "L@A", 
                      "W@A"))) {
                      make_multifig(ptsx = dbase$Bin, ptsy = dbase$Obs, 
                        yr = dbase$Yr.S, linesx = dbase$Bin, 
                        linesy = dbase$Exp, sampsize = dbase$N, 
                        effN = dbase$effN, showsampsize = showsampsize, 
                        showeffN = showeffN, bars = bars, linepos = (1 - 
                          datonly) * linepos, nlegends = 3, legtext = list(dbase$YrSeasName, 
                          "sampsize", "effN"), main = ptitle, 
                        cex.main = cex.main, xlab = kindlab, 
                        ylab = labels[6], maxrows = maxrows, 
                        maxcols = maxcols, rows = rows, cols = cols, 
                        fixdims = fixdims, ipage = ipage, scalebins = scalebins, 
                        colvec = colvec, linescol = linescol, 
                        axis1 = axis1, axis2 = axis2, sexvec = sexvec, 
                        yupper = yupper, ...)
                    }
                    if (kind == "GSTAGE") {
                      make_multifig(ptsx = dbase$Bin, ptsy = dbase$Obs, 
                        yr = dbase$Yr.S, linesx = dbase$Bin, 
                        linesy = dbase$Exp, sampsize = dbase$N, 
                        effN = dbase$effN, showsampsize = FALSE, 
                        showeffN = FALSE, bars = bars, linepos = (1 - 
                          datonly) * linepos, nlegends = 3, legtext = list(dbase$YrSeasName, 
                          "sampsize", "effN"), main = ptitle, 
                        cex.main = cex.main, xlab = kindlab, 
                        ylab = labels[6], maxrows = maxrows, 
                        maxcols = maxcols, rows = rows, cols = cols, 
                        fixdims = fixdims, ipage = ipage, scalebins = scalebins, 
                        colvec = colvec, linescol = linescol, 
                        axis1 = axis1, axis2 = axis2, sexvec = sexvec, 
                        yupper = yupper, ...)
                    }
                    if (kind == "GSTLEN") {
                      make_multifig(ptsx = dbase$Bin, ptsy = dbase$Obs, 
                        yr = dbase$Yr.S, linesx = dbase$Bin, 
                        linesy = dbase$Exp, sampsize = dbase$N, 
                        effN = dbase$effN, showsampsize = FALSE, 
                        showeffN = FALSE, bars = bars, linepos = (1 - 
                          datonly) * linepos, nlegends = 3, legtext = list(dbase$YrSeasName, 
                          "sampsize", "effN"), main = ptitle, 
                        cex.main = cex.main, xlab = kindlab, 
                        ylab = labels[6], maxrows = maxrows, 
                        maxcols = maxcols, rows = rows, cols = cols, 
                        fixdims = fixdims, ipage = ipage, scalebins = scalebins, 
                        colvec = colvec, linescol = linescol, 
                        axis1 = axis1, axis2 = axis2, sexvec = sexvec, 
                        ...)
                    }
                    if (kind %in% c("L@A", "W@A")) {
                      make_multifig(ptsx = dbase$Bin, ptsy = dbase$Obs, 
                        yr = dbase$Yr.S, linesx = dbase$Bin, 
                        linesy = dbase$Exp, ptsSD = dbase$SD, 
                        sampsize = dbase$N, effN = 0, showsampsize = FALSE, 
                        showeffN = FALSE, nlegends = 1, legtext = list(dbase$YrSeasName), 
                        bars = bars, linepos = (1 - datonly) * 
                          linepos, main = ptitle, cex.main = cex.main, 
                        xlab = kindlab, ylab = ifelse(kind == 
                          "W@A", labels[9], labels[1]), maxrows = maxrows, 
                        maxcols = maxcols, rows = rows, cols = cols, 
                        fixdims = fixdims, ipage = ipage, scalebins = scalebins, 
                        colvec = colvec, linescol = linescol, 
                        axis1 = axis1, axis2 = axis2, sexvec = sexvec, 
                        ...)
                    }
                  }
                  if (plot) 
                    tempfun(ipage = 0, ...)
                  if (print) {
                    npages <- ceiling(length(unique(dbase$Yr.S))/maxrows/maxcols)
                    for (ipage in 1:npages) {
                      caption <- ptitle
                      pagetext <- ""
                      if (npages > 1) {
                        pagetext <- paste("_page", ipage, sep = "")
                        caption <- paste(caption, " (plot ", 
                          ipage, " of ", npages, ")", sep = "")
                      }
                      file <- paste(plotdir, "/", filenamestart, 
                        filename_fltsexmkt, pagetext, ".png", 
                        sep = "")
                      plotinfo <- pngfun(file = file, caption = caption)
                      tempfun(ipage = ipage, ...)
                      dev.off()
                    }
                  }
                }
                if (datonly) {
                  z <- dbase$Obs
                  if (scalebubbles) {
                    z <- dbase$N * dbase$Obs
                  }
                  col <- rep("black", 2)
                  titletype <- titledata
                  filetype <- "bub"
                  allopen <- TRUE
                }
                else {
                  z <- dbase$Pearson
                  col <- rep(colvec[3], 2)
                  ##titletype <- "Pearson residuals, "
                  titletype <- ""
                  filetype <- "resids"
                  allopen <- FALSE
                }
                if (2 %in% subplots & bub & kind != "cond") {
                  if (length(cohortlines) > 0) {
                    growdat <- replist$endgrowth
                    growdatF <- growdat[growdat$Gender == 1 & 
                      growdat$Morph == min(growdat$Morph[growdat$Gender == 
                        1]), ]
                    if (nsexes > 1) {
                      growdatM <- growdat[growdat$Gender == 2 & 
                        growdat$Morph == min(growdat$Morph[growdat$Gender == 
                          2]), ]
                    }
                  }
                  ptitle <- paste(titletype, title_sexmkt, fleetnames[f], 
                    sep = "")
                  ptitle <- paste(ptitle, " (max=", round(max(z), 
                    digits = 2), ")", sep = "")
                  titles <- c(ptitle, titles)
                  tempfun2 <- function() {
                    xvals <- dbase$Yr.S
                    xdiff <- 0.1 * sort(unique(diff(sort(unique(dbase$Yr.S)))))[1]
                    if (is.na(xdiff)) {
                      xdiff <- 1
                    }
                    cols <- rep(colvec[3], nrow(dbase))
                    if (nsexes > 1) {
                      xvals[dbase$sex > 0] <- dbase$Yr.S[dbase$sex > 
                        0] - (dbase$sex[dbase$sex > 0] - 1.5) * 
                        xdiff
                      cols[dbase$sex > 0] <- colvec[dbase$sex[dbase$sex > 
                        0]]
                    }
                    bubble3(x = xvals, y = dbase$Bin, z = z, 
                      xlab = labels[3], ylab = kindlab, col = cols, 
                      cexZ1 = cexZ1, legend = bublegend, las = 1, 
                      main = ptitle, cex.main = cex.main, maxsize = pntscalar, 
                      allopen = allopen, minnbubble = minnbubble)
                    if (length(cohortlines) > 0) {
                      for (icohort in 1:length(cohortlines)) {
                        cat("  Adding line for", cohortlines[icohort], 
                          "cohort\n")
                        if (kind == "LEN") {
                          if (nsexes > 1) {
                            lines(growdatF$Age + cohortlines[icohort], 
                              growdatF$Len_Mid, col = colvec[1])
                            lines(growdatM$Age + cohortlines[icohort], 
                              growdatM$Len_Mid, col = colvec[2])
                          }
                          else {
                            lines(growdatF$Age + cohortlines[icohort], 
                              growdatF$Len_Mid, col = colvec[3])
                          }
                        }
                        if (kind %in% c("AGE", "GSTAGE")) {
                          lines(c(cohortlines[icohort], cohortlines[icohort] + 
                            accuage), c(0, accuage), col = colvec[3], 
                            lty = 3)
                        }
                      }
                    }
                  }
                  if (plot) 
                    tempfun2()
                  if (print) {
                    caption <- ptitle
                    pagetext <- ""
                    if (npages > 1) {
                      pagetext <- paste("_page", ipage, sep = "")
                      caption <- paste(caption, " (plot ", ipage, 
                        " of ", npages, ")", sep = "")
                    }
                    if (length(grep("Pearson", caption)) > 0) {
                      caption <- paste(caption, "<br> \nClosed bubbles are positive residuals", 
                        "(observed > expected)", "and open bubbles are negative residuals", 
                        "(observed < expected).")
                    }
                    file <- paste(plotdir, "/", filenamestart, 
                      filetype, filename_fltsexmkt, pagetext, 
                      ".png", sep = "")
                    plotinfo <- pngfun(file = file, caption = caption)
                    tempfun2()
                    dev.off()
                  }
                }
                if (3 %in% subplots & kind == "cond") {
                  ptitle <- paste(titletype, title_sexmkt, fleetnames[f], 
                    sep = "")
                  ptitle <- paste(ptitle, " (max=", round(max(z), 
                    digits = 2), ")", sep = "")
                  titles <- c(ptitle, titles)
                  sampsizeline.old <- sampsizeline
                  effNline.old <- effNline
                  if (is.logical(sampsizeline) && sampsizeline) {
                    sampsizeline <- max(dbase$Bin)/max(dbase$N, 
                      na.rm = TRUE)
                    if (!datonly && is.logical(effNline) && effNline) {
                      sampsizeline <- effNline <- max(dbase$Bin)/max(dbase$N, 
                        dbase$effN, na.rm = TRUE)
                      cat("  Fleet ", f, " ", titlesex, "adj. input & effective N in red & green scaled by ", 
                        effNline, "\n", sep = "")
                    }
                    else {
                      cat("  Fleet ", f, " ", titlesex, "adj. input N in red scaled by ", 
                        sampsizeline, "\n", sep = "")
                    }
                  }
                  tempfun3 <- function(ipage, ...) {
                    sexvec <- dbase$sex
                    col.index <- sexvec
                    col.index[col.index == 0] <- 3
                    cols <- colvec[col.index]
                    yrvec <- dbase$Yr.S + dbase$sex * 1e-06
                    make_multifig(ptsx = dbase$Bin, ptsy = dbase$Lbin_mid, 
                      yr = yrvec, size = z, sampsize = dbase$N, 
                      showsampsize = showsampsize, effN = dbase$effN, 
                      showeffN = FALSE, cexZ1 = cexZ1, bublegend = bublegend, 
                      nlegends = 1, legtext = list(dbase$YrSeasName), 
                      bars = FALSE, linepos = 0, main = ptitle, 
                      cex.main = cex.main, xlab = labels[2], 
                      ylab = labels[1], ymin0 = FALSE, maxrows = maxrows2, 
                      maxcols = maxcols2, fixdims = fixdims, 
                      allopen = allopen, minnbubble = minnbubble, 
                      ptscol = cols, ipage = ipage, scalebins = scalebins, 
                      sampsizeline = sampsizeline, effNline = effNline, 
                      sampsizemean = MeanNage, effNmean = HarmEffNage, 
                      colvec = colvec, linescol = linescol, axis1 = axis1, 
                      axis2 = axis2, sexvec = sexvec, ...)
                  }
                  if (plot) 
                    tempfun3(ipage = 0, ...)
                  if (print) {
                    npages <- ceiling(length(unique(dbase$Yr.S)) * 
                      length(unique(dbase$sex))/maxrows2/maxcols2)
                    for (ipage in 1:npages) {
                      caption <- ptitle
                      pagetext <- ""
                      if (npages > 1) {
                        pagetext <- paste("_page", ipage, sep = "")
                        caption <- paste(caption, " (plot ", 
                          ipage, " of ", npages, ")", sep = "")
                      }
                      file <- paste(plotdir, "/", filenamestart, 
                        filetype, filename_fltsexmkt, pagetext, 
                        ".png", sep = "")
                      plotinfo <- pngfun(file = file, caption = caption)
                      tempfun3(ipage = ipage, ...)
                      dev.off()
                    }
                  }
                  sampsizeline <- sampsizeline.old
                  effNline <- effNline.old
                }
                if ((4 %in% subplots | 5 %in% subplots) & aalyear[1] > 
                  0 & kind == "cond") {
                  for (y in 1:length(aalyear)) {
                    aalyr <- aalyear[y]
                    if (length(dbase$Obs[dbase$Yr == aalyr]) > 
                      0) {
                      if (4 %in% subplots) {
                        ptitle <- paste(aalyr, " age-at-length bin, ", 
                          title_sexmkt, fleetnames[f], sep = "")
                        titles <- c(ptitle, titles)
                        ydbase <- dbase[dbase$Yr == aalyr, ]
                        lenbinlegend <- paste(ydbase$Lbin_lo, 
                          labels[7], sep = "")
                        lenbinlegend[ydbase$Lbin_range > 0] <- paste(ydbase$Lbin_lo, 
                          "-", ydbase$Lbin_hi, labels[7], sep = "")
                        tempfun4 <- function(ipage, ...) {
                          make_multifig(ptsx = ydbase$Bin, ptsy = ydbase$Obs, 
                            yr = ydbase$Lbin_lo, linesx = ydbase$Bin, 
                            linesy = ydbase$Exp, sampsize = ydbase$N, 
                            effN = ydbase$effN, showsampsize = showsampsize, 
                            showeffN = showeffN, nlegends = 3, 
                            legtext = list(lenbinlegend, "sampsize", 
                              "effN"), bars = FALSE, linepos = linepos, 
                            main = ptitle, cex.main = cex.main, 
                            xlab = labels[2], ylab = labels[6], 
                            maxrows = maxrows, maxcols = maxcols, 
                            rows = rows, cols = cols, fixdims = fixdims, 
                            ipage = ipage, scalebins = scalebins, 
                            ...)
                        }
                        if (plot) 
                          tempfun4(ipage = 0, ...)
                        if (print) {
                          npages <- ceiling(length(unique(ydbase$Yr.S))/maxrows/maxcols)
                          for (ipage in 1:npages) {
                            caption <- ptitle
                            pagetext <- ""
                            if (npages > 1) {
                              pagetext <- paste("_page", ipage, 
                                sep = "")
                              caption <- paste(caption, " (plot ", 
                                ipage, " of ", npages, ")", sep = "")
                            }
                            if (length(grep("Pearson", caption)) > 
                              0) {
                              caption <- paste(caption, "<br> \nClosed bubbles are positive residuals", 
                                "(observed > expected)", "and open bubbles are negative residuals", 
                                "(observed < expected).")
                            }
                            file <- paste(plotdir, "/", filenamestart, 
                              filename_fltsexmkt, "_", aalyr, 
                              "_", pagetext, ".png", sep = "")
                            plotinfo <- pngfun(file = file, caption = caption)
                            tempfun4(ipage = ipage, ...)
                            dev.off()
                          }
                        }
                      }
                      if (5 %in% subplots) {
                        z <- ydbase$Pearson
                        ptitle <- paste(aalyr, " Pearson residuals for A-L key, ", 
                          title_sexmkt, fleetnames[f], sep = "")
                        ptitle <- paste(ptitle, " (max=", round(abs(max(z)), 
                          digits = 2), ")", sep = "")
                        titles <- c(ptitle, titles)
                        tempfun5 <- function() {
                          bubble3(x = ydbase$Bin, y = ydbase$Lbin_lo, 
                            z = z, xlab = labels[2], ylab = labels[1], 
                            col = colvec[3], las = 1, main = ptitle, 
                            cex.main = cex.main, maxsize = pntscalar, 
                            cexZ1 = cexZ1, legend = bublegend, 
                            allopen = FALSE, minnbubble = minnbubble)
                        }
                        if (plot) 
                          tempfun5()
                        if (print) {
                          caption <- ptitle
                          pagetext <- ""
                          if (npages > 1) {
                            pagetext <- paste("_page", ipage, 
                              sep = "")
                            caption <- paste(caption, " (plot ", 
                              ipage, " of ", npages, ")", sep = "")
                          }
                          if (length(grep("Pearson", caption)) > 
                            0) {
                            caption <- paste(caption, "<br> \nClosed bubbles are positive residuals", 
                              "(observed > expected)", "and open bubbles are negative residuals", 
                              "(observed < expected).")
                          }
                          file <- paste(plotdir, "/", filenamestart, 
                            "yearresids_", filename_fltsexmkt, 
                            "_", aalyr, pagetext, ".png", sep = "")
                          plotinfo <- pngfun(file = file, caption = caption)
                          tempfun5()
                          dev.off()
                        }
                      }
                    }
                  }
                }
                if (6 %in% subplots & aalbin[1] > 0) {
                  badbins <- setdiff(aalbin, dbase$Lbin_hi)
                  goodbins <- intersect(aalbin, dbase$Lbin_hi)
                  if (length(goodbins) > 0) {
                    if (length(badbins) > 0) {
                      cat("Error! the following inputs for 'aalbin' do not match the Lbin_hi values for the conditional age-at-length data:", 
                        badbins, "\n", "       the following inputs for 'aalbin' are fine:", 
                        goodbins, "\n")
                    }
                    for (ibin in 1:length(goodbins)) {
                      ilenbin <- goodbins[ibin]
                      abindbase <- dbase[dbase$Lbin_hi == ilenbin, 
                        ]
                      if (nrow(abindbase) > 0) {
                        ptitle <- paste("Age-at-length ", ilenbin, 
                          labels[7], ", ", title_sexmkt, fleetnames[f], 
                          sep = "")
                        titles <- c(ptitle, titles)
                        tempfun6 <- function(ipage, ...) {
                          make_multifig(ptsx = abindbase$Bin, 
                            ptsy = abindbase$Obs, yr = abindbase$Yr.S, 
                            linesx = abindbase$Bin, linesy = abindbase$Exp, 
                            sampsize = abindbase$N, effN = abindbase$effN, 
                            showsampsize = showsampsize, showeffN = showeffN, 
                            nlegends = 3, legtext = list(abindbase$YrSeasName, 
                              "sampsize", "effN"), bars = bars, 
                            linepos = (1 - datonly) * linepos, 
                            main = ptitle, cex.main = cex.main, 
                            xlab = kindlab, ylab = labels[6], 
                            maxrows = maxrows, maxcols = maxcols, 
                            rows = rows, cols = cols, fixdims = fixdims, 
                            ipage = ipage, scalebins = scalebins, 
                            ...)
                        }
                        if (plot) 
                          tempfun6(ipage = 0, ...)
                        if (print) {
                          npages <- ceiling(length(unique(abindbase$Yr.S))/maxrows/maxcols)
                          for (ipage in 1:npages) {
                            caption <- ptitle
                            pagetext <- ""
                            if (npages > 1) {
                              pagetext <- paste("_page", ipage, 
                                sep = "")
                              caption <- paste(caption, " (plot ", 
                                ipage, " of ", npages, ")", sep = "")
                            }
                            file <- paste(plotdir, "/", filenamestart, 
                              filename_fltsexmkt, "_length", 
                              ilenbin, labels[7], pagetext, ".png", 
                              sep = "")
                            plotinfo <- pngfun(file = file, caption = caption)
                            tempfun6(ipage = ipage, ...)
                            dev.off()
                          }
                        }
                      }
                    }
                  }
                }
                if (7 %in% subplots & samplesizeplots & !datonly & 
                  !(kind %in% c("GSTAGE", "GSTLEN", "L@A", "W@A"))) {
                  ptitle <- paste("N-EffN comparison, ", titledata, 
                    title_sexmkt, fleetnames[f], sep = "")
                  titles <- c(ptitle, titles)
                  lfitfunc <- function() {
                    if (kind == "cond") {
                      dbasegood <- dbase[dbase$Obs >= 1e-04 & 
                        dbase$Exp < 0.99 & !is.na(dbase$effN) & 
                        dbase$effN < maxneff, ]
                    }
                    else {
                      dbasegood <- dbase
                    }
                    if (nrow(dbasegood) > 0) {
                      dbasegood2 <- dbasegood[, c("YrSeasName", 
                        "N", "effN")]
                      dbasegood2 <- unique(dbasegood2)
                      plot(dbasegood2$N, dbasegood2$effN, xlab = labels[4], 
                        main = ptitle, cex.main = cex.main, ylim = c(0, 
                          1.15 * max(dbasegood2$effN)), xlim = c(0, 
                          1.15 * max(dbasegood2$N)), col = colvec[3], 
                        pch = 19, ylab = labels[5], xaxs = "i", 
                        yaxs = "i")
                      if (showyears) {
                        par(xpd = TRUE)
                        text(x = dbasegood2$N, y = dbasegood2$effN, 
                          dbasegood2$YrSeasName, adj = c(-0.2, 
                            0.5))
                        par(xpd = FALSE)
                      }
                      abline(0, 1, col = "black", lty = 1)
                      if (smooth & length(unique(dbasegood2$N)) > 
                        6 & diff(range(dbasegood2$N)) > 2) {
                        old_warn <- options()$warn
                        options(warn = -1)
                        psmooth <- loess(dbasegood2$effN ~ dbasegood2$N, 
                          degree = 1)
                        options(warn = old_warn)
                        lines(psmooth$x[order(psmooth$x)], psmooth$fit[order(psmooth$x)], 
                          lwd = 1.2, col = "red", lty = "dashed")
                      }
                      if (addMeans) {
                        abline(v = mean(dbasegood2$N), lty = "22", 
                          col = "green3")
                        text(x = mean(dbasegood2$N), y = 0, col = "green3", 
                          "arithmetic mean", srt = 90, adj = c(-0.1, 
                            -0.3))
                        abline(h = 1/mean(1/dbasegood2$effN), 
                          lty = "22", col = "green3")
                        text(x = 0, y = 1/mean(1/dbasegood2$effN), 
                          col = "green3", "harmonic mean", adj = c(-0.1, 
                            -0.3))
                      }
                    }
                  }
                  if (plot) 
                    lfitfunc()
                  if (print) {
                    file <- paste(plotdir, "/", filenamestart, 
                      "sampsize_", filename_fltsexmkt, ".png", 
                      sep = "")
                    caption <- ptitle
                    plotinfo <- pngfun(file = file, caption = caption)
                    lfitfunc()
                    dev.off()
                  }
                }
                if (8 %in% subplots & kind %in% c("LEN", "SIZE", 
                  "AGE")) {
                  ptitle <- paste("Francis data weighting method TA1.8 ", 
                    fleetnames[f], sep = "")
                  kind2 <- tolower(kind)
                  if (plot) {
                    SSMethod.TA1.8(fit = replist, type = kind2, 
                      fleet = f)
                  }
                  if (print) {
                    caption <- ptitle
                    file <- paste(plotdir, "/", filenamestart, 
                      "data_weighting_TA1.8_", fleetnames[f], 
                      ".png", sep = "")
                    png(filename = file, width = pwidth, height = pheight, 
                      units = punits, res = res, pointsize = ptsize)
                    tmp <- SSMethod.TA1.8(fit = replist, type = kind2, 
                      fleet = f)
                    if (!is.null(tmp[1])) {
                      vals <- paste("Suggested sample size adjustment ", 
                        "(with 95% interval) for ", kind2, " data from ", 
                        fleetnames[f], ":<br>", round(tmp[1], 
                          4), " (", round(tmp[2], 4), "-", round(tmp[3], 
                          4), ")", sep = "")
                    }
                    else {
                      vals <- "Too few points to calculate adjustments"
                    }
                    caption <- paste(caption, "<br>", vals)
                    plotinfo <- rbind(plotinfo, data.frame(file = file, 
                      caption = caption))
                    dev.off()
                  }
                }
                if (9 %in% subplots & kind == "cond" & (f %in% 
                  condbase$Fleet)) {
                  ptitle <- paste("Francis data weighting method TA1.8 for conditional age data ", 
                    fleetnames[f], sep = "")
                  if (plot) {
                    SSMethod.Cond.TA1.8(fit = replist, fleet = f)
                  }
                  if (print) {
                    caption <- ptitle
                    caption <- paste(caption, "<br>For more info, see<br>", 
                      "<blockquote>Francis, R.I.C.C. (2011).", 
                      "Data weighting in statistical fisheries stock assessment", 
                      "models. <i>Can. J. Fish. Aquat. Sci.</i>", 
                      "68: 1124-1138.</blockquote>")
                    file <- paste(plotdir, "/", filenamestart, 
                      "data_weighting_TA1.8_condAge", fleetnames[f], 
                      ".png", sep = "")
                    plotinfo <- pngfun(file = file, caption = caption)
                    SSMethod.Cond.TA1.8(fit = replist, fleet = f)
                    dev.off()
                  }
                }
                if (10 %in% subplots & kind == "cond") {
                  ptitle <- paste(labels[14], title_sexmkt, fleetnames[f], 
                    sep = "")
                  andrefun <- function(ipage = 0) {
                    Lens <- sort(unique(dbase$Lbin_lo))
                    Yrs <- sort(unique(dbase$Yr.S))
                    ymax <- 1.1 * max(dbase$Bin, na.rm = TRUE)
                    xmax <- max(condbase$Lbin_hi, na.rm = TRUE)
                    xmin <- min(condbase$Lbin_lo, na.rm = TRUE)
                    npanels <- length(Yrs)
                    npages <- npanels/andrerows
                    panelrange <- 1:npanels
                    if (npages > 1 & ipage != 0) 
                      panelrange <- intersect(panelrange, 1:andrerows + 
                        andrerows * (ipage - 1))
                    Yrs2 <- Yrs[panelrange]
                    par(mfrow = c(andrerows, 2), mar = c(2, 4, 
                      1, 1), oma = andre_oma)
                    for (Yr in Yrs2) {
                      y <- dbase[dbase$Yr.S == Yr, ]
                      Size <- NULL
                      Size2 <- NULL
                      Obs <- NULL
                      Obs2 <- NULL
                      Pred <- NULL
                      Pred2 <- NULL
                      Upp <- NULL
                      Low <- NULL
                      Upp2 <- NULL
                      Low2 <- NULL
                      for (Ilen in Lens) {
                        z <- y[y$Lbin_lo == Ilen, ]
                        if (length(z[, 1]) > 0) {
                          weightsPred <- z$Exp/sum(z$Exp)
                          weightsObs <- z$Obs/sum(z$Obs)
                          ObsV <- sum(z$Bin * weightsObs)
                          ObsV2 <- sum(z$Bin * z$Bin * weightsObs)
                          PredV <- sum(z$Bin * weightsPred)
                          PredV2 <- sum(z$Bin * z$Bin * weightsPred)
                          NN <- z$N[1]
                          if (max(z$Obs) > 1e-04 & NN > 0) {
                            Size <- c(Size, Ilen)
                            Obs <- c(Obs, ObsV)
                            Pred <- c(Pred, PredV)
                            varn <- sqrt(PredV2 - PredV * PredV)/sqrt(NN)
                            Pred2 <- c(Pred2, varn)
                            varn <- sqrt(max(0, ObsV2 - ObsV * 
                              ObsV))/sqrt(NN)
                            Obs2 <- c(Obs2, varn)
                            Low <- c(Low, ObsV - 1.64 * varn)
                            Upp <- c(Upp, ObsV + 1.64 * varn)
                            if (NN > 1) {
                              Size2 <- c(Size2, Ilen)
                              Low2 <- c(Low2, varn * sqrt((NN - 
                                1)/qchisq(0.95, NN)))
                              Upp2 <- c(Upp2, varn * sqrt((NN - 
                                1)/qchisq(0.05, NN)))
                            }
                          }
                        }
                      }
                      if (length(Obs) > 0) {
                        plot(Size, Obs, type = "n", xlab = "", 
                          ylab = "Age", xlim = c(xmin, xmax), 
                          ylim = c(0, ymax), yaxs = "i")
                        label <- ifelse(nseasons == 1, floor(Yr), 
                          Yr)
                        text(x = par("usr")[1], y = 0.9 * ymax, 
                          labels = label, adj = c(-0.5, 0), font = 2, 
                          cex = 1.2)
                        if (length(Low) > 1) 
                          polygon(c(Size, rev(Size)), c(Low, 
                            rev(Upp)), col = "grey95", border = NA)
                        if (!datonly) 
                          lines(Size, Pred, col = 4, lwd = 3)
                        points(Size, Obs, pch = 16)
                        lines(Size, Low, lty = 3)
                        lines(Size, Upp, lty = 3)
                        if (par("mfg")[1] == 1) {
                          title(main = ptitle, xlab = labels[1], 
                            outer = TRUE, line = 1)
                        }
                        box()
                        ymax2 <- max(Obs2, Pred2) * 1.1
                        plot(Size, Obs2, type = "n", xlab = labels[1], 
                          ylab = labels[13], xlim = c(xmin, xmax), 
                          ylim = c(0, ymax2), yaxs = "i")
                        if (length(Low2) > 1) 
                          polygon(c(Size2, rev(Size2)), c(Low2, 
                            rev(Upp2)), col = "grey95", border = NA)
                        if (!datonly) 
                          lines(Size, Pred2, col = 4, lwd = 3)
                        points(Size, Obs2, pch = 16)
                        lines(Size2, Low2, lty = 3)
                        lines(Size2, Upp2, lty = 3)
                        if (!datonly & par("mfg")[1] == 1) {
                          legend("topleft", legend = c("Observed (with 90% interval)", 
                            "Expected"), bty = "n", col = c(1, 
                            4), pch = c(16, NA), lty = c(NA, 
                            1), lwd = 3)
                        }
                        box()
                      }
                    }
                  }
                  if (plot) 
                    andrefun()
                  if (print) {
                    npages <- ceiling(length(unique(dbase$Yr.S))/andrerows)
                    for (ipage in 1:npages) {
                      caption <- paste(ptitle)
                      pagetext <- ""
                      if (npages > 1) {
                        pagetext <- paste("_page", ipage, sep = "")
                        caption <- paste(caption, " (plot ", 
                          ipage, " of ", npages, ")", sep = "")
                        caption <- paste(caption, "\nThese plots show mean age and std. dev. in conditional A@L.<br>", 
                          "Left plots are mean A@L by size-class (obs. and pred.) ", 
                          "with 90% CIs based on adding 1.64 SE of mean to the data.<br>", 
                          "Right plots in each pair are SE of mean A@L (obs. and pred.) ", 
                          "with 90% CIs based on the chi-square distribution.", 
                          sep = "")
                      }
                      file <- paste(plotdir, "/", filenamestart, 
                        "Andre_plots", filename_fltsexmkt, pagetext, 
                        ".png", sep = "")
                      plotinfo <- pngfun(file = file, caption = caption)
                      andrefun(ipage = ipage)
                      dev.off()
                    }
                  }
                }
            }
        }
    }
    if (21 %in% subplots & kind != "cond") {
        dbasef <- dbase_kind[dbase_kind$Fleet %in% fleets, ]
        if (nrow(dbasef) > 0) {
            dbase_k <- dbasef
            for (j in unique(dbase_k$Part)) {
                dbase <- dbase_k[dbase_k$Part == j, ]
                if (nrow(dbase) > 0) {
                  if (j == 0) 
                    titlemkt <- "whole catch, "
                  if (j == 1) 
                    titlemkt <- "discard, "
                  if (j == 2) 
                    titlemkt <- "retained, "
                  titlemkt <- ifelse(printmkt, titlemkt, "")
                  if (datonly | fitbar) {
                    bars <- TRUE
                  }
                  else {
                    bars <- FALSE
                  }
                  title_sexmkt <- paste(titlesex, titlemkt, sep = "")
                  filename_fltsexmkt <- paste(filesex, "mkt", 
                    j, sep = "")
                  ptitle <- paste(titledata, title_sexmkt, "aggregated across time by fleet", 
                    sep = "")
                  titles <- c(ptitle, titles)
                  Bins <- sort(unique(dbase$Bin))
                  nbins <- length(Bins)
                  df <- data.frame(N = dbase$N, effN = dbase$effN, 
                    obs = dbase$Obs * dbase$N, exp = dbase$Exp * 
                      dbase$N)
                  agg <- aggregate(x = df, by = list(bin = dbase$Bin, 
                    f = dbase$Fleet, sex = dbase$sex), FUN = sum)
                  agg <- agg[agg$f %in% fleets, ]
                  agg$obs <- agg$obs/agg$N
                  agg$exp <- agg$exp/agg$N
                  for (f in unique(agg$f)) {
                    infleet <- agg$f == f
                    agg$N[infleet] <- max(agg$N[infleet])
                    agg$effN[infleet] <- max(agg$effN[infleet])
                  }
                  namesvec <- fleetnames[agg$f]
                  if (!(kind %in% c("GSTAGE", "GSTLEN", "L@A", 
                    "W@A"))) {
                    tempfun7 <- function(ipage, ...) {
                      make_multifig(ptsx = agg$bin, ptsy = agg$obs, 
                        yr = agg$f, linesx = agg$bin, linesy = agg$exp, 
                        sampsize = agg$N, effN = agg$effN, showsampsize = showsampsize, 
                        showeffN = showeffN, bars = bars, linepos = (1 - 
                          datonly) * linepos, nlegends = 3, legtext = list(namesvec, 
                          "sampsize", "effN"), main = ptitle, 
                        cex.main = cex.main, xlab = kindlab, 
                        ylab = labels[6], maxrows = maxrows, 
                        maxcols = maxcols, rows = rows, cols = cols, 
                        fixdims = fixdims2, ipage = ipage, lwd = 2, 
                        scalebins = scalebins, sexvec = agg$sex, 
                        yupper = yupper, ...)
                    }
                    if (plot) 
                      tempfun7(ipage = 0, ...)
                    if (print) {
                      npages <- ceiling(length(unique(agg$f))/maxrows/maxcols)
                      for (ipage in 1:npages) {
                        caption <- ptitle
                        pagetext <- ""
                        if (npages > 1) {
                          pagetext <- paste("_page", ipage, sep = "")
                          caption <- paste(caption, " (plot ", 
                            ipage, " of ", npages, ")", sep = "")
                        }
                        file <- paste(plotdir, "/", filenamestart, 
                          filename_fltsexmkt, pagetext, "_aggregated_across_time.png", 
                          sep = "")
                        plotinfo <- pngfun(file = file, caption = caption)
                        tempfun7(ipage = ipage, ...)
                        dev.off()
                      }
                    }
                  }
                  else {
                  }
                }
            }
        }
    }
    if (22 %in% subplots & kind != "cond" & nseasons > 1) {
        dbasef <- dbase_kind[dbase_kind$Fleet %in% fleets, ]
        if (nrow(dbasef) > 0) {
            testor <- length(dbasef$Gender[dbasef$Gender == 1 & 
                dbasef$Pick_gender == 0]) > 0
            testor[2] <- length(dbasef$Gender[dbasef$Gender == 
                1 & dbasef$Pick_gender %in% c(1, 3)]) > 0
            testor[3] <- length(dbasef$Gender[dbasef$Gender == 
                2]) > 0
            for (k in (1:3)[testor]) {
                if (k == 1) {
                  dbase_k <- dbasef[dbasef$Gender == 1 & dbasef$Pick_gender == 
                    0, ]
                }
                if (k == 2) {
                  dbase_k <- dbasef[dbasef$Gender == 1 & dbasef$Pick_gender %in% 
                    c(1, 3), ]
                }
                if (k == 3) {
                  dbase_k <- dbasef[dbasef$Gender == 2, ]
                }
                sex <- ifelse(k == 3, 2, 1)
                for (j in unique(dbase_k$Part)) {
                  dbase <- dbase_k[dbase_k$Part == j, ]
                  if (nrow(dbase) > 0) {
                    if (k == 1) 
                      titlesex <- "sexes combined, "
                    if (k == 2) 
                      titlesex <- "female, "
                    if (k == 3) 
                      titlesex <- "male, "
                    titlesex <- ifelse(printsex, titlesex, "")
                    if (j == 0) 
                      titlemkt <- "whole catch, "
                    if (j == 1) 
                      titlemkt <- "discard, "
                    if (j == 2) 
                      titlemkt <- "retained, "
                    titlemkt <- ifelse(printmkt, titlemkt, "")
                    if (datonly | fitbar) 
                      bars <- TRUE
                    else bars <- FALSE
                    title_sexmkt <- paste(titlesex, titlemkt, 
                      sep = "")
                    filename_fltsexmkt <- paste("sex", k, "mkt", 
                      j, sep = "")
                    ptitle <- paste(titledata, title_sexmkt, 
                      "\naggregated within season by fleet", 
                      sep = "")
                    titles <- c(ptitle, titles)
                    Bins <- sort(unique(dbase$Bin))
                    nbins <- length(Bins)
                    df <- data.frame(N = dbase$N, effN = dbase$effN, 
                      obs = dbase$Obs * dbase$N, exp = dbase$Exp * 
                        dbase$N)
                    agg <- aggregate(x = df, by = list(bin = dbase$Bin, 
                      f = dbase$Fleet, s = dbase$Seas), FUN = sum)
                    agg <- agg[agg$f %in% fleets, ]
                    if (any(agg$s <= 0)) {
                      cat("super-periods may not work correctly in plots of aggregated comps\n")
                      agg <- agg[agg$s > 0, ]
                    }
                    agg$obs <- agg$obs/agg$N
                    agg$exp <- agg$exp/agg$N
                    for (f in unique(agg$f)) {
                      for (s in unique(agg$s[agg$f == f])) {
                        infleetseas <- agg$f == f & agg$s == 
                          s
                        agg$N[infleetseas] <- max(agg$N[infleetseas])
                        agg$effN[infleetseas] <- max(agg$effN[infleetseas])
                      }
                    }
                    agg$fseas <- agg$f + seasfracs[agg$s]
                    namesvec <- paste(fleetnames[agg$f], " s", 
                      agg$s, sep = "")
                    tempfun8 <- function(ipage, ...) {
                      if (!(kind %in% c("GSTAGE", "GSTLEN", "L@A", 
                        "W@A"))) {
                        make_multifig(ptsx = agg$bin, ptsy = agg$obs, 
                          yr = agg$fseas, linesx = agg$bin, linesy = agg$exp, 
                          sampsize = agg$N, effN = agg$effN, 
                          showsampsize = showsampsize, showeffN = showeffN, 
                          bars = bars, linepos = (1 - datonly) * 
                            linepos, nlegends = 3, legtext = list(namesvec, 
                            "sampsize", "effN"), main = ptitle, 
                          cex.main = cex.main, xlab = kindlab, 
                          ylab = labels[6], maxrows = maxrows, 
                          maxcols = maxcols, rows = rows, cols = cols, 
                          fixdims = fixdims2, ipage = ipage, 
                          lwd = 2, scalebins = scalebins, yupper = yupper, 
                          ...)
                      }
                    }
                    if (plot) 
                      tempfun8(ipage = 0, ...)
                    if (print) {
                      npages <- ceiling(length(unique(agg$fseas))/maxrows/maxcols)
                      for (ipage in 1:npages) {
                        caption <- ptitle
                        pagetext <- ""
                        if (npages > 1) {
                          pagetext <- paste("_page", ipage, sep = "")
                          caption <- paste(caption, " (plot ", 
                            ipage, " of ", npages, ")", sep = "")
                        }
                        file <- paste(plotdir, "/", filenamestart, 
                          filename_fltsexmkt, pagetext, "_aggregated_within_season.png", 
                          sep = "")
                        plotinfo <- pngfun(file = file, caption = caption)
                        tempfun8(ipage = ipage, ...)
                        dev.off()
                      }
                    }
                  }
                }
            }
        }
    }
    if (23 %in% subplots & kind != "cond" & nseasons > 1) {
        for (f in fleets) {
            dbasef <- dbase_kind[dbase_kind$Fleet == f, ]
            if (nrow(dbasef) > 0) {
                testor <- length(dbasef$Gender[dbasef$Gender == 
                  1 & dbasef$Pick_gender == 0]) > 0
                testor[2] <- length(dbasef$Gender[dbasef$Gender == 
                  1 & dbasef$Pick_gender %in% c(1, 3)]) > 0
                testor[3] <- length(dbasef$Gender[dbasef$Gender == 
                  2]) > 0
                for (k in (1:3)[testor]) {
                  if (k == 1) {
                    dbase_k <- dbasef[dbasef$Gender == 1 & dbasef$Pick_gender == 
                      0, ]
                  }
                  if (k == 2) {
                    dbase_k <- dbasef[dbasef$Gender == 1 & dbasef$Pick_gender %in% 
                      c(1, 3), ]
                  }
                  if (k == 3) {
                    dbase_k <- dbasef[dbasef$Gender == 2, ]
                  }
                  sex <- ifelse(k == 3, 2, 1)
                  for (j in unique(dbase_k$Part)) {
                    dbase <- dbase_k[dbase_k$Part == j, ]
                    if (nrow(dbase) > 0) {
                      if (k == 1) 
                        titlesex <- "sexes combined, "
                      if (k == 2) 
                        titlesex <- "female, "
                      if (k == 3) 
                        titlesex <- "male, "
                      titlesex <- ifelse(printsex, titlesex, 
                        "")
                      if (j == 0) 
                        titlemkt <- "whole catch, "
                      if (j == 1) 
                        titlemkt <- "discard, "
                      if (j == 2) 
                        titlemkt <- "retained, "
                      titlemkt <- ifelse(printmkt, titlemkt, 
                        "")
                      if (datonly | fitbar) 
                        bars <- TRUE
                      else bars <- FALSE
                      title_sexmkt <- paste(titlesex, titlemkt, 
                        sep = "")
                      filename_fltsexmkt <- paste("flt", f, "sex", 
                        k, "mkt", j, sep = "")
                      Bins <- sort(unique(dbase$Bin))
                      nbins <- length(Bins)
                      df <- data.frame(N = dbase$N, effN = dbase$effN, 
                        obs = dbase$Obs * dbase$N, exp = dbase$Exp * 
                          dbase$N)
                      agg <- aggregate(x = df, by = list(bin = dbase$Bin, 
                        f = dbase$Fleet, y = floor(dbase$Yr.S)), 
                        FUN = sum)
                      agg <- agg[agg$f %in% fleets, ]
                      agg$obs <- agg$obs/agg$N
                      agg$exp <- agg$exp/agg$N
                      for (f in unique(agg$f)) {
                        for (y in unique(agg$y[agg$f == f])) {
                          infleetyr <- agg$f == f & agg$y == 
                            y
                          agg$N[infleetyr] <- max(agg$N[infleetyr])
                          agg$effN[infleetyr] <- max(agg$effN[infleetyr])
                        }
                      }
                      agg$fy <- agg$f + agg$y/10000
                      ptitle <- paste(titledata, title_sexmkt, 
                        fleetnames[f], "\naggregated across seasons within year", 
                        sep = "")
                      tempfun9 <- function(ipage, ...) {
                        if (!(kind %in% c("GSTAGE", "GSTLEN", 
                          "L@A", "W@A"))) {
                          make_multifig(ptsx = agg$bin, ptsy = agg$obs, 
                            yr = agg$fy, linesx = agg$bin, linesy = agg$exp, 
                            sampsize = agg$N, effN = agg$effN, 
                            showsampsize = showsampsize, showeffN = showeffN, 
                            bars = bars, linepos = (1 - datonly) * 
                              linepos, nlegends = 3, legtext = list(agg$y, 
                              "sampsize", "effN"), main = ptitle, 
                            cex.main = cex.main, xlab = kindlab, 
                            ylab = labels[6], maxrows = maxrows, 
                            maxcols = maxcols, rows = rows, cols = cols, 
                            fixdims = fixdims2, ipage = ipage, 
                            lwd = 2, scalebins = scalebins, yupper = yupper, 
                            ...)
                        }
                      }
                      if (plot) 
                        tempfun9(ipage = 0, ...)
                      if (print) {
                        npages <- ceiling(length(unique(agg$fy))/maxrows/maxcols)
                        for (ipage in 1:npages) {
                          caption <- ptitle
                          pagetext <- ""
                          if (npages > 1) {
                            pagetext <- paste("_page", ipage, 
                              sep = "")
                            caption <- paste(caption, " (plot ", 
                              ipage, " of ", npages, ")", sep = "")
                          }
                          file <- paste(plotdir, "/", filenamestart, 
                            filename_fltsexmkt, pagetext, "_aggregated_across_seasons_within_year.png", 
                            sep = "")
                          pngfun(file = file, caption = caption)
                          tempfun9(ipage = ipage, ...)
                          dev.off()
                        }
                      }
                    }
                  }
                }
            }
        }
    }
    if (24 %in% subplots & kind %in% c("LEN", "AGE")) {
        testor <- length(dbase_kind$Gender[dbase_kind$Gender == 
            1 & dbase_kind$Pick_gender == 0]) > 0
        testor[2] <- length(dbase_kind$Gender[dbase_kind$Gender == 
            1 & dbase_kind$Pick_gender %in% c(1, 3)]) > 0
        testor[3] <- length(dbase_kind$Gender[dbase_kind$Gender == 
            2]) > 0
        for (k in (1:3)[testor]) {
            if (k == 1) {
                dbase_k <- dbase_kind[dbase_kind$Gender == 1 & 
                  dbase_kind$Pick_gender == 0, ]
            }
            if (k == 2) {
                dbase_k <- dbase_kind[dbase_kind$Gender == 1 & 
                  dbase_kind$Pick_gender %in% c(1, 3), ]
            }
            if (k == 3) {
                dbase_k <- dbase_kind[dbase_kind$Gender == 2, 
                  ]
            }
            sex <- ifelse(k == 3, 2, 1)
            if (sex %in% sexes) {
                for (j in unique(dbase_k$Part)) {
                  dbase_fleets <- dbase_k[dbase_k$Part == j, 
                    ]
                  fleetvec <- intersect(fleets, dbase_fleets$Fleet)
                  npanels <- length(fleetvec)
                  xlim <- range(dbase_fleets$Yr.S)
                  xaxislab <- sort(unique(floor(dbase_fleets$Yr.S)))
                  if (length(cohortlines) > 0) {
                    growdat <- replist$endgrowth
                    growdatF <- growdat[growdat$Gender == 1 & 
                      growdat$Morph == min(growdat$Morph[growdat$Gender == 
                        1]), ]
                    if (nsexes > 1) {
                      growdatM <- growdat[growdat$Gender == 2 & 
                        growdat$Morph == min(growdat$Morph[growdat$Gender == 
                          2]), ]
                    }
                  }
                  if (k == 1) 
                    titlesex <- "sexes combined, "
                  if (k == 2) 
                    titlesex <- "female, "
                  if (k == 3) 
                    titlesex <- "male, "
                  titlesex <- ifelse(printsex, titlesex, "")
                  if (j == 0) 
                    titlemkt <- "whole catch"
                  if (j == 1) 
                    titlemkt <- "discard"
                  if (j == 2) 
                    titlemkt <- "retained"
                  titlemkt <- ifelse(printmkt, titlemkt, "")
                  title_sexmkt <- paste(titlesex, titlemkt, sep = "")
                  ##ptitle <- paste(titletype, title_sexmkt, ", comparing across fleets", 
                  ##  sep = "")
                  ptitle <- paste(titletype, title_sexmkt, "", 
                    sep = "")
                  titles <- c(ptitle, titles)
                  filename_sexmkt <- paste("sex", k, "mkt", j, 
                    sep = "")
                  multifleet.bubble.fun <- function(ipage = 0) {
                    par_old <- par()
                    par(mfrow = c(min(npanels, maxrows), 1), 
                      mar = c(0.5, 0, 0, 0), oma = c(4, 6, 3, 
                        1))
                    panelrange <- 1:npanels
                    npages <- ceiling(npanels/maxrows)
                    if (npages > 1 & ipage != 0) 
                      panelrange <- intersect(panelrange, 1:maxrows + 
                        maxrows * (ipage - 1))
                    for (f in fleetvec[panelrange]) {
                      dbase <- dbase_fleets[dbase_fleets$Fleet == 
                        f, ]
                      max_n_ageerr <- max(apply(table(dbase$Yr.S, 
                        dbase$Ageerr) > 0, 1, sum))
                      if (max_n_ageerr > 1) {
                        if (ageerr_warning) {
                          cat("Note: multiple samples with different ageing error types within fleet/year.\n", 
                            "     Plots label '2005a3' indicates ageing error type 3 for 2005 sample.\n", 
                            "     Bubble plots may be misleading with overlapping bubbles.\n")
                          ageerr_warning <- FALSE
                        }
                        dbase$Yr.S <- dbase$Yr.S + dbase$Ageerr/(1000 * 
                          max_n_ageerr)
                        dbase$YrSeasName <- paste(dbase$YrSeasName, 
                          "a", dbase$Ageerr, sep = "")
                      }
                      if (datonly) {
                        z <- dbase$Obs
                        if (scalebubbles) 
                          z <- dbase$N * dbase$Obs
                        cols <- rep("black", nrow(dbase))
                        titletype <- titledata
                        filetype <- "bub"
                        allopen <- TRUE
                      }
                      else {
                        z <- dbase$Pearson
                        cols <- rep(colvec[3], nrow(dbase))
                        titletype <- "Pearson residuals, "
                        filetype <- "resids"
                        allopen <- FALSE
                      }
                      ylim <- range(dbase$Bin)
                      ylim[2] <- ylim[2] + 0.2 * diff(ylim)
                      bubble3(x = dbase$Yr.S, y = dbase$Bin, 
                        z = z, col = cols, cexZ1 = cexZ1, legend = bublegend, 
                        las = 1, main = "", cex.main = cex.main, 
                        maxsize = pntscalar, allopen = allopen, 
                        xlim = xlim, ylim = ylim, axis1 = FALSE)
                      mtext(fleetnames[f], side = 2, line = 4.5, 
                        cex = par()$cex)
                      if (length(cohortlines) > 0) {
                        for (icohort in 1:length(cohortlines)) {
                          cat("  Adding line for", cohortlines[icohort], 
                            "cohort\n")
                          if (kind == "LEN") {
                            if (k %in% c(1, 2)) 
                              lines(growdatF$Age + cohortlines[icohort], 
                                growdatF$Len_Mid, col = colvec[1])
                            if (nsexes > 1 & k %in% c(1, 3)) 
                              lines(growdatM$Age + cohortlines[icohort], 
                                growdatM$Len_Mid, col = colvec[2])
                          }
                          if (kind == "AGE") {
                            lines(c(cohortlines[icohort], cohortlines[icohort] + 
                              accuage), c(0, accuage), col = "red")
                          }
                        }
                      }
                      if (par()$mfg[1] == par()$mfg[3] | f == 
                        tail(fleetvec, 1)) {
                        axis(1, at = xaxislab)
                      }
                      else {
                        axis(1, at = xaxislab, labels = rep("", 
                          length(xaxislab)))
                      }
                      if (par()$mfg[1] == 1) 
                        title(main = ptitle, outer = TRUE, xlab = labels[3], 
                          ylab = kindlab)
                    }
                    par(mfcol = par_old$mfcol, mar = par_old$mar, 
                      oma = par_old$oma)
                  }
                  if (length(fleetvec) > 0) {
                    if (plot) 
                      multifleet.bubble.fun(ipage = 0)
                    if (print) {
                      npages <- ceiling(length(fleetvec)/maxrows)
                      for (ipage in 1:npages) {
                        caption <- ptitle
                        pagetext <- ""
                        if (npages > 1) {
                          pagetext <- paste("_page", ipage, sep = "")
                          caption <- paste(caption, " (plot ", 
                            ipage, " of ", npages, ")", sep = "")
                        }
                        if (length(grep("Pearson", caption)) > 
                          0) {
                          caption <- paste(caption, "<br> \nClosed bubbles are positive residuals", 
                            "(observed > expected)", "and open bubbles are negative residuals", 
                            "(observed < expected).")
                        }
                        caption <- paste("Note: this plot doesn't seem to be working right", 
                          "for some models.<br><br>", caption)
                        file <- paste(plotdir, "/", filenamestart, 
                          filename_sexmkt, pagetext, "_multi-fleet_comparison.png", 
                          sep = "")
                        plotinfo <- pngfun(file = file, caption = caption)
                        multifleet.bubble.fun(ipage = ipage)
                        dev.off()
                      }
                    }
                  }
                }
            }
        }
        par(mfcol = c(rows, cols), mar = c(5, 4, 4, 2) + 0.1, 
            oma = rep(0, 4))
    }
    if (!is.null(plotinfo)) 
        plotinfo$category <- "Comp"
    return(invisible(plotinfo))
}

bubble3 <- function (x,y,z,col=1,cexZ1=5,maxsize=NULL,do.sqrt=TRUE,
                     bg.open=gray(0.95,0.3),
                     legend=TRUE,legendloc='top',
                     legend.z="default",legend.yadj=1.1,
                     main="",cex.main=1,xlab="",ylab="",minnbubble=3,
                     xlim=NULL,ylim=NULL,axis1=TRUE,xlimextra=1,
                     add=FALSE,las=1,allopen=TRUE)
{
    # This function is vaguely based on bubble() from gstat.
    # Not sure anymore what happened to bubble2.
    if(diff(range(length(x),length(y),length(z)))>0)
      stop("x, y, and z should all be equal in length")
    # filter NA values
    x <- x[!is.na(z)]
    y <- y[!is.na(z)]
    z <- z[!is.na(z)]

    n <- length(x)
    if(n==0) return()

    az <- abs(z)
    if(legend.z[1]=="default"){
      # set sequence of points to use in legend
      maxaz <- max(az,na.rm=TRUE)
      if(maxaz>1)  legend.z <- c(.1,1:3) # something like Pearsons
      if(maxaz>5)  legend.z <- c(.1,pretty(c(1,maxaz),n=2)) # big Pearsons
      if(maxaz>10) legend.z <- pretty(c(0,maxaz)) # something like numbers
      if(maxaz<=1) legend.z <- c(0.01,.1*pretty(c(1,10*maxaz),n=2)) # something like proportions
      # exclude zero
      legend.z <- setdiff(legend.z,0)
      # if legend is too long, cut in half
      if(length(legend.z)>3) legend.z <- legend.z[seq(1,length(legend.z),2)]
      # add negatives
      if(any(z<0)){
        legend.z <- c(-rev(legend.z[-1]),legend.z)
      }
    }
    legend.n <- length(legend.z)
    legend.z2 <- legend.z

    # scale by square root if desired
    if (do.sqrt){
      legend.z2 <- sqrt(abs(legend.z))
      az <- sqrt(az)
    }

    # set scale
    if(!is.null(maxsize)) cexZ1 <- maxsize/max(az)
    
    cex <- cexZ1*az
    legend.cex <- cexZ1*legend.z2
    
    # if xlim is not specified, then set to the range, or range plus padding
    if(is.null(xlim)){
      xlim <- range(x)
      if(length(unique(x))<minnbubble) xlim=xlim+c(-1,1)*xlimextra
    }
    #### old way using plot character to control open/closed
    ## # set plot character
    ## pch <- rep(NA,n)
    ## pch[z>0] <- 16
    ## pch[z<0] <- 1
    ## legend.pch <- rep(NA,legend.n)
    ## legend.pch[legend.z>0] <- 16
    ## legend.pch[legend.z<0] <- 1

    ## # if only open points to be shown
    ## if(allopen){
    ##   legend.z <- legend.z[legend.z>0]
    ##   legend.pch <- 1
    ##   pch[!is.na(pch)] <- 1
    ## }

    #### new way using background color
    # set plot character
    pch <- rep(21,n)
    pch[is.na(z) | z==0] <- NA
    # set background color equal to open color for all points 
    bg <- rep(bg.open, n)
    if(!allopen){
      # replace background color with foreground color for closed points
      # (if not all open)
      bg[z>0] <- col[z>0]
    }
    legend.pch <- rep(21, legend.n)
    legend.bg <- rep(bg.open, legend.n)
    legend.bg[legend.z>0] <- col[1] # error occured when full vector was used

    # if only open points to be shown
    if(allopen){
      legend.z <- legend.z[legend.z>0]
      legend.bg <- bg.open
      legend.pch <- 21
      #pch[!is.na(pch)] <- 1
    }

    # make empty plot (if needed)
    if(!add){
      if(is.null(ylim)) ylim <- range(y)
      ylim[2] <- legend.yadj*ylim[2]
      plot(x,y,type="n",xlim=xlim,ylim=ylim,main=main,cex.main=cex.main,
           xlab=xlab,ylab=ylab,axes=FALSE)
      xvec <- unique(x)
      if(axis1) axis(1,at=floor(unique(x))) # only printing integer values for years
      axis2vals <- sort(unique(y))
      if(length(axis2vals)>20) axis2vals <- pretty(axis2vals)
      axis(2,at=axis2vals,las=las)
      box()
    }
    # add points
    points(x, y, pch=pch, cex=cex, col=col, bg=bg)
    # do things for legend
    if(legend & all(par()$mfg[1:2]==1)){
      # set labels
      legend.lab <- format(legend.z, scientific=FALSE,drop0trailing=TRUE)
      # add legend
      legend(x=legendloc,legend=legend.lab,pch=legend.pch,col=col,pt.bg=legend.bg,
             pt.cex=legend.cex,ncol=legend.n,bty='n')
      ## next line for debugging legends
      # print(data.frame(legendloc,legend.z,legend.pch,col,legend.cex,legend.n))
    }
  }

SSplotNumbers <- function (replist, subplots = 1:9, plot = TRUE, print = FALSE, 
                           numbers.unit = 1000, areas = "all", areanames = "default", 
                           areacols = "default", pntscalar = 2.6, bub.bg = gray(0.5, 
                                                                                alpha = 0.5), bublegend = TRUE, period = c("B", "M"), 
                           add = FALSE, labels = c("Year", "Age", "True age (yr)", "SD of observed age (yr)", 
                                                   "Mean observed age (yr)", "Mean age (yr)", "mean age in the population", 
                                                   "Ageing imprecision", "Numbers at age at equilibrium", 
                                                   "Equilibrium age distribution", "Sex ratio of numbers at age (males/females)", 
                                                   "Length", "Mean length (cm)", "mean length (cm) in the population", 
                                                   "expected numbers at age", "Beginning of year", "Middle of year", 
                                                   "expected numbers at length", "Sex ratio of numbers at length (males/females)", 
                                                   "Sex ratio of numbers at length (females/males)"), pwidth = 6.5, 
    pheight = 5, punits = "in", res = 300, ptsize = 10, cex.main = 1, 
    plotdir = "default", verbose = TRUE) 
{
    pngfun <- function(file, caption = NA) {
        png(filename = file, width = pwidth, height = pheight, 
            units = punits, res = res, pointsize = ptsize)
        plotinfo <- rbind(plotinfo, data.frame(file = file, caption = caption))
        return(plotinfo)
    }
    plotinfo <- NULL
    natage <- replist$natage
    natlen <- replist$natlen
    if (plotdir == "default") 
        plotdir <- replist$inputs$dir
    if (is.null(natage)) {
        cat("Skipped some plots because NUMBERS_AT_AGE unavailable in report file\n", 
            "     change starter file setting for 'detailed age-structured reports'\n")
    }
    else {
        nsexes <- replist$nsexes
        nareas <- replist$nareas
        nseasons <- replist$nseasons
        spawnseas <- replist$spawnseas
        ngpatterns <- replist$ngpatterns
        morphlist <- replist$morphlist
        morph_indexing <- replist$morph_indexing
        accuage <- replist$accuage
        agebins <- replist$agebins
        endyr <- replist$endyr
        N_ageerror_defs <- replist$N_ageerror_defs
        AAK <- replist$AAK
        age_error_mean <- replist$age_error_mean
        age_error_sd <- replist$age_error_sd
        lbinspop <- replist$lbinspop
        nlbinspop <- replist$nlbinspop
        recruitment_dist <- replist$recruitment_dist
        mainmorphs <- replist$mainmorphs
        if (!"BirthSeas" %in% names(natage)) {
            cat("Numbers at age plots haven't been updated to work with SS version 3.30\n")
            return()
        }
        SS_versionshort <- toupper(substr(replist$SS_version, 
            1, 8))
        if (areas[1] == "all") {
            areas <- 1:nareas
        }
        else {
            if (length(intersect(areas, 1:nareas)) != length(areas)) {
                stop("Input 'areas' should be 'all' or a vector of values between 1 and nareas.")
            }
        }
        if (areanames[1] == "default") 
            areanames <- paste("area", 1:nareas)
        if (areacols[1] == "default") {
            areacols <- rich.colors.short(nareas)
            if (nareas > 2) 
                areacols <- rich.colors.short(nareas + 1)[-1]
        }
        if (SS_versionshort == c("SS-V3.10")) 
            stop("numbers at age plots no longer supported for SS-V3.10 and earlier")
        column1 <- 12
        remove <- -(1:(column1 - 1))
        bseas <- unique(natage$BirthSeas)
        if (length(bseas) > 1) 
            cat("Numbers at age plots are for only the first birth season\n")
        if (ngpatterns > 1) 
            cat("Numbers at age plots may not deal correctly with growth patterns: no guarantees!\n")
        if (nseasons > 1) 
            cat("Numbers at age plots are for season 1 only\n")
        for (iarea in areas) {
            for (iperiod in 1:length(period)) {
                for (m in 1:nsexes) {
                  natagetemp_all <- natage[natage$Area == iarea & 
                    natage$Gender == m & natage$Seas == 1 & natage$Era != 
                    "VIRG" & !is.na(natage$"0") & natage$Yr < 
                    (endyr + 2) & natage$BirthSeas == min(bseas), 
                    ]
                  natagetemp_all <- natagetemp_all[natagetemp_all$"Beg/Mid" == 
                    period[iperiod], ]
                  morphlist <- unique(natagetemp_all$SubMorph)
                  natagetemp0 <- natagetemp_all[natagetemp_all$SubMorph == 
                    morphlist[1] & natagetemp_all$Bio_Pattern == 
                    1, ]
                  for (iage in 0:accuage) natagetemp0[, column1 + 
                    iage] <- 0
                  for (imorph in 1:length(morphlist)) {
                    for (igp in 1:ngpatterns) {
                      natagetemp_imorph_igp <- natagetemp_all[natagetemp_all$SubMorph == 
                        morphlist[imorph] & natagetemp_all$Bio_Pattern == 
                        igp, ]
                      natagetemp0[, column1 + 0:accuage] <- natagetemp0[, 
                        column1 + 0:accuage] + natagetemp_imorph_igp[, 
                        column1 + 0:accuage]
                    }
                  }
                  if (ngpatterns > 0) 
                    natagetemp0$Bio_Pattern == 999
                  nyrsplot <- nrow(natagetemp0)
                  resx <- rep(natagetemp0$Yr, accuage + 1)
                  resy <- NULL
                  for (i in 0:accuage) resy <- c(resy, rep(i, 
                    nyrsplot))
                  resz <- NULL
                  for (i in column1 + 0:accuage) {
                    resz <- c(resz, numbers.unit * natagetemp0[, 
                      i])
                  }
                  units <- ""
                  if (max(resz) > 1e+09) {
                    resz <- resz/1e+09
                    units <- "billion"
                  }
                  if (max(resz) > 1e+06 & units == "") {
                    resz <- resz/1e+06
                    units <- "million"
                  }
                  if (max(resz) > 1000 & units == "") {
                    resz <- resz/1000
                    units <- "thousand"
                  }
                  if (iperiod == 1) {
                    assign(paste("natagetemp0area", iarea, "sex", 
                      m, sep = ""), natagetemp0)
                  }
                  if (m == 1 & nsexes == 1) 
                    sextitle <- ""
                  if (m == 1 & nsexes == 2) 
                    sextitle <- " of females"
                  if (m == 2) 
                    sextitle = " of males"
                  if (nareas > 1) 
                    sextitle <- paste(sextitle, " in ", areanames[iarea], 
                      sep = "")
                  if (!period[iperiod] %in% c("B", "M")) {
                    stop("'period' input to SSplotNumbers should include only 'B' or 'M'")
                  }
                  if (period[iperiod] == "B") {
                    periodtitle <- labels[16]
                    fileperiod <- "_beg"
                  }
                  if (period[iperiod] == "M") {
                    periodtitle <- labels[17]
                    fileperiod <- "_mid"
                  }
                  plottitle1 <- paste(periodtitle, " ", labels[15], 
                    sextitle, " in (max ~ ", format(round(max(resz), 
                      1), nsmall = 1), " ", units, ")", sep = "")
                  natagetemp1 <- as.matrix(natagetemp0[, remove])
                  ages <- 0:accuage
                  natagetemp2 <- as.data.frame(natagetemp1)
                  natagetemp2$sum <- as.vector(apply(natagetemp1, 
                    1, sum))
                  natagetemp0 <- natagetemp0[natagetemp2$sum > 
                    0, ]
                  natagetemp1 <- natagetemp1[natagetemp2$sum > 
                    0, ]
                  natagetemp2 <- natagetemp2[natagetemp2$sum > 
                    0, ]
                  prodmat <- t(natagetemp1) * ages
                  prodsum <- as.vector(apply(prodmat, 2, sum))
                  natagetemp2$sumprod <- prodsum
                  natagetemp2$meanage <- natagetemp2$sumprod/natagetemp2$sum - 
                    (natagetemp0$BirthSeas - 1)/nseasons
                  natageyrs <- sort(unique(natagetemp0$Yr))
                  if (iperiod == 1) 
                    natageyrsB <- natageyrs
                  meanage <- 0 * natageyrs
                  for (i in 1:length(natageyrs)) {
                    meanage[i] <- sum(natagetemp2$meanage[natagetemp0$Yr == 
                      natageyrs[i]] * natagetemp2$sum[natagetemp0$Yr == 
                      natageyrs[i]])/sum(natagetemp2$sum[natagetemp0$Yr == 
                      natageyrs[i]])
                  }
                  if (m == 1 & nsexes == 2) 
                    meanagef <- meanage
                  ylab <- labels[6]
                  plottitle2 <- paste(periodtitle, labels[7])
                  if (nareas > 1) 
                    plottitle2 <- paste(plottitle2, "in", areanames[iarea])
                  ageBubble.fn <- function() {
                    ## Hack! needed to check here so only beginning of year is shown
                    ## Also made title be nothing
                    if(iperiod == 1){
                      bubble3(x = resx, y = resy, z = resz, xlab = labels[1], 
                              ylab = labels[2], legend = bublegend, bg.open = bub.bg, 
                      ##        main = plottitle1, maxsize = (pntscalar +
                      main = "", maxsize = (pntscalar + 
                      1), las = 1, cex.main = cex.main, allopen = TRUE)
                      lines(natageyrs, meanage, col = "red", lwd = 3)
                    }
                  }
                  meanAge.fn <- function() {
                    ylim <- c(0, max(meanage, meanagef, na.rm = TRUE))
                    plot(natageyrs, meanage, col = "blue", lty = 1, 
                      pch = 4, xlab = labels[1], ylim = ylim, 
                      type = "o", ylab = ylab, main = plottitle2, 
                      cex.main = cex.main)
                    points(natageyrs, meanagef, col = "red", 
                      lty = 2, pch = 1, type = "o")
                    legend("bottomleft", bty = "n", c("Females", 
                      "Males"), lty = c(2, 1), pch = c(1, 4), 
                      col = c("red", "blue"))
                  }
                  if (plot) {
                    if (1 %in% subplots) 
                      ageBubble.fn()
                    if (2 %in% subplots & m == 2 & nsexes == 
                      2) 
                      meanAge.fn()
                  }
                  if (print) {
                    filepartsex <- paste("_sex", m, sep = "")
                    filepartarea <- ""
                    if (nareas > 1) 
                      filepartarea <- paste("_", areanames[iarea], 
                        sep = "")
                    if (1 %in% subplots) {
                      file <- paste(plotdir, "/numbers1", filepartarea, 
                        filepartsex, fileperiod, ".png", sep = "")
                      caption <- plottitle1
                      plotinfo <- pngfun(file = file, caption = caption)
                      ageBubble.fn()
                      dev.off()
                    }
                    if (2 %in% subplots & m == 2 & nsexes == 
                      2) {
                      file <- paste(plotdir, "/numbers2_meanage", 
                        filepartarea, fileperiod, ".png", sep = "")
                      caption <- plottitle2
                      plotinfo <- pngfun(file = file, caption = caption)
                      meanAge.fn()
                      dev.off()
                    }
                  }
                }
            }
        }
        if (nsexes > 1) {
            for (iarea in areas) {
                plottitle3 <- paste(labels[11], sep = "")
                if (nareas > 1) 
                  plottitle3 <- paste(plottitle3, " for ", areanames[iarea], 
                    sep = "")
                natagef <- get(paste("natagetemp0area", iarea, 
                  "sex", 1, sep = ""))
                natagem <- get(paste("natagetemp0area", iarea, 
                  "sex", 2, sep = ""))
                natagefyrs <- natagef$Yr
                natageratio <- as.matrix(natagem[, remove]/natagef[, 
                  remove])
                natageratio[is.nan(natageratio)] <- NA
                if (diff(range(natageratio, finite = TRUE)) != 
                  0) {
                  numbersRatioAge.fn <- function(...) {
                    contour(natagefyrs, 0:accuage, natageratio, 
                      xaxs = "i", yaxs = "i", xlab = labels[1], 
                      ylab = labels[2], main = plottitle3, cex.main = cex.main, 
                      ...)
                  }
                  if (plot & 3 %in% subplots) {
                    numbersRatioAge.fn(labcex = 1)
                  }
                  if (print & 3 %in% subplots) {
                    filepart <- ""
                    if (nareas > 1) 
                      filepart <- paste("_", areanames[iarea], 
                        filepart, sep = "")
                    file <- paste(plotdir, "/numbers3_ratio_age", 
                      filepart, ".png", sep = "")
                    caption <- plottitle3
                    plotinfo <- pngfun(file = file, caption = caption)
                    numbersRatioAge.fn(labcex = 0.4)
                    dev.off()
                  }
                }
                else {
                  cat("skipped sex ratio contour plot because ratio=1 for all ages and years\n")
                }
            }
        }
        if (length(intersect(6:7, subplots)) > 1) {
            column1 <- column1 - 1
            for (iarea in areas) {
                for (iperiod in 1:length(period)) {
                  for (m in 1:nsexes) {
                    natlentemp_all <- natlen[natlen$Area == iarea & 
                      natlen$Gender == m & natlen$Seas == 1 & 
                      natlen$Era != "VIRG" & natlen$Yr < (endyr + 
                      2) & natlen$BirthSeas == min(bseas), ]
                    natlentemp_all <- natlentemp_all[natlentemp_all$"Beg/Mid" == 
                      period[iperiod], ]
                    morphlist <- unique(natlentemp_all$SubMorph)
                    natlentemp0 <- natlentemp_all[natlentemp_all$SubMorph == 
                      morphlist[1] & natlentemp_all$Bio_Pattern == 
                      1, ]
                    for (ilen in 1:nlbinspop) natlentemp0[, column1 + 
                      ilen] <- 0
                    for (imorph in 1:length(morphlist)) {
                      for (igp in 1:ngpatterns) {
                        natlentemp_imorph_igp <- natlentemp_all[natlentemp_all$SubMorph == 
                          morphlist[imorph] & natlentemp_all$Bio_Pattern == 
                          igp, ]
                        natlentemp0[, column1 + 1:nlbinspop] <- natlentemp0[, 
                          column1 + 1:nlbinspop] + natlentemp_imorph_igp[, 
                          column1 + 1:nlbinspop]
                      }
                    }
                    if (ngpatterns > 0) 
                      natlentemp0$Bio_Pattern == 999
                    nyrsplot <- nrow(natlentemp0)
                    resx <- rep(natlentemp0$Yr, nlbinspop)
                    resy <- NULL
                    for (ilen in 1:nlbinspop) resy <- c(resy, 
                      rep(lbinspop[ilen], nyrsplot))
                    resz <- NULL
                    for (ilen in column1 + 1:nlbinspop) {
                      resz <- c(resz, numbers.unit * natlentemp0[, 
                        ilen])
                    }
                    units <- ""
                    if (max(resz) > 1e+09) {
                      resz <- resz/1e+09
                      units <- "billion"
                    }
                    if (max(resz) > 1e+06 & units == "") {
                      resz <- resz/1e+06
                      units <- "million"
                    }
                    if (max(resz) > 1000 & units == "") {
                      resz <- resz/1000
                      units <- "thousand"
                    }
                    assign(paste("natlentemp0area", iarea, "sex", 
                      m, sep = ""), natlentemp0)
                    if (m == 1 & nsexes == 1) 
                      sextitle <- ""
                    if (m == 1 & nsexes == 2) 
                      sextitle <- " of females"
                    if (m == 2) 
                      sextitle = " of males"
                    if (nareas > 1) 
                      sextitle <- paste(sextitle, " in ", areanames[iarea], 
                        sep = "")
                    if (period[iperiod] == "B") 
                      periodtitle <- labels[16]
                    else if (period[iperiod] == "M") 
                      periodtitle <- labels[17]
                    else stop("'period' input to SSplotNumbers should include only 'B' or 'M'")
                    plottitle1 <- paste(periodtitle, " ", labels[18], 
                      sextitle, " in (max ~ ", format(round(max(resz), 
                        1), nsmall = 1), " ", units, ")", sep = "")
                    natlentemp1 <- as.matrix(natlentemp0[, remove])
                    natlentemp2 <- as.data.frame(natlentemp1)
                    natlentemp2$sum <- as.vector(apply(natlentemp1, 
                      1, sum))
                    natlentemp0 <- natlentemp0[natlentemp2$sum > 
                      0, ]
                    natlentemp1 <- natlentemp1[natlentemp2$sum > 
                      0, ]
                    natlentemp2 <- natlentemp2[natlentemp2$sum > 
                      0, ]
                    prodmat <- t(natlentemp1) * lbinspop
                    prodsum <- as.vector(apply(prodmat, 2, sum))
                    natlentemp2$sumprod <- prodsum
                    natlentemp2$meanlen <- natlentemp2$sumprod/natlentemp2$sum - 
                      (natlentemp0$BirthSeas - 1)/nseasons
                    natlenyrs <- sort(unique(natlentemp0$Yr))
                    if (iperiod == 1) 
                      natlenyrsB <- natlenyrs
                    meanlen <- 0 * natlenyrs
                    for (i in 1:length(natlenyrs)) {
                      meanlen[i] <- sum(natlentemp2$meanlen[natlentemp0$Yr == 
                        natlenyrs[i]] * natlentemp2$sum[natlentemp0$Yr == 
                        natlenyrs[i]])/sum(natlentemp2$sum[natlentemp0$Yr == 
                        natlenyrs[i]])
                    }
                    if (m == 1 & nsexes == 2) 
                      meanlenf <- meanlenf <- meanlen
                    ylab <- labels[13]
                    plottitle2 <- paste(periodtitle, labels[14])
                    if (nareas > 1) 
                      plottitle2 <- paste(plottitle2, "in", areanames[iarea])
                    lenBubble.fn <- function() {
                      bubble3(x = resx, y = resy, z = resz, xlab = labels[1], 
                        ylab = labels[12], legend = bublegend, 
                        bg.open = bub.bg, main = plottitle1, 
                        maxsize = (pntscalar + 1), las = 1, cex.main = cex.main, 
                        allopen = TRUE)
                      lines(natlenyrs, meanlen, col = "red", 
                        lwd = 3)
                    }
                    meanLen.fn <- function() {
                      ylim <- c(0, max(meanlen, meanlenf))
                      plot(natlenyrs, meanlen, col = "blue", 
                        lty = 1, pch = 4, xlab = labels[1], ylim = ylim, 
                        type = "o", ylab = ylab, main = plottitle2, 
                        cex.main = cex.main)
                      points(natlenyrs, meanlenf, col = "red", 
                        lty = 2, pch = 1, type = "o")
                      legend("bottomleft", bty = "n", c("Females", 
                        "Males"), lty = c(2, 1), pch = c(1, 4), 
                        col = c("red", "blue"))
                    }
                    if (plot) {
                      if (6 %in% subplots) 
                        lenBubble.fn()
                      if (7 %in% subplots & m == 2 & nsexes == 
                        2) 
                        meanLen.fn()
                    }
                    if (print) {
                      filepartsex <- paste("_sex", m, sep = "")
                      filepartarea <- ""
                      if (nareas > 1) 
                        filepartarea <- paste("_", areanames[iarea], 
                          sep = "")
                      if (6 %in% subplots) {
                        file <- paste(plotdir, "/numbers6_len", 
                          filepartarea, filepartsex, ".png", 
                          sep = "")
                        caption <- plottitle1
                        plotinfo <- pngfun(file = file, caption = caption)
                        lenBubble.fn()
                        dev.off()
                      }
                      if (7 %in% subplots & m == 2 & nsexes == 
                        2) {
                        file <- paste(plotdir, "/numbers7_meanlen", 
                          filepartarea, ".png", sep = "")
                        caption <- plottitle2
                        plotinfo <- pngfun(file = file, caption = caption)
                        meanLen.fn()
                        dev.off()
                      }
                    }
                  }
                }
            }
            if (nsexes > 1) {
                for (iarea in areas) {
                  natlenf <- get(paste("natlentemp0area", iarea, 
                    "sex", 1, sep = ""))
                  natlenm <- get(paste("natlentemp0area", iarea, 
                    "sex", 2, sep = ""))
                  natlenratio <- as.matrix(natlenm[, remove]/natlenf[, 
                    remove])
                  if (diff(range(natlenratio, finite = TRUE)) != 
                    0) {
                    numbersRatioLen.fn <- function(males.to.females = TRUE, 
                      ...) {
                      if (males.to.females) {
                        main <- labels[19]
                        z <- natlenratio
                      }
                      else {
                        main <- labels[20]
                        z <- 1/natlenratio
                      }
                      if (nareas > 1) 
                        main <- paste(main, " for ", areanames[iarea], 
                          sep = "")
                      contour(natlenyrsB, lbinspop, z, xaxs = "i", 
                        yaxs = "i", xlab = labels[1], ylab = labels[12], 
                        main = main, cex.main = cex.main, ...)
                    }
                    if (plot & 8 %in% subplots) {
                      numbersRatioLen.fn(males.to.females = TRUE, 
                        labcex = 1)
                    }
                    if (plot & 9 %in% subplots) {
                      numbersRatioLen.fn(males.to.females = FALSE, 
                        labcex = 1)
                    }
                    if (print & 8 %in% subplots) {
                      filepart <- ""
                      if (nareas > 1) 
                        filepart <- paste("_", areanames[iarea], 
                          filepart, sep = "")
                      file <- paste(plotdir, "/numbers8_ratio_len1", 
                        filepart, ".png", sep = "")
                      caption <- labels[19]
                      plotinfo <- pngfun(file = file, caption = caption)
                      numbersRatioLen.fn(labcex = 0.4)
                      dev.off()
                    }
                    if (print & 9 %in% subplots) {
                      filepart <- ""
                      if (nareas > 1) 
                        filepart <- paste("_", areanames[iarea], 
                          filepart, sep = "")
                      file <- paste(plotdir, "/numbers8_ratio_len2", 
                        filepart, ".png", sep = "")
                      caption <- labels[20]
                      plotinfo <- pngfun(file = file, caption = caption)
                      numbersRatioLen.fn(labcex = 0.4)
                      dev.off()
                    }
                  }
                  else {
                    cat("skipped sex ratio contour plot because ratio=1 for all lengths and years\n")
                  }
                }
            }
        }
        equilibfun <- function() {
            equilage <- natage[natage$Era == "VIRG", ]
            equilage <- equilage[as.vector(apply(equilage[, remove], 
                1, sum)) > 0, ]
            BirthSeas <- spawnseas
            if (!(spawnseas %in% bseas)) 
                BirthSeas <- min(bseas)
            if (length(bseas) > 1) 
                cat("showing equilibrium age for first birth season", 
                  BirthSeas, "\n")
            plot(0, type = "n", xlim = c(0, accuage), ylim = c(0, 
                1.05 * max(equilage[equilage$BirthSeas == BirthSeas & 
                  equilage$Seas == BirthSeas, remove])), xaxs = "i", 
                yaxs = "i", xlab = "Age", ylab = labels[9], main = labels[10], 
                cex.main = cex.main)
            legendlty <- NULL
            legendcol <- NULL
            legendlegend <- NULL
            for (iarea in areas) {
                for (m in 1:nsexes) {
                  equilagetemp <- equilage[equilage$Area == iarea & 
                    equilage$Gender == m & equilage$BirthSeas == 
                    BirthSeas & equilage$Seas == BirthSeas, ]
                  if (nrow(equilagetemp) > 1) {
                    cat("in plot of equilibrium age composition by gender and area\n", 
                      "multiple morphs are not supported, using first row from choices below\n")
                    print(equilagetemp[, 1:10])
                  }
                  equilagetemp <- equilagetemp[1, remove]
                  lines(0:accuage, equilagetemp, lty = m, lwd = 3, 
                    col = areacols[iarea])
                  legendlty <- c(legendlty, m)
                  legendcol <- c(legendcol, areacols[iarea])
                  if (m == 1 & nsexes == 1) 
                    sextitle <- ""
                  if (m == 1 & nsexes == 2) 
                    sextitle <- "Females"
                  if (m == 2) 
                    sextitle = "Males"
                  if (nareas > 1) 
                    sextitle <- paste(sextitle, " in ", areanames[iarea], 
                      sep = "")
                  legendlegend <- c(legendlegend, sextitle)
                }
            }
            if (length(legendlegend) > 1) 
                legend("topright", legend = legendlegend, col = legendcol, 
                  lty = legendlty, lwd = 3)
        }
        if (plot & 4 %in% subplots) {
            equilibfun()
        }
        if (print & 4 %in% subplots) {
            file = paste(plotdir, "/numbers4_equilagecomp.png", 
                sep = "")
            caption <- labels[10]
            plotinfo <- pngfun(file = file, caption = caption)
            equilibfun()
            dev.off()
        }
        if (N_ageerror_defs > 0) {
            xvals <- age_error_sd$age + 0.5
            yvals <- age_error_sd[, -1]
            ylim <- c(0, max(yvals))
            if (N_ageerror_defs == 1) 
                colvec <- "black"
            else colvec <- rich.colors.short(N_ageerror_defs)
            ageingfun <- function() {
                matplot(xvals, yvals, ylim = ylim, type = "o", 
                  pch = 1, lty = 1, col = colvec, xlab = labels[3], 
                  ylab = labels[4], main = labels[8], cex.main = cex.main)
                abline(h = 0, col = "grey")
                legend("topleft", bty = "n", pch = 1, lty = 1, 
                  col = colvec, ncol = ifelse(N_ageerror_defs < 
                    20, 1, 2), legend = paste("Ageing method", 
                    1:N_ageerror_defs))
            }
            ageingbias <- age_error_mean[, -1] - (age_error_mean$age + 
                0.5)
            if (mean(ageingbias == 0) != 1) {
                ageingfun2 <- function() {
                  yvals <- age_error_mean[, -1]
                  ylim <- c(0, max(yvals))
                  matplot(xvals, yvals, ylim = ylim, type = "o", 
                    pch = 1, lty = 1, col = colvec, xlab = labels[3], 
                    ylab = labels[5], main = labels[8])
                  abline(h = 0, col = "grey")
                  abline(0, 1, col = "grey")
                  legend("topleft", bty = "n", pch = 1, lty = 1, 
                    col = colvec, ncol = ifelse(N_ageerror_defs < 
                      20, 1, 2), legend = paste("Ageing method", 
                      1:N_ageerror_defs))
                }
            }
            ageing_matrix_fun <- function(i_ageerror_def) {
                ylab <- gsub(pattern = "Mean o", replacement = "O", 
                  x = labels[5])
                agebins.tmp <- sort(unique(as.numeric(dimnames(AAK)$ObsAgeBin)))
                z <- t(AAK[i_ageerror_def, rev(1:length(agebins.tmp)), 
                  ])
                image(x = as.numeric(rownames(z)), y = as.numeric(colnames(z)), 
                  z = z, xlab = labels[3], ylab = ylab, main = paste(labels[8], 
                    ": matrix for method ", i_ageerror_def, sep = ""), 
                  axes = FALSE)
                if (accuage <= 40) {
                  axis(1, at = 0:accuage)
                  axis(2, at = agebins.tmp, las = 2)
                }
                if (accuage > 40) {
                  axis(1, at = 0:accuage, labels = rep("", accuage + 
                    1))
                  axis(1, at = seq(0, accuage, 5))
                  axis(2, at = agebins.tmp, labels = rep("", 
                    length(agebins.tmp)))
                  axis(2, at = agebins.tmp[agebins.tmp %in% seq(0, 
                    accuage, 5)], las = 2)
                }
                box()
            }
            if (plot & 5 %in% subplots) {
                ageingfun()
                if (mean(ageingbias == 0) != 1) 
                  ageingfun2()
                for (i_ageerror_def in 1:N_ageerror_defs) {
                  ageing_matrix_fun(i_ageerror_def)
                }
            }
            if (print & 5 %in% subplots) {
                file <- paste(plotdir, "/numbers5_ageerrorSD.png", 
                  sep = "")
                caption <- paste(labels[8], ": ", labels[4], 
                  sep = "")
                plotinfo <- pngfun(file = file, caption = caption)
                ageingfun()
                dev.off()
                if (mean(ageingbias == 0) != 1) {
                  file <- paste(plotdir, "/numbers5_ageerrorMeans.png", 
                    sep = "")
                  caption <- paste(labels[8], ": ", labels[5], 
                    sep = "")
                  plotinfo <- pngfun(file = file, caption = caption)
                  ageingfun2()
                  dev.off()
                }
                for (i_ageerror_def in 1:N_ageerror_defs) {
                  file <- paste(plotdir, "/numbers5_ageerror_matrix_", 
                    i_ageerror_def, ".png", sep = "")
                  caption <- paste(labels[8], ": matrix for method ", 
                    i_ageerror_def, sep = "")
                  plotinfo <- pngfun(file = file, caption = caption)
                  ageingfun()
                  ageing_matrix_fun(i_ageerror_def)
                  dev.off()
                }
            }
        }
    }
    if (!is.null(plotinfo)) 
        plotinfo$category <- "Numbers"
    return(invisible(plotinfo))
}
