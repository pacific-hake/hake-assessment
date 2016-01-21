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
  par(mar=c(5.1, 4.1, 0, 2.1))
  SSplotComps(model,
              kind = "AGE",
              subplot = 24,
              printmkt = FALSE,
              printsex = FALSE,
              fleetnames = c("Fishery","Survey"))
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
                                      inches = 0.12
                                      ){
  ## Plot the age compositions for whatever subplot is set to
  ## Returns a vector of the start.yr, end.yr, max. proportion,
  ##  year of max. proportion, age of max. proportion.
  oldpar <- par()
  if(show.key){
    if(is.null(key.yrs)){
      stop("make.age.comp.bubble.plot: Error - you must supply a key.yrs vector of 4 years when specifying show.key = TRUE.\n")
    }else{
      if(length(key.yrs) != 4){
        stop("make.age.comp.bubble.plot: Error - key.yrs must be a vector of exactly 4 years when specifying show.key = TRUE.\n")
      }
    }
    par(mar = c(2.1, 4.1, 3.1, 4.1), cex.axis = 0.9)
  }else{
    par(mar = c(2.1, 4.1, 1.1, 4.1), cex.axis = 0.9)
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
            circles = sqrt(c(0.01, 0.1, 0.2, 0.4, max.prop)),
            inches = inches,
            add = TRUE,
            xpd = NA,
            fg = fg,
            bg = bg)
    text(key.yrs + 1.9, c(16.2,16.2,16.2,16.2), c("0.01", "0.1", "0.2", "0.4"), xpd = NA, cex = 0.8)
  }
  axis(1, seq(start.yr, end.yr + 5, 5))
  axis(4)
  par <- oldpar
  ret.vec <- c(start.yr, end.yr, max.prop, which.max.prop)
  names(ret.vec) <- c("start.yr", "end.yr", "max.prop", "max.prop.yr", "max.prop.age")
  return(ret.vec)
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
  age.fits(dat = model,
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
                  titletype <- "Pearson residuals, "
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
                  ptitle <- paste(titletype, title_sexmkt, ", comparing across fleets", 
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
