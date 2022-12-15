make.selex.plot <- function(model,
                            subplot = 2,
                            agefactors = "Asel",
                            #years = c(1966, end_yr-2, end_yr-1),
                            years = c(1966, 2016, 2017),
                            fleetnames = c("Fishery","Survey"),
                            showmain = FALSE,
                            pan.letter = NULL){  ## If not null, this character will be placed on the panel
  ## Make a single selectivity plot for the MLE model provided

  # this function is a bit of a hack and in the future
  # it might be better to just copy the plotAllSel function
  # within SSplotSelex from r4ss into hake-assessment

  # fishery selectivity is reported for start year and all time-varying years
  # survey selectivity is reported for the start year and the end year
  # so input years = c(1966, 2016, 2017) will return 3 lines for the fishery
  # and 2 lines for the survey, the second of which is redundant so omitted from plot

  # vector of colors
  cols=c('blue', rgb(0,seq(0.8,0.6,length=2),0.8), rgb(1,0,0,.7), NA)
  # table of plot characteristics, which matches format
  # of output returned (invisibly) by SSplotSelex
  infotable <- data.frame(Fleet  = c(1,1,1,2,2),
                          Sex    = 1,
                          Yr     = years[c(1,2,3,1,2)],
                          ifleet = c(1,1,1,2,2),
                          FleetName = c(rep("Fishery", 3), "Survey", ""),
                          longname  = c("Fishery (avg.)",
                                        "Fishery 2016",
                                        "Fishery 2017",
                                        "Survey", ""),
                          Yr_range  = c(1966, 2017:2018, 1966, end_yr-1),
                          col       = cols,
                          lty       = c(1,2,4,1,1),
                          lwd       = c(4,2,2,3,NA),
                          pch       = c(NA,NA,NA,NA,NA),
                          stringsAsFactors=FALSE)
  ## Remove rows where col is NA. This is a hack to fix when the model doesn't have age data for the last year
  infotable <- infotable[!is.na(infotable$col),]
  SSplotSelex(model, infotable=infotable, subplot = subplot, agefactors = agefactors,
              years = years, fleetnames = fleetnames, showmain = showmain)
  if(!is.null(pan.letter)){
    panel.letter(pan.letter)
  }
}

calc.tv.selex <- function(model,
                          start_yr,
                          end_yr,
                          ages = 1:8,  ## ages to be included
                          probs = c(0.025, 0.975)){
  ## Calculate the data frame to use for the functions involving time varying selectivity
  ## Returns a list of length 4: The overall selex object, and data frames of median, lower, and upper quantiles
  ##  as calculated for the given probablities. The columns in the data frames are the years
  ##  and the rows are ages. Both row names and column names have been set properly.

  yrs <- start_yr:end_yr
  selex <- list()

  ## changing indexing from 2:length(yrs) to 1:length(yrs)
  for(i in 1:length(yrs)){
    ## calculate selectivity across MCMC for given year
    selexYear <- selexYear.fn(model$mcmc, yrs[i])
    ## if selexYear.fn returns NA, there was no deviation in that year,
    ## calculate average selectivity for non-deviation years
    if(is.null(selexYear)) {
      ## empty matrix
      selex[[1]] <- matrix(NA, nrow = nrow(model$mcmc), ncol = 16)
      ## loop over MCMC samples
      for(i in 1:nrow(model$mcmc)) {
        ## get the base selectivity parameters
        ind <- grep("AgeSel_P[1-9]_Fishery.1.", names(model$mcmc))[1:5]
        ## calculation the selectivity function for each sample
        selex[[1]][i,] <- randWalkSelex.fn(unlist(c(-1000,
                                                    0, model$mcmc[i, ind],
                                                    0, 0, 0, 0, 0, 0, 0, 0, 0)))
      }
    } else { ## not NULL
      # selexYear.fn provided time-varying selectivity for the given year
      selex[[i]] <- selexYear
    }
  }
  ## Strip off unneeded ages
  for(i in 1:length(selex)){
    ## Remove the first column because it is age zero fish
    selex[[i]] <- selex[[i]][,-1]
    cols.to.remove <- NULL
    if(ages[1] > 1){
      cols.to.remove <- (1:(ages[1]-1))
    }
    if(ages[length(ages)] < ncol(selex[[i]])){
      cols.to.remove <- c(cols.to.remove, (ages[length(ages)]+1):ncol(selex[[i]]))
    }
    if(!is.null(cols.to.remove)){
      selex[[i]] <- selex[[i]][,-cols.to.remove]
    }
    if(ncol(selex[[i]]) == length(ages)){
      colnames(selex[[i]]) <- ages
    }else{
      cat("calc.tv.selex: Warning - length of ages vector (",length(ages),
          ") not equal to the number of columns in the selectivity data (",
          ncol(selex[[i]]),") for year ",yrs[i],"\n", sep="")
    }
  }

  selex.median <- as.data.frame(lapply(selex, function(x){apply(x, 2, median)}))
  if(length(selex.median) + 1 == length(yrs)){
    yrs <- yrs[-length(yrs)]
  }
  names(selex.median) <- yrs
  selex.lower <- as.data.frame(lapply(selex, function(x){apply(x, 2, quantile, prob = probs[1])}))
  names(selex.lower) <- yrs
  selex.upper <- as.data.frame(lapply(selex, function(x){apply(x, 2, quantile, prob = probs[2])}))
  names(selex.upper) <- yrs

  return(list(selex = selex,
              lower = selex.lower,
              median = selex.median,
              upper = selex.upper))
}

make.tv.selex.plot <- function(selex.list,
                               oma=c(0,8,0,8), # bad way to make the plot skinnier
                               mcmc=TRUE){
  # note from Ian: use of outer margins to make plot skinnier was a hack
  #                that I used because I didn't understand the figure layout options.
  #                This should be replaced by specifying fig width and height.

  # input selex.list is a list of time varying selectivites
  # as returned by calc.tv.selex (in the mcmc case)
  # or just the model object (in the MLE case)

  ## Plot the time-varying selectivity of model
  oldpar <- par()

  # padding left and right outer margins using oma to make plot less stretched out
  par(mar=c(4,4,1,1), oma=oma)
  if(mcmc){
    selex.dat <- t(selex.list$median)
  }else{
    # in the MLE case, selex.list is the model object created by SS_output
    agesel <- selex.list$agesel # get selectivity at age
    selex.dat <- agesel[agesel$Factor=="Asel" &
                          agesel$Fleet==1 &
                            agesel$Yr %in% 1990:last.data.yr, ] # subset rows
    rownames(selex.dat) <- selex.dat$year # add row names for year
    selex.dat <- selex.dat[,paste(1:8)] # subset columns
  }
  mountains(selex.dat,
            yvec = as.numeric(rownames(selex.dat)),
            rev = TRUE,
            cex.axis = 0.8)
  mtext(side=1,line=2,"Age")
  mtext(side=2,line=3,"Selectivity by year")

  par <- oldpar
}

make.tv.selex.uncertainty.plot <- function(selex.list  ## A list of time varying selectivites as returned by calc.tv.selex
                                           ){
  ## Plot the time-varying selectivity of model with uncertainty
  oldpar <- par()

  par(mar=c(4,4,1,1))

  single.yr.sel <- function(sel.med,   ## vector of selectivity medians for any given year
                            sel.lower, ## vector of selectivity lower quantile for any given year
                            sel.upper, ## vector of selectivity upper quantile for any given year
                            year,
                            ages,
                            yAdjust){
    lines(ages, yAdjust + sel.med, type="b", pch = 20)
    segments(x0 = ages, y0 = yAdjust + sel.upper,
             x1 = ages, y1 = yAdjust + sel.lower)
    polygon(x = c(ages, rev(ages)),
            y = yAdjust + c(sel.upper, rev(sel.lower)),
            col = rgb(0, 0, 1, 0.2), lty = 3)
  }

  selex.med <- selex.list$median
  selex.lower <- selex.list$lower
  selex.upper <- selex.list$upper

  ages <- as.numeric(rownames(selex.med))
  yrs <- as.numeric(colnames(selex.med))

  plot(0,
       type = "n",
       xlim = c(min(ages), max(ages)),
       ylim = -1 * (max(yrs) - c(0, length(yrs))),
       yaxt = "n",
       pch = 20,
       xlab = "",
       ylab = "")
  label <- yrs
  axis(2, las = 1, at = -yrs + 0.5, lab = label)
  for(y in yrs){
    single.yr.sel(selex.med[,names(selex.med) %in% y],
                  selex.lower[,names(selex.lower) %in% y],
                  selex.upper[,names(selex.upper) %in% y],
                  y,
                  ages,
                  yAdjust = -y)
  }
  abline(h = -c(min(yrs)-1, yrs), col = rgb(0, 0, 0, 0.2))

  par <- oldpar
}

make.multiple.tv.selex.uncertainty.plots <- function(tv.sel.list ## A list of outputs from calc.tv.selex function
                                                     ){
  ## Takes a list of outputs from the calc.tv.selex function, and calls
  ##  make.tv.selex.uncertainty.plot for each of the items, placing
  ##  them side-by-side with single labels for Age and Selectivity by year.
  ## This allows the user to select how they want to break up the plots by year

  oldpar <- par()
  par(mfrow=c(1, length(tv.sel.list)), oma=c(1,1,0,0))
  for(i in 1:length(tv.sel.list)){
    make.tv.selex.uncertainty.plot(tv.sel.list[[i]])
  }
  mtext(side = 1, line = -1, outer = TRUE, text="Age")
  mtext(side = 2, outer = TRUE, text="Selectivity by year")
  par <- oldpar
}

make.selex.uncertainty.lines.plot <- function(model,
                                              year = end_yr - 1, # final fishing year
                                              type = 1, # 1 = Fishery, any other value is the survey
                                              selex.list = NULL, # A list of time varying selectivites as returned by calc.tv.selex
                                                                 # *Only used when type=1 (fishery)
                                              probs = c(0.025, 0.975)
                                              ){
  ## Plots estimated selectivity lines from each sample in the posterior distribution
  if(type == 1 & is.null(selex.list)){
    stop("make.mcmc.selex.uncertainty.plot: Error - when type = 1, you must supply a selex.list.\n")
  }
  oldpar <- par()
  par(mar=c(4, 4, 1, 1))
  sel.med <- selex.list$median
  yrs <- as.numeric(names(sel.med))
  if(type == 1){
    ## get matrix of selectivity at age from all MCMC samples for chosen year
    if(!year %in% yrs){
      year = yrs[length(yrs)]
    }
    selex <- selex.list$selex[[which(yrs==year)]]
    ## get vectors of medians and intervals for chosen year
    selex.med <- selex.list$median[,which(yrs==year)]
    selex.lower <- selex.list$lower[,which(yrs==year)]
    selex.upper <- selex.list$upper[,which(yrs==year)]
    seg.color <- rgb(0.1, 0.1, 1, 0.8)
    ages <- seq_len(ncol(selex))
  }else{
    # survey catch is simpler because it doesn't change over time
    selex <- model$mcmc[,grep("Selex_std_2_Fem_A_", names(model$mcmc))]
    selex.med <- apply(selex,2,median)
    selex.lower <- apply(selex, 2, quantile, prob = probs[1])
    selex.upper <- apply(selex, 2, quantile, prob = probs[2])
    seg.color <- rgb(1,0.1,0.1,0.8)
    ages <- as.numeric(gsub("^.*([0-9])$", "\\1", names(selex)))
  }

  # make empty plot
  plot(ages,
       selex.med[ages],
       type = "n",
       ylim = c(0, 1),
       xlim = c(1, 8),
       pch = 20,
       xlab = "Age",
       ylab = "Selectivity",
       xaxt = "n",
       las = 1)
  # loop over MCMC samples and make lines for each one
  for(i in 1:nrow(selex)) {
    lines(ages, selex[i, seq_along(ages)], col = rgb(0, 0, 0, 0.1))
  }
  # add vertical segments
  segments(ages,
           selex.upper[seq_along(ages)],
           ages,
           selex.lower[seq_along(ages)],
           col = seg.color,
           lwd = 3)
  # add points with connecting lines
  points(ages,
         selex.med[seq_along(ages)],
         type='b',
         lwd = 2,
         pch = 16,
         cex = 1.5,
         col = seg.color)
  # add x-axis
  axis(1, at = ages)
  # restore parameters
  par <- oldpar

}
