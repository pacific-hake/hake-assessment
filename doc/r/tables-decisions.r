make.decision.table <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                xcaption   = "default", ## Caption to use
                                xlabel     = "default", ## Latex label to use
                                font.size  = 9,         ## Size of the font for the table
                                space.size = 10,        ## Size of the spaces for the table
                                which      = "biomass"  ## Which type to build. "biomass" or "spr".
                                ){
  ## Returns an xtable in the proper format for the executive summary decision tables

  if(which != "biomass" & which != "spr"){
    stop("make.decisions.table: Error - type '",which,"' is not implemented. Stopping...\n\n")
  }
  if(which == "biomass"){
    forecast <- model$forecasts$biomass
  }else{
    forecast <- model$forecasts$spr
  }

  ## tab.letters are the letters in the table, one for each forecast management action
  ## and a blank for all but the first year in a management action
  tab.letters <- NULL
  next.ind <- 1
  for(i in 1:length(forecast)){
    tab.letters[next.ind] <- letters[i]
    next.ind <- next.ind + 1
    for(j in 1:(nrow(forecast[[i]])-1)){
      tab.letters[next.ind] <- ""
      next.ind <- next.ind + 1
    }
  }

  ## Merge the list elements into a data frame
  forecast.tab <- fmt0(do.call("rbind", forecast) * 100)

  ## Store years for binding later
  yrs <- rownames(forecast.tab)

  ## Append the escaped % symbols
  forecast.tab <- apply(forecast.tab, 2, paste0, "\\%")


  ## Change the quantile levels so they have correct latex escape sequence
  quant.levels <- gsub("%","\\\\%",colnames(forecast.tab))

  ## Set any catch less than 1 to be 0
  c.levels <- unlist(catch.levels)
  c.levels[c.levels < 1] <- 0
  ## Bind the catch levels and years to the correct rows
  forecast.tab <- cbind(tab.letters, yrs, fmt0(c.levels), forecast.tab)
  colnames(forecast.tab) <- c("",
                              "Year",
                              "Catch (t)",
                              quant.levels)

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(forecast.tab)

  quant.string <- ""
  for(i in 1:length(quant.levels)){
    quant.string <- paste0(quant.string, "& ", quant.levels[i], " ")
  }
  addtorow$command <- c(paste0("\\toprule \n",
                               "\\multicolumn{3}{c}{Within model quantile} ",
                               quant.string, " \\\\ ",
                               "\\hline",
                               " & Year & Catch (t) & \\multicolumn{5}{c}{Beginning of year relative spawning biomass} \\\\ ",
                               "\\midrule \n"),
                        "\\bottomrule \n")
  ## Add the right number of horizontal lines to make the table break in the correct places
  ## A line is not needed at the bottom explains the (length(forecast)-1) statement.
  for(i in 1:(length(forecast)-1)){
    addtorow$pos[[i+2]] <- i * nrow(forecast[[i]])
    addtorow$command <- c(addtorow$command, "\\hline ")
  }
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(forecast.tab,
                      caption=xcaption,
                      label=xlabel,
                      align=get.align(ncol(forecast.tab), first.left=FALSE, just="c")),
               caption.placement = "top",
               include.rownames=FALSE,
               include.colnames=FALSE,
               sanitize.text.function=function(x){x},
               size=size.string,
               add.to.row=addtorow,
               table.placement="H",
               hline.after=NULL,
               booktabs=TRUE))
}




################################################################
HakeDecisionTablesQuantiles.ex <- function(models,
                                           years,
                                           outVar="Bratio_",
                                           quantiles=c(0.05,0.25,0.5,0.75,0.95),
                                           scalar=1,
                                           csvFileName=NULL,
                                           sort=FALSE){
    ## Creates decision tables from multiple model runs
    ## Uses the posterior of the 2008 year class

    nmodels <- length(models)

    outVals <- NULL
    outNotCatch <- NULL
    for(i in 1:nmodels) {
        #sort the mcmc by the sorting variable
        xx <- models[[i]]
        xx.list <- list(mcmc=xx)
        ## Get the values of interest, but grab only the middle column (median)
        vals <- DerivedQuants.ex(models=list(xx.list),years=years,variable=outVar,mcmc=rep(T,length(xx.list)),probs=quantiles)
        vals <- vals/scalar
        catch <- apply(xx[,paste("ForeCatch_",years,sep="")],2,function(x){(sum(!duplicated(x))-1)/length(x)})
        cat("The proportion of catches that are not equal (if fixed, some runs could not take catch) for model",i,"\n",catch,"\n",sep=" ")
        medCatch <- apply(xx[,paste("ForeCatch_",years,sep="")],2,median)
        catch <-  cbind(years,medCatch,catch)
        vals <- cbind(years,medCatch,vals)
        outVals <- rbind(outVals,vals)
        outNotCatch <- rbind(outNotCatch,catch)
    }
    if(sort) {
        outVals <- outVals[order(outVals[,"medCatch"],outVals[,"years"]),]  #sort by median catch then year
        outNotCatch <- outNotCatch[order(outNotCatch[,"medCatch"],outNotCatch[,"years"]),]  #sort by median catch then year
    }
    if(is.null(csvFileName)) {
        return(list(outVals,outNotCatch))
    }else {
        print(substring(csvFileName,nchar(csvFileName)-3))
        if(substring(csvFileName,nchar(csvFileName)-3) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        print(outNotCatch)
        invisible(outVals)
    }
}

HakeDecisionTablesSort.ex <- function(models,
                                      years=2013:2014,
                                      sortVar="Recr_2010",
                                      outVar="Bratio_",
                                      percentages=c(0.10,0.20,0.40,0.20,0.10),
                                      scalar=1,
                                      csvFileName=NULL) {
    #creates decision tables from multiple model runs
    #uses the posterior of defined by the sortVar to sort by quantiles
    
    nmodels <- length(models)
    if(sum(percentages) != 1) cat("WARNING: your percentages do not sum to one\n")
    cumPerc <- c(0,cumsum(percentages))

    outVals <- NULL
    outNotCatch <- NULL
    metrics <- NULL
    for(i in 1:nmodels) {
        #sort the mcmc by the sorting variable
        xx <- models[[i]][order(models[[i]][,sortVar]),]
        xx.list <- NULL
        for(j in 1:(length(cumPerc)-1)) {
            print(((ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1])))
            if(i==1) print(length((ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1])))
            xx.list[[j]] <- list(mcmc=xx[(ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1]),])
        }
        print(names(xx.list[[1]]))
        metrics <- rbind(metrics,HakeMetricsTableRisk2(xx.list,year=max(years),decPlaces=4))
        #get the values of interest, but grab only the middle column (median)
        vals <- DerivedQuantsTables.ex(models=xx.list,years=years,variable=outVar,mcmc=rep(T,length(xx.list)))
        vals <- vals[,grep("0\\.5",colnames(vals))]/scalar
        colnames(vals) <- percentages
        catch <- DerivedQuantsTables.ex(models=xx.list,years=years,variable="ForeCatch_",mcmc=rep(T,length(xx.list)))
        catch <- catch[,grep("0\\.5",colnames(catch))]
        #print(catch)
        if(any(apply(catch,1,function(x){x!=x[1]}),na.rm=T)) cat("WARNING: some catches are not equal for the various partitions\n")
        medCatch <- median(xx[,paste("ForeCatch_",years[1],sep="")])
        notCatch <- cbind(years,medCatch,t(apply(catch,1,function(x,medC){x!=medC},medC=medCatch)))
        vals <- cbind(years,medCatch,vals)
        outVals <- rbind(outVals,vals)
        outNotCatch <- rbind(outNotCatch,notCatch)
    }
    outVals <- outVals[order(outVals[,"medCatch"],outVals[,"years"]),]  #sort by median catch then year
    outNotCatch <- outNotCatch[order(outNotCatch[,"medCatch"],outNotCatch[,"years"]),]  #sort by median catch then year
    dimnames(outVals) <- dimnames(outNotCatch) <- list(rep(years,nmodels),c("Year","Catch",as.character(percentages)))
    rownames(metrics) <- rep(percentages,nrow(metrics)/length(percentages))

    if(is.null(csvFileName)) {
        return(list(outVals=outVals,outNotCatch=outNotCatch,metrics=metrics))
    }else {
        print(substring(csvFileName,nchar(csvFileName)-3))
        if(substring(csvFileName,nchar(csvFileName)-3) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        print(outNotCatch)
        invisible(list(outVals=outVals,outNotCatch=outNotCatch,metrics=metrics))
    }
    
}
