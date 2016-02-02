run.partest.model <- function(model,
                              output.file, ## The model object will be stored in binary form here
                              verbose = TRUE){
  ## To ensure integration with the knitr loading step, you must
  ## run this from the Rgui (after you've got a base model loaded) like this:
  ##
  ## run.partest.model(base.model, "model-partest.RData")
  ##
  ## This Re-runs the model (MLE) once for each posterior
  ## and fetches information from their respective Report.sso files.
  ## This is to be run once for the base model, and stored as a binary as
  ## shown above.
  if(!verbose){
    flush.console
    cat("\nRunning partest. Screen may not show output for a while\n\n")
  }

  ## Create the directory partest which will hold the runs
  ##  erasing the directory recursively if necessary
  partest.dir <- file.path(model$path, "partest")
  reports.dir <- file.path(partest.dir, "reports")
  unlink(partest.dir, recursive=TRUE)
  dir.create(partest.dir)
  dir.create(reports.dir)

  ## Copy all mcmc model files into the partest directory
  mcmc.dir <- model$mcmcpath
  file.copy(file.path(mcmc.dir, list.files(mcmc.dir)), partest.dir)
  posts <- read.table(file.path(partest.dir, "posteriors.sso"), header = TRUE)
  ## Change this for testing on smaller subset of posteriors
  ## num.posts <- 10
  num.posts <- nrow(posts)

  ## create a table of parameter values based on labels in parameters section of Report.sso
  newpar <- data.frame(value = c(1, model$parameters$Value),
                       hash = "#",
                       label = c("dummy_parm", model$parameters$Label),
                       stringsAsFactors = FALSE)

  ## add hash before first column name
  names(newpar)[1] <- "#value"

  ## change label for R0 parameter to match R's conversion in "posts"
  newpar$label[newpar$label == "SR_LN(R0)"] <- "SR_LN.R0."

  ## write table of new files
  write.table(x = newpar,
              file = file.path(partest.dir, "ss3.par"),
              quote = FALSE, row.names=FALSE)

  start <- SS_readstarter(file.path(partest.dir, "starter.ss"), verbose=verbose)
  ## Change starter file to read from par file
  start$init_values_src <- 1
  SS_writestarter(start, dir = partest.dir, file = "starter.ss", overwrite = TRUE, verbose=F)

  ## loop over rows of posteriors file
  for(irow in 1:num.posts){
    if(verbose) {print(irow)}
    ## replace values in newpar table with posteriors values
    ## (excluding 1 and 2 for "Iter" and "Objective_function")
    newpar[newpar$label %in% names(posts), 1] <- as.numeric(posts[irow, -(1:2)])
    write.table(x = newpar,
                file = file.path(partest.dir, "ss3.par"),
                quote = FALSE,
                row.names = FALSE)

    file.copy(file.path(partest.dir, "ss3.par"),
              file.path(reports.dir, paste0("ss3_input", irow, ".par")),
              overwrite = TRUE)
    shell.command <- paste0("cd ", partest.dir, " & ss3 -maxfn 0 -phase 10 -nohess")
    if(verbose){
      ## shell doesn't accept the argument show.output.on.console for some reason
      shell(shell.command)
    }else{
      ## This doesn't work!!
      system(shell.command, show.output.on.console = FALSE)
    }
    file.copy(file.path(partest.dir, "ss3.par"),
              file.path(reports.dir, paste0("ss3_output", irow, ".par")),
              overwrite = TRUE)
    file.copy(file.path(partest.dir, "Report.sso"),
              file.path(reports.dir, paste0("Report_", irow, ".sso")),
              overwrite = TRUE)
    file.copy(file.path(partest.dir, "CompReport.sso"),
              file.path(reports.dir, paste0("CompReport_", irow, ".sso")),
              overwrite = TRUE)
  }

  ## make table to store likelihood components
  like.info <- data.frame(Iter = posts$Iter, stringsAsFactors = FALSE)
  for(lab in c("TOTAL",
               "Equil_catch",
               "Survey",
               "Age_comp",
               "Recruitment",
               "Forecast_Recruitment",
               "Parm_priors",
               "Parm_devs",
               "Crash_Pen",
               "Age_comp_surv",
               "Age_comp_fishery")){
    like.info[[lab]] <- 0
  }

  for(irow in 1:num.posts){
    tmp <- readLines(file.path(reports.dir, paste0("Report_", irow,".sso")))
    skip.row <- grep("LIKELIHOOD", tmp)[2]
##    browser()
    likes <- read.table(file.path(reports.dir, paste0("Report_", irow, ".sso")),
                        skip = skip.row,
                        nrows = 17,
                        fill = TRUE,
                        row.names = NULL,
                        col.names = 1:4,
                        stringsAsFactors = FALSE)
    like.info[irow, 2:10] <- as.numeric(likes$X2[3:11])  ## fleet-aggregated likelihoods
    like.info[irow, 11] <- as.numeric(likes[17, 3])      ## fleet-specific age comp likelihoods
    like.info[irow, 12] <- as.numeric(likes[17, 4])      ## fleet-specific age comp likelihoods
  }

  if(verbose){
    cat("\n\nReading comp table\n\n")
    flush.console()
  }
  ## read expected proportions and Pearson values for each age comp observations
  tmp <- readLines(file.path(reports.dir, paste0("CompReport_", irow,".sso")))
  skip.row <- grep("Composition_Database", tmp)
  comp.table <- read.table(file.path(partest.dir, "CompReport.sso"),
                           skip = skip.row,
                           header = TRUE,
                           fill = TRUE,
                           stringsAsFactors = FALSE)
  ## loop to create columns Exp1, Exp2, ..., Exp999 and Pearson1, Pearson2, etc.
  for(irow in 1:num.posts){
    if(irow %% 100 == 0){
      print(irow)
    }
    tmp <- readLines(file.path(reports.dir, paste0("CompReport_", irow,".sso")))
    skip.row <- grep("Composition_Database", tmp)
    comps <- read.table(file.path(reports.dir, paste0("CompReport_", irow, ".sso")),
                        skip = skip.row,
                        header = TRUE,
                        fill = TRUE,
                        stringsAsFactors = FALSE)
    lab1 <- paste0("Pearson",irow)
    lab2 <- paste0("Exp",irow)
    comp.table[lab1] <- comps$Pearson
    comp.table[lab2] <- comps$Exp
  }

  ## filter out values that are not included in agedbase within base model
  comp.table <- comp.table[!is.na(comp.table$N) & comp.table$N>0,]

  ## median and quantiles of expected values and Pearsons
  exp.table <- comp.table[,names(comp.table) %in% paste0("Exp",1:num.posts)]
  Pearson.table <- comp.table[,names(comp.table) %in% paste0("Pearson",1:num.posts)]
  exp.median <- apply(exp.table, MARGIN=1, FUN=median)
  exp.low    <- apply(exp.table, MARGIN=1, FUN=quantile, probs=0.025)
  exp.high   <- apply(exp.table, MARGIN=1, FUN=quantile, probs=0.975)
  Pearson.median <- apply(Pearson.table, MARGIN=1, FUN=median)
  Pearson.low    <- apply(Pearson.table, MARGIN=1, FUN=quantile, probs=0.025)
  Pearson.high   <- apply(Pearson.table, MARGIN=1, FUN=quantile, probs=0.975)

  ## confirm that values match between mcmc tables and base model MLE table
  ## table(base$agedbase$Obs == comp.table$Obs)
  ## TRUE
  ##  750
  if(verbose){
    cat("\n\nReading cpue table\n\n")
    flush.console()
  }
  cpue.table <- NULL
  for(irow in 1:num.posts){
    tmp <- readLines(file.path(reports.dir, paste0("Report_", irow,".sso")))
    skip.row <- grep("INDEX_2", tmp)[2]
    cpue <- read.table(file.path(reports.dir, paste0("Report_", irow,".sso")),
                       skip = skip.row,
                       nrows = model$dat$N_cpue, ## number of survey index points
                       header = TRUE,
                       fill = TRUE,
                       stringsAsFactors = FALSE)
    lab1 <- paste0("Exp", irow)
    cpue.table <- cbind(cpue.table, cpue$Exp)
  }
  model.partest <- model

  model.partest$agedbase$Exp <- exp.median
  model.partest$agedbase$Exp.025 <- exp.low
  model.partest$agedbase$Exp.975 <- exp.high
  model.partest$agedbase$Pearson <- Pearson.median
  model.partest$agedbase$Pearson.025 <- Pearson.low
  model.partest$agedbase$Pearson.975 <- Pearson.high

  model.partest$cpue.table <- cpue.table
  model.partest$cpue.median <- apply(cpue.table, MARGIN = 1, FUN = median)
  model.partest$cpue.025 <- apply(cpue.table, MARGIN = 1, FUN = quantile, probs = 0.025)
  model.partest$cpue.975 <- apply(cpue.table, MARGIN = 1, FUN = quantile, probs = 0.975)

  model.partest$like.info <- like.info
  save(model.partest, file = output.file)
}

number.to.word <- function(x, th = FALSE, cap.first = FALSE){
  ## https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  ## if th is TRUE, the th version will be returned, e.g. 4 = fourth
  ## if cap.first is TRUE, the first letter will be capitalized
  helper <- function(x){
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
        else trim(paste(tens[digits[2]],
                        Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
  }
  trim <- function(text){
    ## Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    ## Clear any trailing " and"
    text=gsub(" and$","",text)
    ##Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  ## Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  j <- helper(x)
  ## Cgrandin added the 'th' bit
  if(th){
    j <- strsplit(j, " ")[[1]]
    first <- j[-length(j)]
    last <- j[length(j)]
    if(last == "one"){
      last <- "first"
    }else if(last == "two"){
      last <- "second"
    }else if(last == "three"){
      last <- "third"
    }else if(last == "five"){
      last <- "fifth"
    }else if(last == "eight"){
      last <- "eighth"
    }else if(last == "nine"){
      last <- "ninth"
    }else if(last == "twelve"){
      last <- "twelfth"
    }else if(last == "twenty"){
      last <- "twentieth"
    }else if(last == "thirty"){
      last <- "thirtieth"
    }else if(last == "forty"){
      last <- "fortieth"
    }else if(last == "fifty"){
      last <- "fiftieth"
    }else if(last == "sixty"){
      last <- "sixtieth"
    }else if(last == "seventy"){
      last <- "seventieth"
    }else if(last == "eighty"){
      last <- "eightieth"
    }else if(last == "ninety"){
      last <- "ninetieth"
    }else{
      last <- paste0(last, "th")
    }
    j <- paste(c(first, last), collapse = " ")
  }
  if(cap.first){
    j <- paste0(toupper(substr(j, 1, 1)), substr(j, 2, nchar(j)))
  }
  return(j)
}

cbind.fill <- function(...){
  ## equivalent of cbind(df, xx) where df is an empty data frame.
  nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

strip.columns <- function(vec, names){
  ## Return a vector which is the same as the vector 'vec'
  ## but with the matching col.names removed
  return(vec[!names(vec) %in% names])
}

install.packages.if.needed <- function(package.name, package.install.name, github=FALSE){
  if(github){
    if(!(package.name %in% rownames(installed.packages()))){
      devtools::install_github(package.install.name)
    }
  }else{
    if(!(package.name %in% rownames(installed.packages()))){
      install.packages(package.install.name)
    }
  }
}

fmt0 <- function(x, dec.points=0){
  ## Format 0
  ## Format x to have supplied number of decimal points
  ## Make thousands seperated by commas and no decimal point
  return(format(round(x,dec.points),big.mark=",",nsmall=dec.points))
}

get.align <- function(num,
                      first.left = TRUE, ## Keep the first column left-justified
                                         ## If FALSE, it will be justified according to the 'just' argument
                      just = "r"         ## just is the justification to use for the columns, "r", "l", or "c"
                      ){
  ## Returns a character vector used in the align argument of the xtable command.
  ## e.g. posterior output tables, reference point tables. Most tables really.
  ## num is the number of columns in the table
  if(first.left){
    align <- c("l", "l")
  }else{
    align <- c(just, just)
  }
  for(i in 1:(num-1)){
    align <- c(align, just)
  }
  return(align)
}

rc <- rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

plotBars.fn <- function(x,y,gap=0,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value,...)
  if(add) points(x,y$value,...)
  segments(x,y$lo,x,y$value-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi,x,y$value+gap,col=ciCol,lty=ciLty,lwd=ciLwd)
}

plotBars.fn <- function(x,y,gap=0,scalar=1e6,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
  ## x is the x axis values (i.e., years)
  ## y is a data frame with:
  ## value: estimate (point) to plot
  ## lo: lower CI
  ## hi: higher CI

  if(!add) plot(x,y$value/scalar,...)
  if(add) points(x,y$value/scalar,...)
  segments(x,y$lo/scalar,x,y$value/scalar-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
  segments(x,y$hi/scalar,x,y$value/scalar+gap,col=ciCol,lty=ciLty,lwd=ciLwd) 
}

addpoly <- function(yrvec, lower, upper, color = 1, shade.col = NULL){
  lower[lower<0] <- 0 ## max of value or 0
  if(is.null(shade.col)){
    shade.col <- rgb(t(col2rgb(color)), alpha = 0.2 * 255, maxColorValue = 255)
  }
  polygon(x = c(yrvec, rev(yrvec)),
          y = c(lower, rev(upper)),
          border = NA,
          col = shade.col)
  lines(yrvec, lower, lty = 3, col = color)
  lines(yrvec, upper, lty = 3, col = color)
}

randWalkSelex.fn <- function(pars,devs=NULL,bounds=NULL) {
  ## calculates the selectivity from the random walk parameters in SS (option 17)
  ## -1000 means to set equal to 0
  ## assumes that this is all pars from age 0 to max age

  logS <- rep(NA,length(pars))
  logS[1] <- 0 #first value is never estimated (age 0)
  if(!is.null(devs)) {
    ## transform parameters based on bounds
    for(a in 2:length(pars)) {
      if(!is.na(devs[a])) {
        tmp <- log((bounds[2]-bounds[1]+0.0000002)/(pars[a]-bounds[1]+0.0000001)-1)/(-2)
        tmp <- tmp + devs[a]
        pars[a] <- bounds[1]+(bounds[2]-bounds[1])/(1+exp(-2*tmp))
      }
    }
  }
  for(a in 2:length(pars)) {
    ifelse(pars[a] == -1000, logS[a] <- 0, logS[a] <- logS[a-1]+pars[a])
  }

  selex <- exp(logS-max(logS))
  selex[pars== -1000] <- 0
  return(selex)
}

selexYear.fn <- function(x, yr, bnds=c(-5,9)) {
  ## specific for hake 2013 and 2014
  selexPars <- matrix(c(-1000, 0, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = nrow(x), ncol = 16, byrow = TRUE)
  devsPars  <- matrix(NA, ncol = ncol(selexPars), nrow = nrow(x))

  tmp <- grep("AgeSel_1P_[1-9]_Fishery", names(x))
  devsInd <- grep("AgeSel_1P_[1-9]_Fishery_DEVadd", names(x))
  allDevsPars <- x[,devsInd]
  selexPars[,3:7] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
  devsInd <- grep(as.character(yr), names(x)[devsInd])
  devsPars[,3:7] <- as.matrix(allDevsPars[,devsInd])

  selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
  for(i in 1:nrow(selexPars)) {
    selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
  }
  return(selex)
}

selexYear10.fn <- function(x,yr,bnds=c(-5,9)) {
  ## specific for hake 2013 and 2014
  selexPars <- matrix(c(-1000,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,0,0,0),nrow=nrow(x),ncol=16,byrow=T)
  devsPars  <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))

  tmp <- grep("AgeSel_1P_[0-9]+_Fishery",names(x))
  devsInd <- grep("AgeSel_1P_[0-9]+_Fishery_DEVadd",names(x))
  allDevsPars <- x[,devsInd]
  selexPars[,3:11] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
  devsInd <- grep(as.character(yr),names(x)[devsInd])
  devsPars[,3:11] <- as.matrix(allDevsPars[,devsInd])

  selex <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))
  for(i in 1:nrow(selexPars)) {
    selex[i,] <- randWalkSelex.fn(selexPars[i,],devsPars[i,],bounds=bnds)
  }

  return(selex)
}

biomass_fraction_plots <- function(replist, selected=FALSE){
  ## biomass fraction of ages 4+
  ## get weight at age
  wtatage <- replist$wtatage[replist$wtatage$fleet==1,-(2:6)]
  ## make years positive
  names(wtatage)[1] <- "Yr" # avoid annoying mix of 'yr' and 'Yr'
  wtatage$Yr <- abs(wtatage$Yr)
  ## get equilibrium value (mean across years for hake)
  wtatage.mean <- wtatage[1,]
  ## get numbers at age
  natage <- replist$natage[replist$natage$"Beg/Mid"=="B",-c(1:6,8:11)]
  ## fill in missing years in weight at age with equilibrium value
  for(y in setdiff(natage$Yr, wtatage$Yr)){
    tmp <- wtatage.mean
    tmp$Yr <- y
    wtatage <- rbind(wtatage,tmp)
  }
  wtatage <- wtatage[order(wtatage$Yr),]
  Yrs <- rownames(natage) <- natage$Yr
  ## get fishery selectivity (method would differ if it were time-varying)
  sel <- replist$ageselex[replist$ageselex$fleet==1 &
                          replist$ageselex$factor=="Asel",c("year",paste(0:20))]
  if(nrow(sel)<10){
    ## if not time-varying, repeat vector for all years
    sel <- matrix(as.numeric(sel[1,-1]),ncol=ncol(sel)-1,nrow=length(Yrs),byrow=TRUE)
  }else{
    ## if time-varying, fiddle with years to make them match the numbers at age
    sel.init <- sel[sel$year==1963,]
    sel <- sel[sel$year!=1963,]
    for(y in 1965:1964){
      sel.init$year <- y
      sel <- rbind(sel.init, sel)
    }
    sel.2014 <- sel[sel$year==2014,]
    for(y in 2015:2016){
      sel.2014$year <- y
      sel <- rbind(sel, sel.2014)
    }
    if(any(sel$year!=Yrs)){
      stop("problem with selectivity")
    }else{
      sel <- sel[,-1]
    }
  }

  ## calculate biomass at age
  batage <- natage[,-1]*wtatage[,-1]
  ## selected biomass at age
  batage.sel <- batage*sel

  if(selected){
    B0plus <- as.numeric(apply(batage.sel, 1, sum))
    B4plus <- as.numeric(apply(batage.sel[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage.sel[, 0:20 >= 5], 1, sum))
  }else{
    B0plus <- as.numeric(apply(batage, 1, sum))
    B4plus <- as.numeric(apply(batage[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage[, 0:20 >= 5], 1, sum))
  }

  ## define time-periods
  par(mfrow=c(2,1))
  main.yrs <- Yrs %in% 1966:2014
  fore.yrs <- Yrs >= 2014

  ## plot timeseries of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,ceiling(max(B0plus/1e6))), yaxs='i', las=1,
       xlab='Year', ylab='Biomass (millions of mt)')
  lines(Yrs[main.yrs], B0plus[main.yrs]/1e6, lwd=3, col=1, lty=1)
  lines(Yrs[main.yrs], B4plus[main.yrs]/1e6, lwd=3, col=2, lty=1)
  lines(Yrs[main.yrs], B5plus[main.yrs]/1e6, lwd=3, col=4, lty=1)
  lines(Yrs[fore.yrs], B0plus[fore.yrs]/1e6, lwd=2, col=1, lty='12')
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/1e6, lwd=2, col=2, lty='12')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/1e6, lwd=2, col=4, lty='12')
  points(Yrs[1], B0plus[1]/1e6,lwd=3)
  points(Yrs[1], B4plus[1]/1e6,col=2,lwd=3)
  points(Yrs[1], B5plus[1]/1e6,col=4,lwd=3)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=1:5,lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(1,2,4), ncol=3,
         legend=c("All ages","Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated selected biomass", "Estimated total biomass"))

  ## plot timeseries of fractions of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,1), yaxs='i', las=1,
       xlab='Year', ylab='Fraction of biomass')
  lines(Yrs[main.yrs], B4plus[main.yrs]/B0plus[main.yrs], lwd=3, col=2)
  lines(Yrs[main.yrs], B5plus[main.yrs]/B0plus[main.yrs], lwd=3, col=4)
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=2, lty='11')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=4, lty='11')
  points(Yrs[1], B4plus[1]/B0plus[1], lwd=3, col=2)
  points(Yrs[1], B5plus[1]/B0plus[1], lwd=3, col=4)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=seq(0,0.8,.2),lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(2,4), ncol=2,
         legend=c("Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated fractions of selected biomass", "Estimated fractions of total biomass"))
}

mcmc.out <- function (directory = "c:/mydirectory/", run = "mymodel/", file = "keyposteriors.csv", 
    namefile = "postplotnames.sso", names = FALSE, headernames = TRUE, 
    numparams = 1, closeall = TRUE, burn = 0, thin = 1, scatter = FALSE, 
    surface = FALSE, surf1 = 1, surf2 = 2, stats = FALSE, plots = TRUE, 
    header = TRUE, sep = ",", print = FALSE, new = T, colNames = NULL) 
{
    if (print == TRUE) {
    }
    if (closeall == TRUE) {
    }
    filename <- file.path(directory, run, file)
    if (!file.exists(filename)) {
        stop("file doesn't exist:\n", filename)
    }
    mcmcdata <- read.table(filename, header = header, sep = sep, 
        fill = TRUE)
    if (names == TRUE) {
        nameout <- file.path(directory, run, namefile)
        namedata <- read.table(nameout, header = FALSE, sep = "", 
            colClasses = "character", fill = TRUE)
        numparams <- as.numeric(namedata[1, 1])
        for (j in 1:numparams) {
            names(mcmcdata)[j] <- namedata[(j + 1), 1]
        }
    }
    if (!is.null(colNames)) {
        if (length(colNames) != numparams) 
            cat("numparams argument overidden by length of colNames argument\n")
        numparams <- length(colNames)
        mcmcdata <- mcmcdata[, colNames]
        if (length(colNames) == 1) {
            mcmcdata <- data.frame(mcmcdata)
            names(mcmcdata) <- colNames
        }
    }
    mcmcfirst <- mcmc(mcmcdata)
    mcmctemp <- window(mcmcfirst, thin = thin, start = (1 + burn))
    mcthinned <- as.matrix(mcmctemp)
    mcmcobject <- mcmc(mcthinned)
    draws <- length(mcmcobject[, 1])
    if (plots == TRUE) {
        if (new) 
            dev.new(record = TRUE)
        if (numparams == 5 || numparams == 9 || numparams == 
            13 || numparams == 17) {
            plot(0, 0, xlab = "", ylab = "", frame.plot = FALSE, 
                yaxt = "n", xaxt = "n", type = "n")
        }
        for (i in 1:numparams) {
            par(new = FALSE, mfrow = c(2, 2), ann = TRUE)
            traceplot(mcmcobject[, i], smooth = TRUE)
            mtext("Value", side = 2, line = 3, font = 1, cex = 0.8)
            if (names | headernames) {
                mtext(names(mcmcdata)[i], side = 3, adj = 0, 
                  line = 2, font = 2, cex = 1)
            }
            else {
                mtext(paste("param", i), side = 3, adj = 0, line = 2, 
                  font = 2, cex = 1)
            }
            lowest <- min(mcmcobject[, i])
            highest <- max(mcmcobject[, i])
            plot(c(seq(1, draws, by = 1)), c(lowest, rep(c(highest), 
                (draws - 1))), xlab = "Iterations", ylab = "", 
                yaxt = "n", type = "n")
            if (!exists("running")) {
                cat("skipping running average section because function 'running' is needed\n")
            }
            else {
                lines(running(mcmcobject[, i], fun = median, 
                  allow.fewer = TRUE, width = draws))
                fun <- function(x, prob) quantile(x, probs = prob, 
                  names = FALSE)
                lines(running(mcmcobject[, i], fun = fun, prob = 0.05, 
                  allow.fewer = TRUE, width = draws), col = "GREY")
                lines(running(mcmcobject[, i], fun = fun, prob = 0.95, 
                  allow.fewer = TRUE, width = draws), col = "GREY")
            }
            par(ann = FALSE)
            autocorr.plot(mcmcobject[, i], auto.layout = FALSE, 
                lag.max = 20, ask = FALSE)
            mtext("Autocorrelation", side = 2, line = 3, font = 1, 
                cex = 0.8)
            mtext("Lag", side = 1, line = 3, font = 1, cex = 0.8)
            lines(seq(1, 20, by = 1), rep(0.1, 20), col = "GREY")
            lines(seq(1, 20, by = 1), rep(-0.1, 20), col = "GREY")
            densplot(mcmcobject[, i], show.obs = TRUE)
            mtext("Density", side = 2, line = 3, font = 1, cex = 0.8)
            mtext("Value", side = 1, line = 3, font = 1, cex = 0.8)
        }
    }
    if (stats == TRUE) {
        dev.new()
        par(mar = c(0, 0, 3, 0))
        plot(0, ylab = "", xlab = "", type = "n", xlim = c(0, 
            25), ylim = c(0, 25), main = "Summary statistics for key parameters", 
            axes = FALSE)
        text(0.001, 25, "Parameter", font = 2, cex = 0.9, adj = 0)
        text(4, 25, "Median (0.05-0.95)", font = 2, cex = 0.9, 
            adj = 0)
        text(13, 25, "AC Lag 1", font = 2, cex = 0.9, adj = 0)
        text(16.5, 25, "Eff. N", font = 2, cex = 0.9, adj = 0)
        text(19, 25, "Geweke-Z", font = 2, cex = 0.9, adj = 0)
        text(22.5, 25, "Heidel-W", font = 2, cex = 0.9, adj = 0)
        for (i in 1:numparams) {
            text(0, (25 - i), paste("param", i), font = 1, cex = 0.9, 
                adj = 0)
            med <- quantile(mcmcobject[, i], probs = 0.5, names = FALSE)
            range <- quantile(mcmcobject[, i], probs = c(0.05, 
                0.95), names = FALSE)
            text(3.2, 25 - i, paste(signif(round(med, 6), 6), 
                "(", paste(signif(round(range[1], 6), 6), "-", 
                  signif(round(range[2], 6), 6)), ")"), font = 1, 
                cex = 0.9, adj = 0)
            l1.ac <- acf(mcmcobject[, i], lag.max = 1, type = "correlation", 
                plot = F)
            acoruse <- round(l1.ac$acf[2], 6)
            text(13, 25 - i, acoruse, font = 1, cex = 0.9, adj = 0)
            effsize <- effectiveSize(mcmcobject[, i])
            text(16.5, 25 - i, round(min(effsize, draws), 0), 
                font = 1, cex = 0.9, adj = 0)
            if (acoruse > 0.4) {
                gewuse <- "None"
            }
            if (acoruse <= 0.4) {
                geweke <- geweke.diag(mcmcobject[, i], frac1 = 0.1, 
                  frac2 = 0.5)
                gewuse <- round(geweke$z, 3)
            }
            text(19, 25 - i, gewuse, font = 1, cex = 0.9, adj = 0)
            if (acoruse > 0.4) {
                send <- "None"
            }
            if (acoruse <= 0.4) {
                hw <- as.list(heidel.diag(mcmcobject[, i], pvalue = 0.05))
                if (hw[1] == 0) {
                  send <- "Failed"
                }
                if (hw[1] == 1) {
                  send <- "Passed"
                }
            }
            text(22.5, 25 - i, send, font = 1, cex = 0.9, adj = 0)
        }
    }
    if (scatter == TRUE) {
        dev.new()
        par(xaxt = "n", yaxt = "n")
        pairs(mcmcdata[1:numparams], cex = 0.1, gap = 0)
    }
    if (surface == TRUE) {
        dev.new()
        par(new = FALSE)
        hist2d(mcmcobject[, surf1], mcmcobject[, surf2], nbins = 100, 
            na.rm = TRUE, xlab = paste("parameter", surf1), ylab = paste("parameter", 
                surf2), show = TRUE, col = c("GREY", topo.colors(20)))
    }
}
