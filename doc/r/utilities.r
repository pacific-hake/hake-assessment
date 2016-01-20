run.partest.model <- function(model,
                              output.file, ## The model object will be stored in binary form here
                              rows.to.skip.to.comps.header = 21,
                              rows.to.skip.to.likelihood = 90,
                              rows.to.skip.to.survey.header = 1246){
  ## To ensure integration with the knitr loading step, you must
  ## run this from the Rgui (after you've got a base model loaded) like this:
  ##
  ## run.partest.model(base.model, "model-partest.RData")
  ##
  ## This Re-runs the model (MLE) once for each posterior
  ## and fetches information from their respective Report.sso files.
  ## This is to be run once for the base model, and stored as a binary as
  ## shown above.
  ##
  ## rows.to.skip.to.survey.header should be the line just above the header for
  ## the survey (index 2 in this case) in the Reports.sso file
  ##
  ## rows.to.skip.to.likelihood should be the line just above the LIKELIHOOD
  ## line in the Reports.sso file
  ##
  ## rows.to.skip.to.comps.header should be the line just above the header for
  ## the age comps in the CompReport.sso file

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
  ## num.posts <- 5
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

  start <- SS_readstarter(file.path(partest.dir, "starter.ss"))
  ## Change starter file to read from par file
  start$init_values_src <- 1
  SS_writestarter(start, dir = partest.dir, file = "starter.ss", overwrite = TRUE)

  if(start$init_values_src != 1){
    stop("run.partest.model: Error - change starter file to read from par file!")
  }

  ## loop over rows of posteriors file
  for(irow in 1:num.posts){
    print(irow)
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
    shell(shell.command)
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
    likes <- read.table(file.path(reports.dir, paste0("Report_", irow, ".sso")),
                        skip = rows.to.skip.to.likelihood,
                        nrows = 17,
                        fill = TRUE,
                        row.names = NULL,
                        col.names = 1:4,
                        stringsAsFactors = FALSE)
    like.info[irow, 2:10] <- as.numeric(likes$X2[3:11])  ## fleet-aggregated likelihoods
    like.info[irow, 11] <- as.numeric(likes[17, 3])      ## fleet-specific age comp likelihoods
    like.info[irow, 12] <- as.numeric(likes[17, 4])      ## fleet-specific age comp likelihoods
  }

  ## read expected proportions and Pearson values for each age comp observations
  comp.table <- read.table(file.path(partest.dir, "CompReport.sso"),
                           skip = rows.to.skip.to.comps.header,
                           header = TRUE,
                           fill = TRUE,
                           stringsAsFactors = FALSE)
  ## loop to create columns Exp1, Exp2, ..., Exp999 and Pearson1, Pearson2, etc.
  for(irow in 1:num.posts){
    if(irow %% 100 == 0){
      print(irow)
    }
    comps <- read.table(file.path(reports.dir, paste0("CompReport_", irow, ".sso")),
                        skip = rows.to.skip.to.comps.header,
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

  cpue.table <- NULL
  for(irow in 1:num.posts){
    cpue <- read.table(file.path(reports.dir, paste0("Report_", irow,".sso")),
                       skip = rows.to.skip.to.survey.header,
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
  return(format(round(x,dec.points),big.mark=","))
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

addpoly <- function(yrvec, lower, upper, color){ # add shaded uncertainty intervals behind line
  lower[lower<0] <- 0 # max of value or 0
  shadeCol <- rgb(t(col2rgb(color)),alpha=0.2*255,maxColorValue=255)
  polygon(x=c(yrvec,rev(yrvec)),y=c(lower,rev(upper)),
          border=NA,col=shadeCol)
  lines(yrvec,lower,lty=3,col=color)
  lines(yrvec,upper,lty=3,col=color)
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

#randWalkSelex.fn(c(-1000,-1000,0,0.317379,0.00653887,-0.0244099,0.449238,0,0,0,0,0,0,0,0))
#randWalkSelex.fn(c(-1000,0,4.02533, 1.65537,   0.49088, 0.264563, 0.330266, 0,0,0,0,0,0,0,0,0),
#             devs=c(NA, NA,0.203144,-0.102409,-0.020993,0.0786477,0.0492123,NA,NA,NA,NA,NA,NA,NA,NA,NA),
#             bounds=c(-5,9))

selexYear.fn <- function(x,yr,bnds=c(-5,9)) {
    ## specific for hake 2013 and 2014
    selexPars <- matrix(c(-1000,0,NA,NA,NA,NA,NA,0,0,0,0,0,0,0,0,0),nrow=nrow(x),ncol=16,byrow=T)
    devsPars  <- matrix(NA,ncol=ncol(selexPars),nrow=nrow(x))

    tmp <- grep("AgeSel_1P_[1-9]_Fishery",names(x))
    devsInd <- grep("AgeSel_1P_[1-9]_Fishery_DEVadd",names(x))
    allDevsPars <- x[,devsInd]
    selexPars[,3:7] <- as.matrix(x[,tmp[!(tmp %in% devsInd)]])
    devsInd <- grep(as.character(yr),names(x)[devsInd])
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
