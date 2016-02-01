load.models <- function(models.dir = file.path("..","..","models"),
                        yr,          ## The year to calculate mcmc values for. Should be the last year in the time series (without projections).
                        smart.load,  ## If TRUE, load only new models, keeping the models list intact, but with new models added on the end.
                                     ##  This allows you to keep any forecasting already done in the previous models, saving a lot of time
                                     ## If FALSE, the models list will be completely wiped out, and all models will be reloaded from disk.
                                     ##  This would require a re-running of the forecasting.
                        model.list,  ## If not NULL and smart.load is TRUE, this is the models list to be appended to.
                                     ##  If NULL and smart.load is FALSE, the models list will be built from scratch.
                        verbose = TRUE
                        ){
  ## Load all models and their dat files into a list
  ## For all model directories that have an 'mcmc'
  ## sub-directory, load that as well, and attach
  ## as an object of its parents' list item called 'mcmc'.
  ## If the loading of the mcmc directory fails,
  ## its object will be set to NULL but code will continue.
  ##
  ## Returns a list of models, each of which has a dat member,
  ## an mcmc member, and a catch.proj member. The dat member must be populated, but
  ## the mcmc member will be NULL if the mcmc directory had
  ## something wrong with it. The catch.proj member will be NULL if catch.levels was NULL
  ## or a list of the same length as the number of catch levels input into this function.

  if(smart.load & is.null(model.list)){
    cat("load.models: Warning - you specified to use smart loading but the models list is empty. Continuing with loading all models...\n")
    smart.load <- FALSE
  }
  models.names <- file.path(models.dir, dir(models.dir))
  if(smart.load){
    curr.model.names <- sapply(model.list[1:length(model.list)], "[[", "path")
    which.loaded <- models.names %in% curr.model.names
  }else{
    which.loaded <- rep(FALSE, length(models.names))
  }

  for(nm in 1:length(models.names)){
    if(!smart.load | (smart.load & !which.loaded[nm])){
      tryCatch({
        model.list[[nm]] <- SS_output(dir=models.names[nm], verbose = verbose)
      }, warning = function(war){
        cat("load.models: Warning - warning loading model ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
      }, error = function(err){
        cat("load.models: Error - error loading model ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
        stop("load.models: Check model outputs and re-run.")
      })

      ## Load the data file and control file for the model
      ## Get the file whose name contains "_data.ss" and "_control.ss"
      ## If there is not exactly one of each, stop with error.
      model.dir.listing <- dir(models.names[nm])
      fn.ind <- grep("_data.ss", model.dir.listing)
      ## Also get control file name
      c.file.ind <- grep("_control.ss", model.dir.listing)
      if(!length(fn.ind)){
        stop("load.models: Error in model ",models.names[nm],
             ", there is no data file. A data file is anything followed and ending in _data.ss.\n")
      }
      if(length(fn.ind) > 1){
        stop("load.models: Error in model ",models.names[nm],
             ", there is more than one data file. A data file is anything followed and ending in _data.ss. Check and make sure there is only one.\n")
      }
      fn <- file.path(models.names[nm], model.dir.listing[fn.ind])
      c.fn <- file.path(models.names[nm], model.dir.listing[c.file.ind])
      tryCatch({
        model.list[[nm]]$path <- models.names[nm]
        model.list[[nm]]$dat.file <- fn
        model.list[[nm]]$dat <- SS_readdat(fn)
        model.list[[nm]]$ctl.file <- c.fn
        model.list[[nm]]$ctl <- readLines(c.fn)
      }, warning = function(war){
        cat("load.models: Warning while loading the dat file '",fn,
            "' for model scenario ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
      }, error = function(err){
        cat("load.models: Error while loading the dat file '",fn,
            "' for model scenario ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
        stop("load.models: Check model outputs and re-run.")
      })

      ## If it has an 'mcmc' sub-directory, load that as well
      mcmc.dir <- file.path(models.names[nm], "mcmc")
      if(dir.exists(mcmc.dir)){
        tryCatch({
          model.list[[nm]]$mcmc <- data.frame(SSgetMCMC(dir=mcmc.dir, writecsv=FALSE, verbose = verbose)$model1)
          model.list[[nm]]$mcmcpath <- mcmc.dir
          create.key.nuisance.posteriors.files(model.list[[nm]],
                                               key.posteriors,
                                               key.posteriors.file,
                                               nuisance.posteriors.file)
          ## Likely these are unneccessary
          ## model.list[[nm]]$mcmckey <- read.csv(file.path(mcmc.dir, "keyposteriors.csv"))
          ## model.list[[nm]]$mcmcnuc <- read.csv(file.path(mcmc.dir, "nuisanceposteriors.csv"))
          ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS
          model.list[[nm]]$mcmccalcs <- calc.mcmc(model.list[[nm]]$mcmc)
        }, warning = function(war){
          cat("load.models: Warning while loading the MCMC model ", mcmc.dir,". Warning was: ", war$message,". Continuing...\n")
        }, error = function(err){
          cat("load.models: Error while loading the MCMC model ", mcmc.dir,". Error was: ", err$message,". Continuing...\n")
          model.list[[nm]]$mcmc <- NULL
        })
      }else{
        model.list[[nm]]$mcmc <- NULL
      }
    }
  }
  return(model.list)
}

create.key.nuisance.posteriors.files <- function(model,
                                                 posterior.regex,
                                                 key.post.file,
                                                 nuisance.post.file){
  ## Creates the two files for key and nuisance posteriors
  key.file <- file.path(model$mcmcpath, key.post.file)
  nuisance.file <- file.path(model$mcmcpath, nuisance.post.file)

  mc <- model$mcmc
  mc.names <- names(mc)
  mcmc.grep <- unique(grep(paste(posterior.regex, collapse="|"), mc.names))
  mcmc.names <- mc.names[mcmc.grep]
  keys <- mc[,mcmc.grep]
  nuisances <- mc[,-mcmc.grep]
  write.csv(keys, key.file, row.names = FALSE)
  write.csv(nuisances, nuisance.file, row.names = FALSE)
}

calc.mcmc <- function(mcmc,            ## mcmc is the output of the SS_getMCMC function from the r4ss package as a data.frame
                      lower = 0.025,   ## Lower quantile for confidence interval calcs
                      upper = 0.975    ## Upper quantile for confidence interval calcs
                      ){
  ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS, SPR
  ## Returns a list of them all

  ## 2e6 used here because biomass will be shown in the millions of tonnes and it is female only
  spb <- mcmc[,grep("SPB",names(mcmc))]/2e6
  svirg <- quantile(spb[,names(spb) == "SPB_Virgin"], c(lower, 0.5, upper))
  sinit <- quantile(spb[,names(spb) == "SPB_Initial"], c(lower, 0.5, upper))

  ## sinit.post is saved here so that depletion calculations can be done for each posterior,
  sinit.post <- spb[,names(spb) == "SPB_Initial"]

  names(spb) <- gsub("SPB_","",names(spb))
  cols.to.strip <- c("Virgin", "Initial")
  spb <- strip.columns(spb, cols.to.strip)

  slower <- apply(spb,2,quantile,prob=lower)
  smed   <- apply(spb,2,quantile,prob=0.5)
  supper <- apply(spb,2,quantile,prob=upper)

  depl   <- apply(spb,2,function(x){x/sinit.post})
  dlower <- apply(depl,2,quantile,prob=lower)
  dmed   <- apply(depl,2,quantile,prob=0.5)
  dupper <- apply(depl,2,quantile,prob=upper)

  ## 1e6 used here because recruitment will be shown in the millions of tonnes
  recr <- mcmc[,grep("Recr_",names(mcmc))]/1e6
  recr <- recr[,-grep("Fore",names(recr))]
  names(recr) <- gsub("Recr_","",names(recr))
  rvirg <- quantile(recr[,names(recr) == "Virgin"], c(lower, 0.5, upper))
  rinit <- quantile(recr[,names(recr) == "Initial"], c(lower, 0.5, upper))
  runfished <- quantile(recr[,names(recr) == "Unfished"], c(lower, 0.5, upper))

  cols.to.strip <- c("Virgin", "Initial","Unfished")
  recr <- strip.columns(recr, cols.to.strip)

  rmed <- apply(recr, 2, quantile, prob=0.5)
  rmean <- apply(recr, 2, mean)
  rlower <- apply(recr, 2, quantile,prob=lower)
  rupper <- apply(recr, 2, quantile,prob=upper)

  dev <- mcmc[,c(grep("Early_InitAge_", names(mcmc)),
                 grep("Early_RecrDev_", names(mcmc)),
                 grep("Main_RecrDev_", names(mcmc)),
                 grep("Late_RecrDev_", names(mcmc)),
                 grep("ForeRecr_", names(mcmc)))]

  names(dev) <- gsub("Early_RecrDev_", "", names(dev))
  names(dev) <- gsub("Main_RecrDev_", "", names(dev))
  names(dev) <- gsub("Late_RecrDev_", "", names(dev))
  names(dev) <- gsub("ForeRecr_", "", names(dev))

  ## Change the Early_Init names to be the correct preceeding years
  start.yr <- as.numeric(min(names(dev)))
  early <- grep("Early_InitAge_",names(dev))
  num.early.yrs <- length(early)
  early.yrs <- seq(start.yr - num.early.yrs, start.yr - 1, 1)
  late.yrs <- names(dev[-early])
  names(dev) <- c(as.character(early.yrs), late.yrs)

  devlower <- apply(dev, 2, quantile, prob=lower)
  devmed <- apply(dev, 2, quantile, prob=0.5)
  devupper <- apply(dev, 2, quantile, prob=upper)

  spr <- mcmc[,grep("SPRratio_", names(mcmc))]
  names(spr) <- gsub("SPRratio_", "", names(spr))

  plower <- apply(spr, 2, quantile, prob=lower)
  pmed <- apply(spr, 2, quantile, prob=0.5)
  pupper <- apply(spr, 2, quantile, prob=upper)

  f <- mcmc[,grep("F_", names(mcmc))]
  names(f) <- gsub("F_", "", names(f))
  flower <- apply(f, 2, quantile, prob=lower)
  fmed   <- apply(f, 2, quantile, prob=0.5)
  fupper <- apply(f, 2, quantile, prob=upper)

  return(list(svirg=svirg, sinit=sinit, slower=slower, smed=smed, supper=supper,
              dlower=dlower, dmed=dmed, dupper=dupper,
              rvirg=rvirg, rinit=rinit, runfihed=runfished,
              rlower=rlower, rmed=rmed, rupper=rupper, rmean=rmean,
              devlower=devlower, devmed=devmed, devupper=devupper,
              plower=plower, pmed=pmed, pupper=pupper,
              flower=flower, fmed=fmed, fupper=fupper))
}

calc.forecast <- function(mcmc,                ## The output of the SS_getMCMC function from the r4ss package as a data.frame
                          model.dir,           ## The path of the model to run forecasts for
                          forecast.yrs,        ## A vector of years to do projections for
                          catch.levels,        ## catch.levels is a list of N catch levels to run projections for
                          catch.levels.names,  ## catch.levels.names is a list of N names for the catch levels given in catch.levels
                          probs = NULL){       ## Probabilities for table
  ## Run forecasts  on the mcmc model and return the list of outputs.
  ## Also returns the list of mcmc calcs from the models
  if(is.null(probs)){
    stop("\ncalc.forecast: Error - You must supply a probability vector (probs)\n")
  }
  if(!is.null(catch.levels)){
    if(length(catch.levels) != length(catch.levels.names)){
      stop("\ncalc.forecast: Error - catch.levels is not the same length as catch.levels.names!\n")
    }
  }

  mcmc.dir <- file.path(model.dir,"mcmc")
  forecast.dir <- file.path(mcmc.dir,"forecasts")
  ## Delete any old forecasts by removing whole forecasts directory recursively, then re-create
  unlink(forecast.dir, recursive=TRUE)
  dir.create(forecast.dir)

  ## biomass.list and spr.list will contain one element for each model run for each forecast.
  ##  Each will have the quantiles for the proj.years.
  biomass.list <- NULL
  spr.list <- NULL

  ## mcmccalcs.list holds the quantile calculations of important variables such as
  ##  biomass, depletion, recruitment
  mcmccalcs.list <- NULL
  ## outputs.list holds the outputs from the mcmc models as read in by SSgetMCMC
  outputs.list <- NULL

  for(level.ind in 1:length(catch.levels)) {
    ## Create a new sub-directory for each catch projection
    name <- catch.levels.names[level.ind]
    new.forecast.dir <- file.path(forecast.dir, name)
    dir.create(new.forecast.dir)

    ## Copy all model files into this new forecast directory
    file.copy(file.path(mcmc.dir, list.files(mcmc.dir)), new.forecast.dir)

    ## Insert fixed catches into forecast file
    forecast.file <- file.path(new.forecast.dir, "forecast.ss")
    fore <- SS_readforecast(forecast.file, Nfleets=1, Nareas=1, verbose=FALSE)
    fore$Ncatch <- length(forecast.yrs)
    fore$ForeCatch <- data.frame(Year=forecast.yrs, Seas=1, Fleet=1, Catch_or_F=catch.levels[[level.ind]])
    SS_writeforecast(fore, dir = new.forecast.dir, overwrite = TRUE, verbose = FALSE)

    ## Evaluate the model using mceval option of ADMB, and retrieve the output
    shell.command <- paste0("cd ", new.forecast.dir, " & ss3 -mceval")
    shell(shell.command)
    mcmc.out <- SSgetMCMC(dir=new.forecast.dir, writecsv=FALSE)$model1

    ## Get the values of interest, namely Spawning biomass and SPR for the two
    ## decision tables in the executive summary
    sb <- mcmc.out[,grep("Bratio_",names(mcmc.out))]
    spr <- mcmc.out[,grep("SPRratio_",names(mcmc.out))]

    ## Strip out the Bratio_ and SPRratio_ headers so columns are years only
    names(sb) <- gsub("Bratio_", "",names(sb))
    names(spr) <- gsub("SPRratio_", "",names(spr))

    ## Now, filter out the projected years only
    sb.proj.cols <- sb[,names(sb) %in% forecast.yrs]
    spr.proj.cols <- spr[,names(spr) %in% forecast.yrs]

    ## Do quantile calculations and put results in the output lists
    biomass.list[[level.ind]] <- t(apply(sb.proj.cols, 2, quantile, probs=probs))
    spr.list[[level.ind]] <- t(apply(spr.proj.cols, 2, quantile, probs=probs))
    mcmccalcs.list[[level.ind]] <- calc.mcmc(mcmc.out)
    outputs.list[[level.ind]] <- mcmc.out
  }
  names(biomass.list) <- catch.levels.names
  names(spr.list) <- catch.levels.names
  names(mcmccalcs.list) <- catch.levels.names
  names(outputs.list) <- catch.levels.names

  return(list(biomass.list,
              spr.list,
              mcmccalcs.list,
              outputs.list))
}

calc.risk <- function(forecast.outputs,    ## A list of the output of the SS_getMCMC function, 1 for each catch.level
                      forecast.yrs,        ## A vector of years to do projections for
                      catch.levels,        ## catch.levels is a list of N catch levels to run projections for
                      catch.levels.names){ ## catch.levels.names is a list of N names for the catch levels given in catch.levels
  ## Calculate the probablities of being under several reference points from one forecast year to the next

  ## risk.list will hold the probabilities of being under several reference points.
  ##  it will be of length 1 less than the number of forecast years, and each element
  ##  will itself be a data.frame of catch levels with those holding the probabilities.
  ## For example, list element 1 will hold the probabilities for each catch.level of being under
  ##  several reference points for the first two years in the forecast.yrs vector

  if(!is.null(catch.levels)){
    if(length(catch.levels) != length(catch.levels.names)){
      stop("\ncalc.risk: Error - catch.levels is not the same length as catch.levels.names!\n")
    }
  }

  metric <- function(x, yr){
    out <- NULL
    out[1] <- max(x[,paste0("ForeCatch_",yr)])
    out[2] <- sum(x[,paste0("SPB_",yr+1)] < x[,paste("SPB",yr,sep="_")])/nrow(x) * 100.0
    out[3] <- sum(x[,paste0("Bratio_",yr+1)] < 0.40)/nrow(x) * 100.0
    out[4] <- sum(x[,paste0("Bratio_",yr+1)] < 0.25)/nrow(x) * 100.0
    out[5] <- sum(x[,paste0("Bratio_",yr+1)] < 0.10)/nrow(x) * 100.0
    out[6] <- sum(x[,paste0("SPRratio_",yr)] > 1.00)/nrow(x) * 100.0
    out[7] <- sum(x[,paste0("ForeCatch_",yr+1)] < out[1])/nrow(x) * 100.0
    return(out)
  }
  risk.list <- NULL
  for(i in 1:(length(forecast.yrs)-1)){
    yr <- forecast.yrs[i]
    risk.list[[i]] <- data.frame()
    for(j in 1:length(forecast.outputs)){
      x <- forecast.outputs[[j]]
      risk.list[[i]] <- rbind(risk.list[[i]], metric(x, yr))
    }
  }
  return(risk.list)
}

