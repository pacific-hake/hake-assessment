load.model <- function(models.dir,                ## Directory name for all models location
                       model.name,                ## Directory name of model to be loaded
                       overwrite = FALSE,         ## Overwrite the RData file?
                       yr,                        ## The year to calculate mcmc values for if load.mcmc = TRUE.
                                                  ##  Should normally be the last year in the time series (without projections).
                       run.forecasting = FALSE,   ## Run forecasting for this model?
                       ## ovwrt.forecasting = FALSE, ## If TRUE and run.forecasting=TRUE and forecast folder exists, re-run forecasting
                       forecast.yrs,              ## Vector of years to run forecasting for if run.forecasting = TRUE
                       forecast.probs,            ## Vector of quantile values if run.forecasting = TRUE
                       catch.levels,              ## Vector of catch levels to run forecasting for if run.forecasting = TRUE
                       catch.levels.dir.names,    ## Vector of catch levels directory names if run.forecasting = TRUE
                       run.retros = FALSE,        ## Run retrospectives for this model?
                       ## ovwrt.retros = FALSE,      ## If TRUE and run.retros=TRUE and retrospectives folder exists, re-run retros
                       retro.yrs,                 ## Vector of integers (positives) to run retrospectives for if run.retros = TRUE
                       run.partest = FALSE,       ## Run partest for this model?
                       key.posteriors,            ## Vector of key posteriors used to create key posteriors file
                       key.posteriors.fn = "keyposteriors.csv",
                       nuisance.posteriors.fn = "nuisanceposteriors.csv",
                       verbose = TRUE){
  ## Load the model given by model.name in from disk,
  ##  by first looking for an RData file and attemting to load that.
  ##  If no RData file exists, the model will be loaded into an R object
  ##  and saved as an RData file. If overwrite==TRUE and an RData file
  ##  exists, it will be deleted and a re-load will take place for this model.
  ##  When this function exits, an RData file will be located in the
  ##  directory given by model.name.
  model.dir <- file.path(models.dir, model.name)
  if(!dir.exists(model.dir)){
    stop("load.model: Error - the directory ", model.dir, " does not exist. Fix the problem and try again.\n")
  }
  ## The RData file will have the same name as the directory it is in
  rdata.file <- file.path(model.dir, paste0(model.dir, ".RData"))

  if(file.exists(rdata.file)){
    if(overwrite){
      ## Delete the file
      unlink(rdata.file)
    }else{
      ## Load the RData file into this environment and return the model object
      local({
        load(rdata.file)
      })
      ## Assumes the model was saved with the object name 'model'
      return(invisible(model))
    }
  }

  ## Do the model load, save the RData file, and return the resulting object
  model <- SS_output(dir = model.dir, verbose = verbose)
  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model.dir.listing <- dir(model.dir)
  dat.fn.ind <- grep("_data.ss", model.dir.listing)
  ctl.fn.ind <- grep("_control.ss", model.dir.listing)
  if(!length(dat.fn.ind)){
    stop("load.models: Error in model ", model.name,
         ", there is no data file. A data file is anything followed by and ending in _data.ss.\n\n")
  }
  if(length(dat.fn.ind) > 1){
    stop("load.models: Error in model ", model.name,
         ", there is more than one data file. A data file is anything followed by and ending in _data.ss.\n\n")
  }
  if(!length(ctl.fn.ind)){
    stop("load.models: Error in model ", model.name,
         ", there is no control file. A control file is anything followed by and ending in _control.ss.\n\n")
  }
  if(length(ctl.fn.ind) > 1){
    stop("load.models: Error in model ", model.name,
         ", there is more than one control file. A control file is anything followed by and ending in _control.ss.\n\n")
  }
  dat.fn <- file.path(model.dir, model.dir.listing[dat.fn.ind])
  ctl.fn <- file.path(model.dir, model.dir.listing[ctl.fn.ind])
  model$path <- model.dir
  model$dat.file <- dat.fn
  model$dat <- SS_readdat(dat.fn)
  model$ctl.file <- ctl.fn
  model$ctl <- readLines(ctl.fn)
  model$mcmc <- NULL

  ## If it has an 'mcmc' sub-directory, load that as well
  mcmc.dir <- file.path(model.dir, "mcmc")
  if(dir.exists(mcmc.dir)){
    model$mcmc <- data.frame(SSgetMCMC(dir = mcmc.dir, writecsv = FALSE, verbose = verbose)$model1)
    model$mcmcpath <- mcmc.dir
    create.key.nuisance.posteriors.files(model,
                                         key.posteriors,
                                         key.posteriors.fn,
                                         nuisance.posteriors.fn)
    ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc.mcmc(model$mcmc)

    ## Forecasting can only happen for mcmc models. Check to see if the
    ##  forecasts directory exists
    forecasts.path <- file.path(model$path, "mcmc", "forecasts")
    metrics.paths <- file.path(model$path, "mcmc", paste0("metrics-", seq(length(forecast.yrs) - 1)))
    if(all(dir.exists(c(forecasts.path, metrics.paths)))){
      if(run.forecasting){
        ## Delete all the directories and files
        unlink(forecasts.path, recursive = TRUE)
        unlink(metrics.paths, recursive = TRUE)
      }
    }else if(!run.forecasting & !any(dir.exists(c(forecasts.path, metrics.paths)))){
      fore.answer <- readline(paste0("load.model: The forecast and metrics directories do not exist for model ", model$path, ". Do you want to run forecasting? [(y)es/(q)uit] "))
      if(fore.answer == "q" | fore.answer == "Q"){
        stop("Quitting...\n\n")
      }
      run.forecasting <- TRUE
    }
    if(run.forecasting){
      cat0("load.model: Running forecasts for model located in ", model$path, "...\n")
      calc.forecast(model$mcmc,
                    model$path,
                    forecast.yrs,
                    catch.levels,
                    catch.levels.dir.names,
                    forecast.probs)
      create.metrics(model$mcmc,
                     model$path,
                     forecast.yrs[-length(forecast.yrs)],
                     catch.levels,
                     catch.levels.dir.names)
    }

    ## Load the forecasting results
    forecasts <- fetch.forecast(model$path,
                                forecast.yrs,
                                catch.levels.dir.names,
                                probs = forecast.probs)

    model$forecasts$biomass <- forecasts[[1]]
    model$forecasts$spr <- forecasts[[2]]
    model$forecasts$mcmccalcs <- forecasts[[3]]
    model$forecasts$outputs <- forecasts[[4]]

    metrics <- fetch.metrics(model$path,
                             forecast.yrs[-length(forecast.yrs)],
                             catch.levels.dir.names)

    model$metrics$outputs <- fetch.metrics(model$path,
                                           forecast.yrs[-length(forecast.yrs)],
                                           catch.levels.dir.names)

    ## calc.risk assumes the forecasting step was done correctly
    risks <- calc.risk(model$metrics$outputs,
                       forecast.yrs,
                       catch.levels)

    model$risks <- risks
    cat0("load.model: Finished running forecasts for model located in ", model$path, "...\n")
  }

  retros.dir <- file.path(model$path, "retrospectives")
  retros.paths <- file.path(retros.dir, paste0("retro-", retro.yrs))
  if(dir.exists(retros.dir)){
    if(all(dir.exists(retros.paths))){
      if(run.retros){
        unlink(retros.paths, recursive = TRUE)
      }
    }else{
      stop("load.model: Error - The retrospectives directory has missing retrospective runs for model ", model$path, ".\n\n")
    }
  }
  if(run.retros){
    cat("load.model: Running retrospectives\n\n")
    run.retrospectives(model, yrs = retro.yrs, verbose = verbose)
    model$retros <- list()
    retros.dir <- file.path(model$path, "retrospectives")
    for(retro in 1:length(retro.yrs)){
      retro.dir <- file.path(retros.dir, paste0("retro-", retro.yrs[retro]))
      model$retros[[retro]] <- SS_output(dir = retro.dir, verbose = verbose)
    }
    cat("load.model: Retrospectives Completed\n\n")
  }

  if(run.partest){
    cat("load.model:: Running partest\n\n")
    ## TODO: Separate the running of the partest model with the fetching of it's output.
    run.partest.model(model, output.file = "model-partest.RData", verbose = verbose)
    cat("load.model: Partest Completed\n\n")
  }

  return(model)
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
  ## Run forecasts  on the mcmc model.
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

  for(level.ind in 1:length(catch.levels)){
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
  }
}

fetch.forecast <- function(model.dir,           ## The path of the model to fetch the forecasts from
                           forecast.yrs,        ## A vector of years to do projections for
                           catch.levels.names,  ## Directory names for the catch levels which have been forecast
                           probs = NULL){       ## Probabilities for decision tables
  ## Fetch the output from previously-run forecasts
  forecast.dir <- file.path(model.dir, "mcmc", "forecasts")
  dir.listing <- dir(forecast.dir)
  if(!identical(catch.levels.names, dir.listing)){
    stop("fetch.forecast: There is a discrepancy between what you have set for catch.levels.names and what appears in the forecast directory '",
         forecast.dir,"'. Check the names and try again...\n\n")
  }

  ## biomass.list and spr.list will contain one element for each model run for each forecast.
  ##  Each will have the quantiles for the proj.years.
  biomass.list <- NULL
  spr.list <- NULL

  ## mcmccalcs.list holds the quantile calculations of important variables such as
  ##  biomass, depletion, recruitment
  mcmccalcs.list <- NULL
  ## outputs.list holds the outputs from the mcmc models as read in by SSgetMCMC
  outputs.list <- NULL

  for(level.ind in 1:length(dir.listing)){
    fore.dir <- file.path(forecast.dir, dir.listing[level.ind])
    mcmc.out <- SSgetMCMC(dir = fore.dir, writecsv = FALSE)$model1

    ## Get the values of interest, namely Spawning biomass and SPR for the two
    ## decision tables in the executive summary
    sb <- mcmc.out[,grep("Bratio_", names(mcmc.out))]
    spr <- mcmc.out[,grep("SPRratio_", names(mcmc.out))]

    ## Strip out the Bratio_ and SPRratio_ headers so columns are years only
    names(sb) <- gsub("Bratio_", "", names(sb))
    names(spr) <- gsub("SPRratio_", "", names(spr))

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

create.metrics <- function(mcmc,                 ## The output of the SS_getMCMC function from the r4ss package as a data.frame
                           model.dir,            ## The path of the model to run metrics for
                           metric.yrs,           ## A vector of years to do projections for
                           catch.levels,         ## catch.levels is a list of N catch levels to run projections for
                           catch.levels.names) { ## catch.levels.names is a list of N names for the catch levels given in catch.levels

  ## Run forecasts  on the mcmc model and return the list of outputs.
  ## returns the list of mcmc calcs from the models

  if(!is.null(catch.levels)){
    if(length(catch.levels) != length(catch.levels.names)){
      stop("\ncreate.metrics: Error - catch.levels is not the same length as catch.levels.names!\n")
    }
  }

  mcmc.dir <- file.path(model.dir,"mcmc")

  for(i in 1:length(metric.yrs)) {
    metrics.dir <- file.path(mcmc.dir,paste0("metrics_",i))
    ## Delete any old forecasts by removing whole forecasts directory recursively, then re-create
    unlink(metrics.dir, recursive=TRUE)
    dir.create(metrics.dir)

    for(level.ind in 1:length(catch.levels)) {
      ## Create a new sub-directory for each catch projection
      name <- catch.levels.names[level.ind]
      new.forecast.dir <- file.path(metrics.dir, name)
      dir.create(new.forecast.dir)

      ## Copy all model files into this new forecast directory
      file.copy(file.path(mcmc.dir, list.files(mcmc.dir)), new.forecast.dir)

      ## Insert fixed catches into forecast file (depending on i)
      forecast.file <- file.path(new.forecast.dir, "forecast.ss")
      fore <- SS_readforecast(forecast.file, Nfleets=1, Nareas=1, verbose=FALSE)
      fore$Ncatch <- length(forecast.yrs[1:i])
      fore$ForeCatch <- data.frame(Year=forecast.yrs[1:i], Seas=1, Fleet=1, Catch_or_F=catch.levels[[level.ind]][1:i])
      SS_writeforecast(fore, dir = new.forecast.dir, overwrite = TRUE, verbose = FALSE)

      ## Evaluate the model using mceval option of ADMB, and retrieve the output
      shell.command <- paste0("cd ", new.forecast.dir, " & ss3 -mceval")
      shell(shell.command)
    }
  }
}

fetch.metrics <- function(model.dir,
                          metric.yrs,
                          catch.levels.names){
  ## Fetch the output from previously-run metrics

  ## outputs.list holds the outputs from the mcmc models as read in by SSgetMCMC
  outputs.list <- vector(mode = "list", length = length(metric.yrs))

  for(i in 1:length(metric.yrs)) {
    metrics.dir <- file.path(model.dir, "mcmc", paste0("metrics_", i))
    dir.listing <- dir(metrics.dir)
    if(!identical(catch.levels.names, dir.listing)){
      stop("fetch.metrics: There is a discrepancy between what you have set for catch.levels.names and what appears in the metrics directory '",
           metrics.dir,"'. Check the names and try again...\n\n")
    }
    for(level.ind in 1:length(catch.levels.names)){
      metrics.level.dir <- file.path(metrics.dir, catch.levels.names[level.ind])
      mcmc.out <- SSgetMCMC(dir = metrics.level.dir, writecsv = FALSE)$model1
      outputs.list[[i]][[level.ind]] <- mcmc.out
    }
    names(outputs.list[[i]]) <- catch.levels.names
  }
  return(outputs.list)
}

calc.risk <- function(forecast.outputs,    ## A list of the output of the SS_getMCMC function, 1 for each catch.level
                      forecast.yrs,        ## A vector of years to do projections for
                      catch.levels){       ## catch.levels is a list of N catch levels to run projections for
  ## Calculate the probablities of being under several reference points from one forecast year to the next

  ## risk.list will hold the probabilities of being under several reference points.
  ##  it will be of length 1 less than the number of forecast years, and each element
  ##  will itself be a data.frame of catch levels with those holding the probabilities.
  ## For example, list element 1 will hold the probabilities for each catch.level of being under
  ##  several reference points for the first two years in the forecast.yrs vector

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
    for(j in 1:length(forecast.outputs[[i]])){
      x <- forecast.outputs[[i]][[j]]
      risk.list[[i]] <- rbind(risk.list[[i]], metric(x, yr))
    }
  }
  return(risk.list)
}

