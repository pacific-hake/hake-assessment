load.models <- function(models.dir = file.path("..","..","models"),
                        yr  ## The year to calculate mcmc values for. Should be the last year in the time series (without projections).
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

  models.names <- file.path(models.dir, dir(models.dir))
  model.list <- NULL

  for(nm in 1:length(models.names)){
    ## Load the model in the order in which is is listed
    tryCatch({
      model.list[[nm]] <- SS_output(dir=models.names[nm])
    }, warning = function(war){
      cat("load.models: Warning - warning loading model ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
    }, error = function(err){
      cat("load.models: Error - error loading model ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
      stop("load.models: Check model outputs and re-run.")
    })

    ## Load the data file for the model
    ## Get the file whose name contains "_data.ss"
    ## If there is not exactly one, stop with error.
    model.dir.listing <- dir(models.names[nm])
    fn.ind <- grep("_data.ss", model.dir.listing)
    if(!length(fn.ind)){
      stop("load.models: Error in model ",models.names[nm],", there is no data file. A data file is anything followed and ending in _data.ss.\n")
    }
    if(length(fn.ind) > 1){
      stop("load.models: Error in model ",models.names[nm],", there is more than one data file. A data file is anything followed and ending in _data.ss. Check and make sure there is only one.\n")
    }
    fn <- file.path(models.names[nm], model.dir.listing[fn.ind])
    tryCatch({
      model.list[[nm]]$path <- models.names[nm]
      model.list[[nm]]$dat <- SS_readdat(fn)
    }, warning = function(war){
      cat("load.models: Warning while loading the dat file '",fn,"' for model scenario ", models.names[nm],". Warning was: ", war$message,". Continuing...\n")
    }, error = function(err){
      cat("load.models: Error while loading the dat file '",fn,"' for model scenario ", models.names[nm],". Error was: ", err$message,". Stopping...\n")
      stop("load.models: Check model outputs and re-run.")
    })

    ## If it has an 'mcmc' sub-directory, load that as well
    mcmc.dir <- file.path(models.names[nm], "mcmc")
    if(dir.exists(mcmc.dir)){
      tryCatch({
        model.list[[nm]]$mcmc <- data.frame(SSgetMCMC(dir=mcmc.dir, writecsv=FALSE)$model1)
        ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS
        model.list[[nm]]$mcmccalcs <- calc.mcmc(model.list[[nm]]$mcmc, yr = yr)
        ## For decision tables, need to create some sub-directories and run the mceval.
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
  return(model.list)
}

calc.forecast <- function(mcmc,                ## mcmc is the output of the SS_getMCMC function from the r4ss package as a data.frame
                          model.dir,           ## The path of the model to run forecasts for
                          forecast.yrs,        ## Years to do projections for, e.g. 2015:2017
                          catch.levels,        ## catch.levels is a list of N catch levels to run projections for
                          catch.levels.names,  ## catch.levels.names is a list of N names for the catch levels given in catch.levels
                          probs = NULL){       ## Probabilities for table
  ## Run forecasts  on the mcmc model and return the list of outputs.
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
  dir.create(forecast.dir)
  ## biomass.list will contain one element for each model run
  ## for each catch projections, each having the quantiles for the proj.years
  biomass.list <- NULL
  spr.list <- NULL

  for(forecast in 1:length(catch.levels)) {
    ## Create a new sub-directory for each catch projection
    name <- catch.levels.names[forecast]
    new.forecast.dir <- file.path(forecast.dir, name)
    dir.create(new.forecast.dir)

    ## Copy all model files into this new forecast directory
    file.copy(file.path(mcmc.dir, list.files(mcmc.dir)), new.forecast.dir)

    ## Insert fixed catches into forecast file
    forecast.file <- file.path(new.forecast.dir, "forecast.ss")
    fore <- SS_readforecast(forecast.file, Nfleets=1, Nareas=1, verbose=FALSE)
    fore$Ncatch <- 3
    fore$ForeCatch <- data.frame(Year=forecast.yrs, Seas=1, Fleet=1, Catch_or_F=catch.levels[[forecast]])
    SS_writeforecast(fore, dir = new.forecast.dir, overwrite = TRUE, verbose = FALSE)

    ## Evaluate the model using mceval option of ADMB
    shell.command <- paste0("cd ", new.forecast.dir, " & ss3 -mceval")
    shell(shell.command)
    mcmc.out <- SSgetMCMC(dir=list.dirs(forecast.dir)[forecast+1], writecsv=FALSE)$model1

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
    biomass.list[[forecast]] <- t(apply(sb.proj.cols, 2, quantile, probs=probs))
    spr.list[[forecast]] <- t(apply(spr.proj.cols, 2, quantile, probs=probs))
  }
  names(biomass.list) <- catch.levels.names
  names(spr.list) <- catch.levels.names

  return(list(biomass.list,
              spr.list))
}

calc.mcmc <- function(mcmc,            ## mcmc is the output of the SS_getMCMC function from the r4ss package as a data.frame
                      start.yr = 1966, ## Start year for recruitment
                      yr,              ## The year to calculate mcmc values for. Should be the last year in the time series (without projections).
                      lower = 0.025,   ## Lower quantile for confidence interval calcs
                      upper = 0.975    ## Upper quantile for confidence interval calcs
                      ){
  ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS, SPR
  ## Returns a list of them all

  ## 2e6 used here because biomass will be shown in the millions of tonnes and it is female only
  spb <- mcmc[,grep("SPB",names(mcmc))]/2e6
  spb <- spb[,!names(spb) %in% c("SPB_Virgin", paste0("SPB_",yr+1:20))]

  slower <- apply(spb,2,quantile,prob=lower)
  smed   <- apply(spb,2,quantile,prob=0.5)
  supper <- apply(spb,2,quantile,prob=upper)

  depl <- t(apply(spb,1,function(x){x/x[1]}))[,-1]
  dlower <- apply(depl,2,quantile,prob=lower)
  dmed   <- apply(depl,2,quantile,prob=0.5)
  dupper <- apply(depl,2,quantile,prob=upper)

  ## 1e6 used here because recruitment will be shown in the millions of tonnes
  recr <- mcmc[,grep("Recr_",names(mcmc))]/1e6
  recr <- recr[,-grep("Fore",names(recr))]
  yrs <- unlist(lapply(strsplit(names(recr),"_"),function(x){x[2]}))
  recr <- recr[,yrs%in%c("Virgin",start.yr:yr)]

  rmed <- apply(recr, 2, quantile, prob=0.5)
  rmean <- apply(recr, 2, mean)
  rlower <- apply(recr, 2, quantile,prob=lower)
  rupper <- apply(recr, 2, quantile,prob=upper)

  dev <- mcmc[,grep("Early_InitAge_20",names(mcmc)):
                     grep(paste0("ForeRecr_",yr+2),names(mcmc))]
  devlower <- apply(dev, 2, quantile, prob=lower)
  devmed <- apply(dev, 2, quantile, prob=0.5)
  devupper <- apply(dev, 2, quantile, prob=upper)

  spr <- mcmc[,grep("SPRratio_",names(mcmc))]
  plower <- apply(spr, 2, quantile, prob=lower)
  pmed <- apply(spr, 2, quantile, prob=0.5)
  pupper <- apply(spr, 2, quantile, prob=upper)

  f <- mcmc[,grep("F_",names(mcmc))]
  flower <- apply(f, 2, quantile, prob=lower)
  fmed   <- apply(f, 2, quantile, prob=0.5)
  fupper <- apply(f, 2, quantile, prob=upper)

  return(list(slower=slower, smed=smed, supper=supper,
              dlower=dlower, dmed=dmed, dupper=dupper,
              rlower=rlower, rmed=rmed, rupper=rupper, rmean=rmean,
              devlower=devlower, devmed=devmed, devupper=devupper,
              plower=plower, pmed=pmed, pupper=pupper,
              flower=flower, fmed=fmed, fupper=fupper))
}
