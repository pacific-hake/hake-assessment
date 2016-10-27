load.ss.files <- function(model.dir,
                          key.posts = key.posteriors, ## Vector of key posteriors used to create key posteriors file
                          key.posts.fn = "keyposteriors.csv",
                          nuisance.posts.fn = "nuisanceposteriors.csv",
                          verbose = FALSE){
  ## Load all the SS files for output and input, and return the model object.
  ## If MCMC directory is present, load that and perform calculations for mcmc parameters.

  curr.func.name <- get.curr.func.name()
  ## Load MPD results
  model <- SS_output(dir = model.dir, verbose = verbose)

  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model.dir.listing <- dir(model.dir)
  dat.fn.ind <- grep("_data.ss", model.dir.listing)
  ctl.fn.ind <- grep("_control.ss", model.dir.listing)
  if(!length(dat.fn.ind)){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is no data file. A data file is anything followed by and ending in _data.ss.\n\n")
  }
  if(length(dat.fn.ind) > 1){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is more than one data file. A data file is anything followed by and ending in _data.ss.\n\n")
  }
  if(!length(ctl.fn.ind)){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is no control file. A control file is anything followed by and ending in _control.ss.\n\n")
  }
  if(length(ctl.fn.ind) > 1){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is more than one control file. A control file is anything followed by and ending in _control.ss.\n\n")
  }
  dat.fn <- file.path(model.dir, model.dir.listing[dat.fn.ind])
  ctl.fn <- file.path(model.dir, model.dir.listing[ctl.fn.ind])
  model$path <- model.dir
  model$dat.file <- dat.fn
  model$dat <- SS_readdat(dat.fn)
  model$ctl.file <- ctl.fn
  model$ctl <- readLines(ctl.fn)
  ## Set default mcmc members to NULL. Later code depends on this.
  model$mcmc <- NULL
  model$mcmcpath <- NULL

  ## If it has an 'mcmc' sub-directory, load that as well
  mcmc.dir <- file.path(model.dir, "mcmc")
  if(dir.exists(mcmc.dir)){
    model$mcmc <- data.frame(SSgetMCMC(dir = mcmc.dir, writecsv = FALSE, verbose = verbose)$model1)
    model$mcmcpath <- mcmc.dir
    create.key.nuisance.posteriors.files(model,
                                         key.posts,
                                         key.posts.fn,
                                         nuisance.posts.fn)
    ## Do the mcmc calculations, e.g. quantiles for SB, SPB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc.mcmc(model$mcmc)

  }
  return(model)
}

create.rdata.file <- function(
           models.dir = model.dir,          ## Directory name for all models location
           model.name,                      ## Directory name of model to be loaded
           ovwrt.rdata = FALSE,             ## Overwrite the RData file if it exists?
           run.forecasts = FALSE,           ## Run forecasting metrics for this model? *This will overwrite any already run*
           fore.yrs = forecast.yrs,         ## Vector of years to run forecasting for if run.metrics = TRUE
           forecast.probs = forecast.probs, ## Vector of quantile values if run.metrics = TRUE
           forecast.catch.levels = catch.levels, ## List of catch levels to run forecasting for if run.metrics = TRUE
           load.forecasts = FALSE,          ## Load the forecasts for this model?
           run.retros = FALSE,              ## Run retrospectives for this model? *This will overwrite any already run*
           my.retro.yrs = retro.yrs,        ## Vector of integers (positives) to run retrospectives for if run.retros = TRUE
           load.retros = FALSE,             ## Load retrospectives for this model?
           run.partest = FALSE,             ## Run partest for this model?
           key.posteriors = key.posteriors, ## Vector of key posteriors used to create key posteriors file
           key.posteriors.fn = "keyposteriors.csv",
           nuisance.posteriors.fn = "nuisanceposteriors.csv",
           verbose = FALSE){
  ## Create an rdata file to hold the model's data and outputs.
  ## If an RData file exists, and overwrite is FALSE, return immediately.
  ## If no RData file exists, the model will be loaded from outputs into an R list
  ##  and saved as an RData file in the correct directory.
  ## When this function exits, an RData file will be located in the
  ##  directory given by model.name.
  ## Assumes the files model-setup.r, retrospective-setup.r, and forecast-catch-levels.r
  ##  have been sourced (for default values of args).
  ## Assumes utilities.r has been sourced.
  curr.func.name <- get.curr.func.name()
  model.dir <- file.path(models.dir, model.name)
  if(!dir.exists(model.dir)){
    stop(curr.func.name,"Error - the directory ", model.dir, " does not exist. ",
         "Fix the problem and try again.\n")
  }
  ## The RData file will have the same name as the directory it is in
  rdata.file <- file.path(model.dir, paste0(model.name, ".RData"))
  ## If the user asks to not overwrite Rdata file, but wants to run retros
  ## or forecasting or partest, stop and tell them.
  if(!ovwrt.rdata){
    if(run.forecasts){
      stop(curr.func.name,
           "Error - You have asked to run forecasting metrics, ",
           "but set ovwrt.rdata to FALSE.\n")
    }
    if(run.retros){
      stop(curr.func.name,
           "Error - You have asked to run retrospectives, ",
           "but set ovwrt.rdata to FALSE.\n")
    }
    if(run.partest){
      stop(curr.func.name,
           "Error - You have asked to run partest, ",
           "but set ovwrt.rdata to FALSE.\n")
    }
  }

  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the file
      cat0(curr.func.name, "RData file found in ", model.dir,
           ". Deleting...\n")
      unlink(rdata.file)
    }else{
      cat0(curr.func.name, "RData file found in ", model.dir, "\n")
      return(invisible())
    }
  }

  ## If this point is reached, no RData file exists so it
  ##  has to be built from scratch
  model <- load.ss.files(model.dir)
  if(run.forecasts){
    run.forecasts(model,
                  fore.yrs,
                  forecast.probs,
                  forecast.catch.levels)
  }

  if(load.forecasts){
    model$forecasts <- fetch.forecasts(model$mcmcpath,
                                       fore.yrs, ## [-length(fore.yrs)],
                                       forecast.catch.levels,
                                       probs = forecast.probs)
    model$risks <- calc.risk(model$forecasts,
                             fore.yrs)
  }

  retros.dir <- file.path(model$path, "retrospectives")
  retros.paths <- file.path(retros.dir, paste0("retro-", pad.num(my.retro.yrs, 2)))
  if(run.retros){
    if(dir.exists(retros.dir)){
      unlink(retros.dir, recursive = TRUE)
    }
    cat0(curr.func.name, "Running retrospectives...\n")
    run.retrospectives(model, yrs = my.retro.yrs, verbose = verbose)
  }

  if(load.retros){
    if(all(dir.exists(retros.paths))){
      cat0(curr.func.name, "Loading retrospectives...\n")
      model$retros <- list()
      retros.dir <- file.path(model$path, "retrospectives")
      for(retro in 1:length(my.retro.yrs)){
        retro.dir <- file.path(retros.dir, paste0("retro-", pad.num(my.retro.yrs[retro], 2)))
        model$retros[[retro]] <- SS_output(dir = retro.dir, verbose = verbose)
      }
      cat0(curr.func.name, "Retrospectives loaded.\n")
    }else{
      stop(curr.func.name, "Error - Not all retrospective directories exist.\n",
           "Look at retrospective-setup.r and your directories ",
           "to make sure they are both the same\n",
           "or set run.retros = TRUE.\n")
    }
  }

  if(run.partest){
    cat0(curr.func.name, "Running partest...\n\n")
    ## TODO: Separate the running of the partest model with
    ##  the fetching of it's output.
    run.partest.model(model, output.file = "model-partest.RData",
                      verbose = verbose)
    cat0(curr.func.name, "Partest Completed.\n\n")
  }
  ## Save the model as an RData file
  save(model, file = rdata.file)
  return(invisible())
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

  list(svirg=svirg, sinit=sinit, slower=slower, smed=smed, supper=supper,
       dlower=dlower, dmed=dmed, dupper=dupper,
       rvirg=rvirg, rinit=rinit, runfihed=runfished,
       rlower=rlower, rmed=rmed, rupper=rupper, rmean=rmean,
       devlower=devlower, devmed=devmed, devupper=devupper,
       plower=plower, pmed=pmed, pupper=pupper,
       flower=flower, fmed=fmed, fupper=fupper)
}

run.forecasts <- function(model,
                          forecast.yrs,
                          forecast.probs,
                          catch.levels){
  ## Run forecasting for the model supplied. If there is no mcmc component
  ##  to the model, an error will be given and the program will be stopped.
  curr.func.name <- get.curr.func.name()

  mcmc.path <- model$mcmcpath
  ## forecast.yrs <- forecast.yrs[-length(forecast.yrs)]
  ## Extract the catch level names from the list into a vector
  catch.levels.names <- sapply(catch.levels, "[[", 3)
  ## Make the catch level values a matrix where the columns represent the cases in catch.names
  catch.levels <- sapply(catch.levels, "[[", 1)
  forecasts.path <- file.path(mcmc.path, "forecasts")
  if(dir.exists(forecasts.path)){
    unlink(forecasts.path, recursive = TRUE)
  }
  cat0(curr.func.name, "Running forecasts for model located in ", mcmc.path, "...\n")
  dir.create(forecasts.path)

  for(i in 1:length(forecast.yrs)){
    fore.path <- file.path(forecasts.path, paste0("forecast-year-", forecast.yrs[i]))
    dir.create(fore.path)
    for(level.ind in 1:ncol(catch.levels)){
      ## Create a new sub-directory for each catch projection
      name <- catch.levels.names[level.ind]
      new.forecast.dir <- file.path(fore.path, name)
      dir.create(new.forecast.dir)

      ## Copy all model files into this new forecast directory
      ## file.copy(file.path(mcmc.path, list.files(mcmc.path)), new.forecast.dir, copy.mode = TRUE)
      file.copy(file.path(mcmc.path, list.files(mcmc.path)),
                file.path(new.forecast.dir, list.files(mcmc.path)), copy.mode = TRUE)

      ## Insert fixed catches into forecast file (depending on i)
      forecast.file <- file.path(new.forecast.dir, "forecast.ss")
      fore <- SS_readforecast(forecast.file, Nfleets = 1, Nareas = 1, verbose = FALSE)
      fore$Ncatch <- length(forecast.yrs[1:i])
      ## fore$ForeCatch <- data.frame(Year = forecast.yrs[1:i], Seas = 1, Fleet = 1, Catch_or_F = catch.levels[[level.ind]][1:i])
      fore$ForeCatch <- data.frame(Year = forecast.yrs[1:i], Seas = 1, Fleet = 1, Catch_or_F = catch.levels[,level.ind][1:i])
      SS_writeforecast(fore, dir = new.forecast.dir, overwrite = TRUE, verbose = FALSE)

      ## Evaluate the model using mceval option of ADMB, and retrieve the output
      shell.command <- paste0("cd ", new.forecast.dir, " & ss3 -mceval")
      shell(shell.command)
    }
  }

  cat0(curr.func.name, "Finished running forecasts for model located in ", model$path, "...\n")
}

fetch.forecasts <- function(mcmc.path,
                            forecast.yrs,
                            catch.levels,
                            probs = NULL){ ## Probabilities for table
  ## Fetch the output from previously-run metrics
  ## If the metrics directory does not exist or there is a problem
  ##  loading the metrics, return NULL.
  ## Only the last metrics directory will be loaded because it contains
  ##  all the outputs for all forecast years.

  ## outputs.list holds the outputs from the mcmc models as read in by SSgetMCMC
  curr.func.name <- get.curr.func.name()

  ## Extract the catch level names from the list into a vector
  catch.levels.names <- sapply(catch.levels, "[[", 3)

  outputs.list <- vector(mode = "list", length = length(catch.levels))
  forecasts.path <- file.path(mcmc.path, "forecasts")
  if(!dir.exists(forecasts.path)){
    stop(curr.func.name, "Error - forecasts directory does not exist ",
         "for mcmc model: ", mcmc.path, "\n",
         "Set run.forecasts = TRUE to create it.\n")
    return(NULL)
  }
  ## Get the directory listing and choose the last one for loading
  dir.listing <- dir(forecasts.path)
  fore.path <- file.path(forecasts.path, dir.listing[length(dir.listing)])
  ## Get the directory listing of the last year's forecasts directory and make sure
  ##  it matches what the catch levels are.
  dir.listing <- dir(fore.path)
  if(!identical(catch.levels.names, dir.listing)){
    stop(curr.func.name, "There is a discrepancy between what you have set ",
         "for the catch.levels names \n and what appears in the forecasts directory '",
         fore.path,"'. \n Check the names in both and try again.\n\n")
  }

  for(level.ind in 1:length(catch.levels.names)){
    fore.level.path <- file.path(fore.path, catch.levels.names[level.ind])
    mcmc.out <- SSgetMCMC(dir = fore.level.path, writecsv = FALSE)$model1
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

    outputs.list[[level.ind]]$biomass <- t(apply(sb.proj.cols, 2, quantile, probs=probs))
    outputs.list[[level.ind]]$spr <- t(apply(spr.proj.cols, 2, quantile, probs=probs))
    outputs.list[[level.ind]]$mcmccalcs <- calc.mcmc(mcmc.out)
    outputs.list[[level.ind]]$outputs <- mcmc.out
  }
  names(outputs.list) <- catch.levels.names
  outputs.list
}

calc.risk <- function(forecast.outputs,    ## A list of the output of the SS_getMCMC function, 1 for each catch.level
                      forecast.yrs){       ## A vector of years to do projections for
  ## Calculate the probablities of being under several reference points from one forecast year to the next

  ## risk.list will hold the probabilities of being under several reference points.
  ##  it will be of length 1 less than the number of forecast years, and each element
  ##  will itself be a data.frame of catch levels with those holding the probabilities.
  ## For example, list element 1 will hold the probabilities for each catch.level of being under
  ##  several reference points for the first two years in the forecast.yrs vector
  curr.func.name <- get.curr.func.name()

  metric <- function(x, yr){
    out <- NULL
    out[1] <- max(x[,paste0("ForeCatch_",yr)])
    out[2] <- sum(x[,paste0("SPB_", yr + 1)] < x[,paste("SPB", yr, sep = "_")]) / nrow(x) * 100.0
    out[3] <- sum(x[,paste0("Bratio_", yr + 1)] < 0.40) / nrow(x) * 100.0
    out[4] <- sum(x[,paste0("Bratio_", yr + 1)] < 0.25) / nrow(x) * 100.0
    out[5] <- sum(x[,paste0("Bratio_", yr + 1)] < 0.10) / nrow(x) * 100.0
    out[6] <- sum(x[,paste0("SPRratio_", yr)] > 1.00) / nrow(x) * 100.0
    out[7] <- sum(x[,paste0("ForeCatch_", yr + 1)] < out[1]) / nrow(x) * 100.0
    out
  }
  risk.list <- NULL
  for(i in 1:(length(forecast.yrs) - 1)){
    yr <- forecast.yrs[i]
    risk.list[[i]] <- data.frame()
    for(case in 1:length(forecast.outputs)){
      x <- forecast.outputs[[case]]$outputs
      risk.list[[i]] <- rbind(risk.list[[i]], metric(x, yr))
    }
  }
  return(risk.list)
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

## Load model(s) and return as a list if more than one. If only one,
## return that object.
load.models <- function(model.dir,
                        model.dir.names){
  ret.list = NULL
  model.rdata.files <- file.path(model.dir, model.dir.names, paste0(model.dir.names, ".Rdata"))
  for(i in 1:length(model.rdata.files)){
    load(model.rdata.files[i])
    ret.list[[i]] <- model
    rm(model)
  }
  if(length(model.dir.names) == 1){
    ret.list[[1]]
  }else{
    ret.list
  }
}

delete.all.rdata.files <- function(model.dir){
  ## Delete all RData files from all directories within the model.dir
  
}
