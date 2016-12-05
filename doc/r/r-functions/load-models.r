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
  model$dat <- SS_readdat(dat.fn, version = ss.version, verbose = ss.verbose)
  model$ctl.file <- ctl.fn
  model$ctl <- readLines(ctl.fn)
  ## Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  model$mcmcpath <- NA

  ## If it has an 'mcmc' sub-directory, load that as well
  mcmc.dir <- file.path(model.dir, "mcmc")
  if(dir.exists(mcmc.dir)){
    model$mcmc <- data.frame(SSgetMCMC(dir = mcmc.dir,
                                       writecsv = FALSE,
                                       verbose = ss.verbose)$model1)
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

delete.rdata.files <- function(
           models.dir = model.dir ## Directory name for all models location
           ){
  ## Delete all rdata files found in the subdirectories of the models.dir
  ## directory. This is not undo-able so be careful as all forecasts and
  ## retrospective runs will have to be done again.
  dirs <- dir(models.dir)
  rdata.files <- file.path(models.dir, dirs, paste0(dirs, ".rdata"))
  ans <- readline("This operation cannot be undone, are you sure (y/n)? ")
  if(ans == "Y" | ans == "y"){
    unlink(rdata.files, force = TRUE)
    cat(paste0("Deleted ", rdata.files, "\n"))
    cat("All rdata files were deleted.\n")
  }else{
    cat("No files were deleted.\n")
  }
}

create.rdata.file <- function(
           models.dir = model.dir,          ## Directory name for all models location
           model.name,                      ## Directory name of model to be loaded
           ovwrt.rdata = FALSE,             ## Overwrite the RData file if it exists?
           run.forecasts = FALSE,           ## Run forecasting metrics for this model? *This will overwrite any already run*
           fore.yrs = forecast.yrs,         ## Vector of years to run forecasting for if run.metrics = TRUE
           forecast.probs = forecast.probs, ## Vector of quantile values if run.metrics = TRUE
           forecast.catch.levels = catch.levels, ## List of catch levels to run forecasting for if run.forecasts = TRUE
           run.retros = FALSE,              ## Run retrospectives for this model? *This will overwrite any already run*
           my.retro.yrs = retro.yrs,        ## Vector of integers (positives) to run retrospectives for if run.retros = TRUE
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
  if(!ovwrt.rdata){
    if(run.forecasts){
      stop(curr.func.name,
           "Error - You have asked to run forecasting, ",
           "but set ovwrt.rdata to FALSE.\n")
    }
    if(run.retros){
      stop(curr.func.name,
           "Error - You have asked to run retrospectives, ",
           "but set ovwrt.rdata to FALSE.\n")
    }
  }

  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the RData file
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

  ## Try loading forecasts. If there is a problem,
  ## model$forecasts and model$risks will be NA

  ## message(curr.func.name, "'mcmc' directory does not exist ",
  ##           "for model found in: ", mcmc.path)

  model$forecasts <- fetch.forecasts(model$mcmcpath,
                                     fore.yrs, ## [-length(fore.yrs)],
                                     forecast.catch.levels,
                                     probs = forecast.probs)
  model$risks <- calc.risk(model$forecasts,
                           fore.yrs)

  model$retropath <- file.path(model$path, "retrospectives")
  if(is.null(model$retropath)){
    model$retropath <- NA
  }


  retros.paths <- file.path(model$retropath, paste0("retro-", pad.num(my.retro.yrs, 2)))
  if(run.retros){
    if(dir.exists(model$retropath)){
      unlink(model$retropath, recursive = TRUE)
    }
    cat0(curr.func.name, "Running retrospectives...\n")
    run.retrospectives(model,
                       yrs = my.retro.yrs,
                       verbose = verbose)
  }

  ## Try loading retrospectives. If none are found, model$retros will be NA
  model$retros <- fetch.retros(model$retropath,
                               my.retro.yrs,
                               verbose = verbose)

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
      file.copy(file.path(mcmc.path, list.files(mcmc.path)),
                file.path(new.forecast.dir, list.files(mcmc.path)), copy.mode = TRUE)

      ## Insert fixed catches into forecast file (depending on i)
      forecast.file <- file.path(new.forecast.dir, "forecast.ss")
      fore <- SS_readforecast(forecast.file, Nfleets = 1, Nareas = 1, nseas = 1, verbose = FALSE)
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
  ## Fetch the output from previously-run forecasting
  ## If the forecasts directory does not exist or there is a problem
  ##  loading the forecasts, return NA.

  ## outputs.list holds the outputs from the mcmc models as read in by SSgetMCMC
  curr.func.name <- get.curr.func.name()

  ## Extract the catch level names from the list into a vector
  catch.levels.names <- sapply(catch.levels, "[[", 3)

  ## outputs.list <- vector(mode = "list", length = length(catch.levels))
  outputs.list <- vector(mode = "list", length = length(forecast.yrs))
  for(i in 1:length(forecast.yrs)){
    outputs.list[[i]] <- vector(mode = "list", length = length(catch.levels))
  }
  if(is.null(mcmc.path)){
    return(NA)
  }
  forecasts.path <- file.path(mcmc.path, "forecasts")
  if(!dir.exists(forecasts.path)){
    message(curr.func.name, "'forecasts' directory does not exist ",
            "for mcmc model: ", mcmc.path)
    return(NA)
  }
  ## Get the directory listing and choose the last one for loading
  dir.listing <- dir(forecasts.path)

  for(i in 1:length(forecast.yrs)){
    fore.path <- file.path(forecasts.path, paste0("forecast-year-", forecast.yrs[i]))
    ## fore.path <- file.path(forecasts.path, dir.listing[length(dir.listing)])
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

      outputs.list[[i]][[level.ind]]$biomass <- t(apply(sb.proj.cols, 2, quantile, probs=probs))
      outputs.list[[i]][[level.ind]]$spr <- t(apply(spr.proj.cols, 2, quantile, probs=probs))
      outputs.list[[i]][[level.ind]]$mcmccalcs <- calc.mcmc(mcmc.out)
      outputs.list[[i]][[level.ind]]$outputs <- mcmc.out
      names(outputs.list[[i]]) <- catch.levels.names
    }
  }
  names(outputs.list) <- forecast.yrs
  outputs.list
}

calc.risk <- function(forecast.outputs, ## A list of length = number of forecast years.
                                        ## Each element of the list is a list of the output of the
                                        ## SS_getMCMC function, 1 for each catch.level
                      forecast.yrs){    ## A vector of years to do projections for
  ## Calculate the probablities of being under several reference points from one forecast year to the next
  ## risk.list will hold the probabilities of being under several reference points.
  ##  it will be of length 1 less than the number of forecast years, and each element
  ##  will itself be a data.frame of catch levels with those holding the probabilities.
  ## For example, list element 1 will hold the probabilities for each catch.level of being under
  ##  several reference points for the first two years in the forecast.yrs vector
  ## If forecast.outputs is NA, NA will be returned, otherwise the risk.list will be returned.

  if(length(forecast.outputs) == 1){
    if(is.na(forecast.outputs)){
      return(NA)
    }
  }
  curr.func.name <- get.curr.func.name()

  metric <- function(x, yr){
    out <- NULL
    out[1] <- max(x[, paste0("ForeCatch_", yr)])
    out[2] <- sum(x[, paste0("SPB_", yr + 1)] < x[, paste0("SPB_", yr)]) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1]) / nrow(x) * 100.0
    names(out) <- c(paste0("ForeCatch_", yr),
                    paste0("SPB_", yr + 1, "<SPB_", yr),
                    paste0("Bratio_", yr + 1, "<0.40"),
                    paste0("Bratio_", yr + 1, "<0.25"),
                    paste0("Bratio_", yr + 1, "<0.10"),
                    paste0("SPRratio_", yr, ">1.00"),
                    paste0("ForeCatch_", yr + 1, "<ForeCatch_", yr))
    out
  }
  risk.list <- vector(mode = "list", length = length(forecast.yrs) - 1)
  for(yr in 1:(length(forecast.yrs) - 1)){
    ## outputs is a list of one data frame per case, for the current year yr
    outputs <- lapply(forecast.outputs[[yr]], "[[", "outputs")
    ## This call calculates the metrics for each element in the list (each catch case)
    ##  and binds them together into a data frame.
    risk.list[[yr]] <- do.call("rbind",
                               lapply(outputs,
                                      function(x, yr){metric(x, yr)}, yr = forecast.yrs[yr]))
  }
  names(risk.list) <- names(forecast.outputs[1:(length(forecast.outputs)-1)])
  return(risk.list)
}

run.retrospectives <- function(model,
                               yrs = 1:15,            ## A vector of years to subtract from the model's data to run on.
                               remove.blocks = FALSE,
                               extras = "-nox",       ## Extra switches for the command line.
                               verbose = TRUE){
  ## Runs retrospectives for the given model and for the vector of years given
  ## This will create a 'retrospectives' directory in the same directory as the model resides,
  ##  create a directory for each restrospective year, copy all model files into each directory,
  ##  run the retrospectives, and make a list of the SS_output() call to each
  ## Warning - This function will completely delete all previous retrospectives that have been run without notice.

  ## Create the directory 'retrospectives' which will hold the runs
  ##  erasing the directory recursively if necessary
  retros.dir <- model$retropath
  unlink(retros.dir, recursive = TRUE)
  dir.create(retros.dir)

  ## Create a list for the retros' output to be saved to
  retros.list <- list()

  ## Create a directory for each retrospective, copy files, and run retro
  for(retro in 1:length(yrs)){
    retro.dir <- file.path(retros.dir, paste0("retro-", pad.num(yrs[retro], 2)))
    unlink(retro.dir, recursive = TRUE)
    dir.create(retro.dir)

    ## Copy all required model files into the retrospective directory
    files.to.copy <- file.path(model$path, c(exe.file.name,
                                             starter.file.name,
                                             forecast.file.name,
                                             weight.at.age.file.name,
                                             model$ctl.file,
                                             model$dat.file))
    file.copy(file.path(model$path, files.to.copy), retro.dir)
    starter.file <- file.path(retro.dir, starter.file.name)
    starter <- SS_readstarter(starter.file, verbose = verbose)
    starter$retro_yr <- -yrs[retro]
    starter$init_values_src <- 0
    SS_writestarter(starter, dir = retro.dir, verbose = verbose, overwrite = TRUE)
    if(remove.blocks){
      ctl.file <- file.path(retro.dir, model$ctl.file)
      ctl <- readLines(ctl.file)
      ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
      ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
      unlink(ctl.file)
      writeLines(ctl, ctl.file)
    }
    covar.file <- file.path(retro, "covar.sso")
    unlink(covar.file)
    shell.command <- paste0("cd ", retro.dir, " & ss3 ", extras)
    shell(shell.command)
  }
}

fetch.retros <- function(retro.path, ## The full or reletive path in which the retrospective directories live
                         retro.yrs,  ## A vector of years for the retrospectives
                         verbose = FALSE
                         ){
  ## Fetch the retrospectives and return a list of each. If there are no retrospective
  ##  directories or there is some other problem, NA will be returned.
  curr.func.name <- get.curr.func.name()
  if(is.na(retro.path)){
    return(NA)
  }
  if(!dir.exists(retro.path)){
    return(NA)
  }
  retros.paths <- file.path(retro.path, paste0("retro-", pad.num(retro.yrs, 2)))
  if(all(dir.exists(retros.paths))){
    message(curr.func.name, "Loading retrospectives...\n")
    retros.list <- list()
    for(retro in 1:length(retro.yrs)){
      retro.dir <- file.path(retro.path, paste0("retro-", pad.num(retro.yrs[retro], 2)))
      retros.list[[retro]] <- SS_output(dir = retro.dir, verbose = verbose)
    }
    message(curr.func.name, "Retrospectives loaded for '", retro.path, "'")
  }else{
    message(curr.func.name, "Not all retrospective directories exist in ",
            "'", retro.path ,"'",
            "Look at retrospective-setup.r and your directories ",
            "to make sure they are both the same",
            "or set run.retros = TRUE.")
    return(NA)
  }
  retros.list
}

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

load.models <- function(model.dir,
                        model.dir.names){
  ## Load model(s) and return as a list if more than one. If only one,
  ## return that object.
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

