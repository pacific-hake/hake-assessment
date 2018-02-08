load.ss.files <- function(model.dir,
                          key.posts = key.posteriors, ## Vector of key posteriors used to create key posteriors file
                          key.posts.fn = "keyposteriors.csv",
                          nuisance.posts.fn = "nuisanceposteriors.csv",
                          verbose = FALSE,
                          printstats = FALSE, ## print info on each model loaded via SS_output
                          ss.version = "3.24"){
  ## Load all the SS files for output and input, and return the model object.
  ## If MCMC directory is present, load that and perform calculations for mcmc parameters.
  ## ss.version determines which version of SS_readdat() is used.

  curr.func.name <- get.curr.func.name()
  ## Load MPD results
  model <- SS_output(dir = model.dir, verbose = verbose, printstats = printstats)

  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model.dir.listing <- dir(model.dir)
  dat.fn.ind <- grep("_data.ss", model.dir.listing)
  ctl.fn.ind <- grep("_control.ss", model.dir.listing)
  par.fn.ind <- grep("ss.par", model.dir.listing)
  if(!length(dat.fn.ind)){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is no data file. A data file is any file whose name contains the text _data.ss.\n\n")
  }
  if(length(dat.fn.ind) > 1){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is more than one data file. A data file is any file whose name contains the text _data.ss.\n\n")
  }
  if(!length(ctl.fn.ind)){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is no control file. A control file is any file whose name contains the text _control.ss.\n\n")
  }
  if(length(ctl.fn.ind) > 1){
    stop(curr.func.name, "Error in model ", model.dir,
         ", there is more than one control file. A control file is any file whose name contains the text _control.ss.\n\n")
  }
  dat.fn <- file.path(model.dir, model.dir.listing[dat.fn.ind])
  ctl.fn <- file.path(model.dir, model.dir.listing[ctl.fn.ind])
  par.fn <- file.path(model.dir, model.dir.listing[par.fn.ind])
  model$path <- model.dir
  model$dat.file <- dat.fn
  model$dat <- SS_readdat(dat.fn, version = ss.version, verbose = ss.verbose)
  model$ctl.file <- ctl.fn
  model$ctl <- readLines(ctl.fn)
  model$par.file <- par.fn
  ## model$par <- readLines(par.fn)
  ## Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  ## Set the mcmc path. This doesn't mean it exists.
  mcmc.dir <- file.path(model.dir, "mcmc")
  model$mcmcpath <- mcmc.dir

  ## If it has an 'mcmc' sub-directory, load that as well
  if(dir.exists(mcmc.dir)){
    model$mcmc <- data.frame(SSgetMCMC(dir = mcmc.dir,
                                       writecsv = FALSE,
                                       verbose = ss.verbose)$model1)
    # replace any SPB with SSB
    names(model$mcmc) <- gsub(pattern="SPB", replacement="SSB", names(model$mcmc))
    create.key.nuisance.posteriors.files(model,
                                         key.posts,
                                         key.posts.fn,
                                         nuisance.posts.fn)
    ## Do the mcmc calculations, e.g. quantiles for SB, SSB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc.mcmc(model$mcmc)

  }
  return(model)
}

delete.rdata.files <- function(
           models.dir = model.dir ## Directory name for all models location
           ){
  ## Delete all rdata files found in the subdirectories of the models.dir
  ## directory.
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

delete.dirs <- function(models.dir = model.dir, ## Directory name for all models location
                        sub.dir = NULL){        ## The subdirectory to delete recursively
  ## Delete all directories and files of sub.dir
  dirs <- dir(models.dir)
  files <- file.path(models.dir, dirs, sub.dir)
  unlink(files, recursive = TRUE, force = TRUE)
  cat("All files and directories were deleted from the",
      sub.dir, "directory in each model directory.\n")
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
           run.extra.mcmc = FALSE,          ## Run extra mcmc output (a report file for each of the mcmc samples)
           key.posteriors = key.posteriors, ## Vector of key posteriors used to create key posteriors file
           key.posteriors.fn = "keyposteriors.csv",
           nuisance.posteriors.fn = "nuisanceposteriors.csv",
           ss.version = "3.24",
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
  ## ss.version determines which version of SS_readdat() is used.
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
           "but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.")
    }
    if(run.retros){
      stop(curr.func.name,
           "Error - You have asked to run retrospectives, ",
           "but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.")
    }
    if(run.extra.mcmc){
      stop(curr.func.name,
           "Error - You have asked to run the extra mcmc output, ",
           "but set ovwrt.rdata to FALSE.\n",
           "Set ovwrt.rdata to TRUE and try again.")
    }
  }
  if(file.exists(rdata.file)){
    if(ovwrt.rdata){
      ## Delete the RData file
      cat0(curr.func.name, "RData file found in ", model.dir,
           ". Deleting...\n")
      unlink(rdata.file, force = TRUE)
    }else{
      cat0(curr.func.name, "RData file found in ", model.dir, "\n")
      return(invisible())
    }
  }else{
    cat0(curr.func.name, "No RData file found in ", model.dir,
         ". Creating one now.\n")
  }

  ## If this point is reached, no RData file exists so it
  ##  has to be built from scratch
  model <- load.ss.files(model.dir,
                         ss.version = ss.version)

  ##----------------------------------------------------------------------------
  ## Run extra mcmc output.
  model$extra.mcmc.path <- file.path(model$path, "extra-mcmc")
  if(run.extra.mcmc){
    run.extra.mcmc.models(model, verbose = verbose)
  }
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Run forecasts
  if(run.forecasts){
    run.forecasts(model,
                  fore.yrs,
                  forecast.probs,
                  forecast.catch.levels)
  }
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Run retrospectives
  model$retropath <- file.path(model$path, "retrospectives")
  if(is.null(model$retropath)){
    model$retropath <- NA
  }
  if(run.retros){
    run.retrospectives(model,
                       yrs = my.retro.yrs,
                       verbose = verbose)
  }
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Load forecasts.  If none are found or there is a problem, model$forecasts
  ##  will be NA
  model$forecasts <- fetch.forecasts(model$mcmcpath,
                                     fore.yrs,
                                     forecast.catch.levels,
                                     fore.probs = forecast.probs)
  model$risks <- calc.risk(model$forecasts,
                           forecast.catch.levels,
                           fore.yrs)
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Load retrospectives. If none are found or there is a problem, model$retros
  ##  will be NA
  model$retros <- fetch.retros(model$retropath,
                               my.retro.yrs,
                               verbose = verbose)
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Try loading extra mcmc output. If none are found or there is a problem,
  ##  model$extra.mcmc will be NA
  model$extra.mcmc <- fetch.extra.mcmc(model,
                                       verbose = verbose)
  ##----------------------------------------------------------------------------

  ## Save the model as an RData file
  save(model, file = rdata.file)
  return(invisible())
}

calc.mcmc <- function(mcmc,
                      lower = 0.025,
                      upper = 0.975){
  ## Return a list of mcmc calculations, e.g. quantiles for various values
  ##
  ## mcmc - is the output of the SS_getMCMC function from the r4ss
  ##  package as a data.frame
  ## lower - lower quantile for confidence interval calcs
  ## upper - upper quantile for confidence interval calcs

  ## 2e6 used here because biomass will be shown in the millions of tonnes
  ##  and it is female only.
  ssb <- mcmc[,grep("SSB",names(mcmc))]/2e6
  svirg <- quantile(ssb[,names(ssb) == "SSB_Virgin"],
                    c(lower, 0.5, upper))
  sinit <- quantile(ssb[,names(ssb) == "SSB_Initial"],
                    c(lower, 0.5, upper))

  ## sinit.post is saved here so that depletion calculations can be done for
  ##  each posterior,
  sinit.post <- ssb[,names(ssb) == "SSB_Initial"]

  names(ssb) <- gsub("SSB_", "", names(ssb))
  cols.to.strip <- c("Virgin", "Initial")
  ssb <- strip.columns(ssb, cols.to.strip)

  slower <- apply(ssb, 2, quantile, prob = lower)
  smed   <- apply(ssb, 2, quantile, prob = 0.5)
  supper <- apply(ssb, 2, quantile, prob = upper)

  depl   <- apply(ssb, 2, function(x){x / sinit.post})
  dlower <- apply(depl, 2, quantile, prob = lower)
  dmed   <- apply(depl, 2, quantile, prob = 0.5)
  dupper <- apply(depl, 2, quantile, prob = upper)

  ## 1e6 used here because recruitment will be shown in the millions of tonnes
  recr <- mcmc[,grep("Recr_", names(mcmc))] / 1e6
  recr <- recr[,-grep("Fore", names(recr))]
  names(recr) <- gsub("Recr_", "", names(recr))
  rvirg <- quantile(recr[,names(recr) == "Virgin"],
                    c(lower, 0.5, upper))
  rinit <- quantile(recr[,names(recr) == "Initial"],
                    c(lower, 0.5, upper))
  runfished <- quantile(recr[,names(recr) == "Unfished"],
                        c(lower, 0.5, upper))

  cols.to.strip <- c("Virgin", "Initial", "Unfished")
  recr <- strip.columns(recr, cols.to.strip)

  rmed <- apply(recr, 2, quantile, prob = 0.5)
  rmean <- apply(recr, 2, mean)
  rlower <- apply(recr, 2, quantile,prob = lower)
  rupper <- apply(recr, 2, quantile,prob = upper)

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
  early <- grep("Early_InitAge_", names(dev))
  num.early.yrs <- length(early)
  early.yrs <- seq(start.yr - num.early.yrs, start.yr - 1, 1)
  late.yrs <- names(dev[-early])
  names(dev) <- c(as.character(early.yrs), late.yrs)

  devlower <- apply(dev, 2, quantile, prob = lower)
  devmed <- apply(dev, 2, quantile, prob = 0.5)
  devupper <- apply(dev, 2, quantile, prob = upper)

  spr <- mcmc[,grep("SPRratio_", names(mcmc))]
  names(spr) <- gsub("SPRratio_", "", names(spr))

  plower <- apply(spr, 2, quantile, prob = lower)
  pmed <- apply(spr, 2, quantile, prob = 0.5)
  pupper <- apply(spr, 2, quantile, prob = upper)

  f <- mcmc[,grep("F_", names(mcmc))]
  names(f) <- gsub("F_", "", names(f))
  flower <- apply(f, 2, quantile, prob = lower)
  fmed   <- apply(f, 2, quantile, prob = 0.5)
  fupper <- apply(f, 2, quantile, prob = upper)

  ## Calculations for the reference points table
  probs <- c(lower, 0.5, upper)

  unfish.fem.bio <-
    f(round(quantile(mcmc$SSB_Virgin,
                     prob = probs) / 2e6, 3) * 1000,
      0)
  unfish.recr <-
    f(round(quantile(mcmc$Recr_Virgin,
                     prob = probs) / 1e6, 3) * 1000,
      0)
  f.spawn.bio.bf40 <-
    f(round(quantile(mcmc$SSB_SPR,
                     prob = probs) / 2e6, 3) * 1000,
      0)
  spr.msy.proxy <- c(latex.bold("--"),
                     "40\\%",
                     latex.bold("--"))
  exp.frac.spr <-
    paste0(f(100 * quantile(mcmc$Fstd_SPR,
                            prob = probs),
             1),
           "\\%")
  yield.bf40 <-
    f(round(quantile(mcmc$Dead_Catch_SPR,
                     prob = probs) / 1e6, 3) * 1000,
      0)
  fem.spawn.bio.b40 <-
    f(round(quantile(mcmc$SSB_Btgt,
                     prob = probs) / 2e6, 3) * 1000,
      0)
  spr.b40 <-
    paste0(f(100 * quantile(mcmc$SPR_Btgt,
                            prob = probs),
             1),
           "\\%")
  exp.frac.b40 <-
    paste0(f(100 * quantile(mcmc$Fstd_Btgt,
                            prob = probs),
             1),
           "\\%")
  yield.b40 <-
    f(round(quantile(mcmc$Dead_Catch_Btgt,
                     prob = probs) / 1e6, 3) * 1000,
      0)
  fem.spawn.bio.bmsy <-
    f(round(quantile(mcmc$SSB_MSY,
                     prob = probs) / 2e6, 3) * 1000,
      0)
  spr.msy <- paste0(f(100 * quantile(mcmc$SPR_MSY,
                                     prob = probs),
                      1),
                    "\\%")
  exp.frac.sprmsy <-
    paste0(f(100 * quantile(mcmc$Fstd_MSY,
                            prob = probs),
             1),
           "\\%")
  msy <-
    f(round(quantile(mcmc$Dead_Catch_MSY,
                     prob = probs) / 1e6, 3) * 1000,
      0)

  ## Return a list of the calculated values
  sapply(c("svirg",
           "sinit",
           "slower",
           "smed",
           "supper",
           "dlower",
           "dmed",
           "dupper",
           "rvirg",
           "rinit",
           "runfished",
           "rlower",
           "rmed",
           "rupper",
           "rmean",
           "devlower",
           "devmed",
           "devupper",
           "plower",
           "pmed",
           "pupper",
           "flower",
           "fmed",
           "fupper",
           ## Reference points
           "unfish.fem.bio",
           "unfish.recr",
           "f.spawn.bio.bf40",
           "spr.msy.proxy",
           "exp.frac.spr",
           "yield.bf40",
           "fem.spawn.bio.b40",
           "spr.b40",
           "exp.frac.b40",
           "yield.b40",
           "fem.spawn.bio.bmsy",
           "spr.msy",
           "exp.frac.sprmsy",
           "msy"),
         function(x){get(x)})
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

  cat0(curr.func.name, "Running forecasts for model located in ", mcmc.path, "...\n")
  dir.create(forecasts.path, showWarnings = FALSE)

  for(i in 1:length(forecast.yrs)){
    fore.path <- file.path(forecasts.path, paste0("forecast-year-", forecast.yrs[i]))
    dir.create(fore.path, showWarnings = FALSE)
    for(level.ind in 1:ncol(catch.levels)){
      ## Create a new sub-directory for each catch projection
      name <- catch.levels.names[level.ind]
      new.forecast.dir <- file.path(fore.path, name)
      dir.create(new.forecast.dir, showWarnings = FALSE)

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
                            fore.probs = NULL){ ## Probabilities for table
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

      outputs.list[[i]][[level.ind]]$biomass <- t(apply(sb.proj.cols, 2, quantile, probs=fore.probs))
      outputs.list[[i]][[level.ind]]$spr <- t(apply(spr.proj.cols, 2, quantile, probs=fore.probs))
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
                      catch.levels,     ## The catches to use in the table
                      forecast.yrs){    ## A vector of years to do projections for
  ## Calculate the probablities of being under several reference points from one forecast year to the next
  ## risk.list will hold the probabilities of being under several reference points.
  ##  it will be of length 1 less than the number of forecast years, and each element
  ##  will itself be a data.frame of catch levels with those holding the probabilities.
  ## For example, list element 1 will hold the probabilities for each catch.level of being under
  ##  several reference points for the first two years in the forecast.yrs vector
  ## If forecast.outputs is NA, NA will be returned, otherwise the risk.list will be returned.

  ## Make the catch level values a matrix where the columns represent the cases in catch.names
  catch.levels <- sapply(catch.levels, "[[", 1)

  if(length(forecast.outputs) == 1){
    if(is.na(forecast.outputs)){
      return(NA)
    }
  }
  curr.func.name <- get.curr.func.name()

  metric <- function(case.ind, x, yr, yr.ind){
    out <- NULL
    out[1] <- catch.levels[yr.ind, case.ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] < x[, paste0("SSB_", yr)]) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1]) / nrow(x) * 100.0
    names(out) <- c(paste0("ForeCatch_", yr),
                    paste0("SSB_", yr + 1, "<SSB_", yr),
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
    ##  and binds them together into a data frame. If there was a problem,
    ##  (e.g. a bridge model is set up for forecasting) it will be set to NA.
    risk.list[[yr]] <- tryCatch({
      do.call("rbind",
              lapply(1:length(outputs),
                     function(ind, yr, yr.ind){
                       metric(ind, outputs[[ind]], yr, yr.ind)
                     },
                     yr = forecast.yrs[yr],
                     yr.ind = yr))
    }, error = function(e){
      NA
    })
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
  if(is.na(model$retropath)){
    return(invisible())
  }

  retros.dir <- model$retropath
  dir.create(retros.dir, showWarnings = FALSE)

  ## Create a list for the retros' output to be saved to
  retros.list <- list()

  ## Create a directory for each retrospective, copy files, and run retro
  for(retro in 1:length(yrs)){
    retro.dir <- file.path(retros.dir, paste0("retro-", pad.num(yrs[retro], 2)))
    dir.create(retro.dir, showWarnings = FALSE)

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
    covar.file <- file.path(retro.dir, "covar.sso")
    file.remove(covar.file)
    shell.command <- paste0("cd ", retro.dir, " & ss3 ", extras)
    shell(shell.command)
  }
}

fetch.retros <- function(retro.path, ## The full or reletive path in which the retrospective directories live
                         retro.yrs,  ## A vector of years for the retrospectives
                         verbose = FALSE,
                         printstats = FALSE  ## print info on each model loaded via SS_output
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
      retros.list[[retro]] <- SS_output(dir = retro.dir, verbose = verbose,
                                        printstats = printstats)
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

run.extra.mcmc.models <- function(model, verbose = TRUE){
  ## This Re-runs the model (MLE) once for each posterior
  ## and saves the Report.sso files in model$extra.mcmc.path/report
  curr.func.name <- get.curr.func.name()

  mcmc.dir <- model$mcmcpath
  if(!dir.exists(mcmc.dir)){
    return(invisible())
  }

  if(!verbose){
    flush.console
    cat0(curr.func.name, "Running model for each mcmc sample to get additional mcmc outputs...\n\n")
  }

  ## Create the directories extra-mcmc and extra-mcmc/reports
  ##  which will hold the runs
  extra.mcmc.dir <- model$extra.mcmc.path
  dir.create(extra.mcmc.dir, showWarnings = FALSE)
  reports.dir <- file.path(extra.mcmc.dir, "reports")
  dir.create(reports.dir, showWarnings = FALSE)

  ## Copy all mcmc model files into the extra-mcmc directory
  file.copy(file.path(mcmc.dir, list.files(mcmc.dir)), extra.mcmc.dir)
  posts <- read.table(file.path(extra.mcmc.dir, "posteriors.sso"), header = TRUE)
  ## Change this for testing on smaller subset of posteriors
  num.posts <- nrow(posts)
  checksum <- 999 # just a code, unrelated to num.posts
  ## create a table of parameter values based on labels in parameters section of Report.sso
  newpar <- data.frame(value = c(1, model$parameters$Value, checksum),
                       hash = "#",
                       label = c("dummy_parm", model$parameters$Label, "checksum999"),
                       stringsAsFactors = FALSE)

  ## add hash before first column name
  names(newpar)[1] <- "#value"

  ## change labels parameters like "SR_LN(R0)" to "SR_LN.R0."
  ## to match what read.table does to posteriors.sso
  newpar$label <- gsub(pattern="(", replacement=".", newpar$label, fixed=TRUE)
  newpar$label <- gsub(pattern=")", replacement=".", newpar$label, fixed=TRUE)
  
  ## write table of new files
  write.table(x = newpar,
              file = file.path(extra.mcmc.dir, "ss.par"),
              quote = FALSE, row.names=FALSE)

  start <- SS_readstarter(file.path(extra.mcmc.dir, "starter.ss"), verbose = verbose)
  ## Change starter file to read from par file
  start$init_values_src <- 1
  SS_writestarter(start, dir = extra.mcmc.dir, file = "starter.ss", overwrite = TRUE, verbose=F)

  ## modify control file to make bias adjustment of recruit devs = 1.0 for all years
  ## this is required to match specification used by MCMC as noted in
  ## "Spawner-Recruitment" section of SS User Manual and described in
  ## Methot & Taylor (2011)
  ctl.lines <- readLines(file.path(extra.mcmc.dir, start$ctlfile))
  bias.adjust.line.num <- grep("Maximum bias adjustment in MPD", ctl.lines)
  if(length(bias.adjust.line.num)==0){
    # alternative label used in control.ss_new file
    bias.adjust.line.num <- grep("max_bias_adj_in_MPD", ctl.lines)
  }
  ctl.lines[bias.adjust.line.num] <-
    "-1      # Maximum bias adjustment in MPD (set to -1 for extra.mcmc only)"
  writeLines(ctl.lines, file.path(extra.mcmc.dir, start$ctlfile))

  ## Remove brackets in newpar labels so that the names match column names in posts
  ## this line may be redundant with the gsub commands above
  newpar$label <- gsub("\\(([0-9])\\)", ".\\1.", newpar$label)

  ## loop over rows of posteriors file
  for(irow in 1:num.posts){
    if(verbose){
      cat("irow:", irow, "natM:", newpar[2], "\n")
    }
    ## replace values in newpar table with posteriors values
    ## (excluding 1 and 2 for "Iter" and "Objective_function")
    newpar[newpar$label %in% names(posts), 1] <- as.numeric(posts[irow, -(1:2)])
    write.table(x = newpar,
                file = file.path(extra.mcmc.dir, "ss.par"),
                quote = FALSE,
                row.names = FALSE)
    file.copy(file.path(extra.mcmc.dir, "ss.par"),
              file.path(reports.dir, paste0("ss_input", irow, ".par")),
              overwrite = TRUE)
    ## delete existing output files to make sure that if model fails to run,
    ## it won't just copy the same files again and again
    file.remove(file.path(extra.mcmc.dir, "Report.sso"))
    file.remove(file.path(extra.mcmc.dir, "CompReport.sso"))

    shell.command <- paste0("cd ", extra.mcmc.dir, " & ss3 -maxfn 0 -phase 10 -nohess")
    if(verbose){
      ## shell doesn't accept the argument show.output.on.console for some reason
      shell(shell.command)
    }else{
      ## This doesn't work!!
      shell(shell.command)
      ## system(shell.command, show.output.on.console = FALSE)
    }
    file.copy(file.path(extra.mcmc.dir, "ss.par"),
              file.path(reports.dir, paste0("ss_output", irow, ".par")),
              overwrite = TRUE)
    file.copy(file.path(extra.mcmc.dir, "Report.sso"),
              file.path(reports.dir, paste0("Report_", irow, ".sso")),
              overwrite = TRUE)
    file.copy(file.path(extra.mcmc.dir, "CompReport.sso"),
              file.path(reports.dir, paste0("CompReport_", irow, ".sso")),
              overwrite = TRUE)
  }

  cat0(curr.func.name, "Extra mcmc output model runs completed.\n\n")
}

fetch.extra.mcmc <- function(model,
                             verbose = FALSE){
  ## Create and return a list of stats to attach to the main model by
  ## looking in path for the report files.
  extra.mcmc.path <- model$extra.mcmc.path
  reports.dir <- file.path(extra.mcmc.path, "reports")
  curr.func.name <- get.curr.func.name()
  if(is.na(extra.mcmc.path)){
    return(NA)
  }
  if(!dir.exists(extra.mcmc.path)){
    return(NA)
  }
  if(!dir.exists(reports.dir)){
    return(NA)
  }

  extra.mcmc.path <- model$extra.mcmc.path
  ## Get the number of Report.sso files in the directory
  dir.list <- dir(reports.dir)
  if(!length(dir.list)){
    return(NA)
  }
  num.reports <- length(grep("^Report_[[:digit:]]+\\.sso$", dir.list))
  num.comp.reports <- length(grep("^CompReport_[[:digit:]]+\\.sso$", dir.list))
  posts <- read.table(file.path(extra.mcmc.path, "posteriors.sso"),
                      header = TRUE,
                      fill = TRUE,
                      stringsAsFactors = FALSE)
  message(curr.func.name, "Reading extra MCMC output from", extra.mcmc.path)

  ## Data frame to store likelihood components
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

  ## Objects to store selectivity, select*wt, and numbers at age
  sel.table <- NULL
  selwt.table <- NULL
  natage.table <- NULL

  ## unique strings associated with rows reporting selectivity and numbers at age
  sel.text1 <- paste0(model$endyr+1, "_1Asel")
  sel.text2 <- paste0(model$endyr+1, "_1_sel*wt")
  natage.text <- "Z_AT_AGE_Annual_2 With_fishery"

  ## Object to store total biomass
  Bio_all <- NULL

  ## loop over all report files to extract quantities
  for(irow in 1:num.reports){
    # read full report file as strings
    rep.file <- file.path(reports.dir, paste0("Report_", irow,".sso"))
    tmp <- readLines(rep.file)
    # find section on likelihoods and read as a table
    skip.row <- grep("LIKELIHOOD", tmp)[2]
    likes <- read.table(rep.file,
                        skip = skip.row,
                        nrows = 17,
                        fill = TRUE,
                        row.names = NULL,
                        col.names = 1:4,
                        stringsAsFactors = FALSE)
    # extract likelihoods from table and make numeric
    like.info[irow, 2:10] <- as.numeric(likes$X2[3:11])  ## fleet-aggregated likelihoods
    like.info[irow, 11] <- as.numeric(likes[17, 3])      ## fleet-specific age comp likelihoods
    like.info[irow, 12] <- as.numeric(likes[17, 4])      ## fleet-specific age comp likelihoods

    # find lines in report file containing unique strings related to selectivity
    sel.line1 <- grep(sel.text1, tmp)
    sel.line2 <- grep(sel.text2, tmp, fixed=TRUE)
    cat("Loading report file: ", rep.file, "\n")
    # read individual rows of selectivity info
    sel.row1 <- read.table(file=rep.file, skip=sel.line1-1, nrow=1)
    sel.row2 <- read.table(file=rep.file, skip=sel.line2-1, nrow=1)

    # read numbers at age table based on start and end lines and length of table
    natage.line.start <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", tmp)
    natage.line.end <- grep("Z_AT_AGE_Annual_2 With_fishery", tmp)-3
    natage.N.lines <- natage.line.end - natage.line.start
    natage.allrows <- read.table(file=rep.file, skip=natage.line.start,
                                 nrow=natage.N.lines, header=TRUE)
    ## subset all rows to select first forecast year
    nms <- colnames(natage.allrows)
    nms[nms == "Year"] <- "Yr"
    colnames(natage.allrows) <- nms
    natage.row <- natage.allrows[natage.allrows$Yr==model$endyr + 1,]

    # add rows to tables of values for each MCMC sample
    sel.table <- rbind(sel.table, sel.row1)
    selwt.table <- rbind(selwt.table, sel.row2)
    natage.table <- rbind(natage.table, natage.row)

    # read time series table to get total biomass
    # (in the future we could add more things from the timeseries table)
    ts.start <- grep("^TIME_SERIES", tmp) + 1 # row with header
    ts.end <- grep("^SPR_series", tmp) - 2 # final row
    ts <- read.table(rep.file, header=TRUE, skip=ts.start-1, nrows=ts.end - ts.start)
    Bio_all <- cbind(Bio_all, ts$Bio_all)
  }

  ## Make sure the number of rows matches the number of posteriors
  like.info <- like.info[like.info$Equil_catch != 0 &
                         like.info$Survey !=0 &
                         like.info$Age_comp != 0 &
                         like.info$Recruitment != 0 &
                         like.info$Parm_priors != 0,]

  ## Process selectivity values
  ## remove initial columns (containing stuff like Gender and Year)
  natage.table.slim <- natage.table[,-(1:3)]
  sel.table.slim <- sel.table[,-(1:7)]
  selwt.table.slim <- selwt.table[,-(1:7)]

  ## selected biomass by age is product of numbers*selectivity*weight at each age
  natselwt <- natage.table.slim*selwt.table.slim
  ## selected numbers by age is product of numbers*selectivity at each age
  natsel <- natage.table.slim*sel.table.slim

  ## define new objects to store proportions by age
  natsel.prop <- natsel
  natselwt.prop <- natselwt

  ## create tables of proportions by dividing by sum of each row
  for(irow in 1:num.reports){
    natsel.prop[irow,] <- natsel[irow,]/sum(natsel[irow,])
    natselwt.prop[irow,] <- natselwt[irow,]/sum(natselwt[irow,])
  }

  if(verbose){
    cat0(curr.func.name, "Reading comp table\n\n")
    flush.console()
  }
  ## read expected proportions and Pearson values for each age comp observations
  tmp <- readLines(file.path(reports.dir, paste0("CompReport_", irow,".sso")))
  skip.row <- grep("Composition_Database", tmp)
  comp.table <- read.table(file.path(extra.mcmc.path, "CompReport.sso"),
                           skip = skip.row,
                           header = TRUE,
                           fill = TRUE,
                           stringsAsFactors = FALSE)

  ## loop to create columns Exp1, Exp2, ..., Exp999 and Pearson1, Pearson2, etc.
  for(irow in 1:num.comp.reports){
    if(verbose & (irow %% 100 == 0)){
      print(irow)
    }
    tmp <- readLines(file.path(reports.dir, paste0("CompReport_", irow,".sso")))
    skip.row <- grep("Composition_Database", tmp)
    comps <- read.table(file.path(reports.dir, paste0("CompReport_", irow, ".sso")),
                        skip = skip.row,
                        header = TRUE,
                        fill = TRUE,
                        stringsAsFactors = FALSE)
    lab1 <- paste0("Pearson", irow)
    lab2 <- paste0("Exp", irow)
    comp.table[lab1] <- comps$Pearson
    comp.table[lab2] <- comps$Exp
  }
  ## filter out values that are not included in agedbase within base model
  comp.table <- comp.table[!is.na(comp.table$N) & comp.table$N>0,]

  ## median and quantiles of expected values and Pearsons
  exp.table <- comp.table[,names(comp.table) %in% paste0("Exp", 1:num.comp.reports)]
  Pearson.table <- comp.table[,names(comp.table) %in% paste0("Pearson", 1:num.comp.reports)]
  exp.median <- apply(exp.table, MARGIN = 1, FUN = median)
  exp.low <- apply(exp.table, MARGIN = 1, FUN = quantile, probs = 0.025)
  exp.high <- apply(exp.table, MARGIN = 1, FUN = quantile, probs = 0.975)
  Pearson.median <- apply(Pearson.table, MARGIN = 1, FUN = median)
  Pearson.low <- apply(Pearson.table, MARGIN = 1, FUN = quantile, probs = 0.025)
  Pearson.high <- apply(Pearson.table, MARGIN = 1, FUN = quantile, probs = 0.975)

  # get index fits from CPUE table
  if(verbose){
    cat0(curr.func.name, "Reading cpue table\n\n")
    flush.console()
  }
  cpue.table <- NULL
  Q.vector <- NULL
  for(irow in 1:num.reports){
    if(verbose & (irow %% 100 == 0)){
      print(irow)
    }
    tmp <- readLines(file.path(reports.dir, paste0("Report_", irow,".sso")))
    skip.row <- grep("INDEX_2", tmp)[2]
    # number of CPUE values includes dummy values for in-between years
    # reading these values is needed to get expected survey biomass in those years
    ncpue <- nrow(model$dat$CPUE)
    cpue <- read.table(file.path(reports.dir, paste0("Report_", irow,".sso")),
                       skip = skip.row,
                       nrows = ncpue, ## number of survey index points
                       header = TRUE,
                       fill = TRUE,
                       stringsAsFactors = FALSE)
    lab1 <- paste0("Exp", irow)
    cpue.table <- cbind(cpue.table, cpue$Exp)
    Q.vector <- c(Q.vector, cpue$Calc_Q[1]) # values are the same for all rows
  }

  ## Build the list of extra mcmc outputs and return
  extra.mcmc <- model

  ## add information on posterior distribution to existing agedbase data frame
  extra.mcmc$agedbase$Exp <- exp.median
  extra.mcmc$agedbase$Exp.025 <- exp.low
  extra.mcmc$agedbase$Exp.975 <- exp.high
  extra.mcmc$agedbase$Pearson <- Pearson.median
  extra.mcmc$agedbase$Pearson.025 <- Pearson.low
  extra.mcmc$agedbase$Pearson.975 <- Pearson.high

  ## add new table to output containing info on posterior distribution of index fits
  extra.mcmc$cpue.table <- cpue.table
  extra.mcmc$cpue.median <- apply(cpue.table, MARGIN = 1, FUN = median)
  extra.mcmc$cpue.025 <- apply(cpue.table, MARGIN = 1, FUN = quantile, probs = 0.025)
  extra.mcmc$cpue.975 <- apply(cpue.table, MARGIN = 1, FUN = quantile, probs = 0.975)
  extra.mcmc$Q_vector <- Q.vector

  ## add new table of info on posterior distributions of likelihoods
  extra.mcmc$like.info <- like.info

  ## add new table vectors containing expected proportions in first forecast year
  extra.mcmc$natsel.prop <- natsel.prop
  extra.mcmc$natselwt.prop <- natselwt.prop

  ## add info on distribution of total biomass to existing time series data frame
  extra.mcmc$timeseries$Bio_all <- apply(Bio_all, MARGIN = 1, FUN = median)
  extra.mcmc$timeseries$Bio_all.0.025 <- apply(Bio_all, MARGIN = 1,
                                               FUN = quantile, probs = 0.025)
  extra.mcmc$timeseries$Bio_all.0.975 <- apply(Bio_all, MARGIN = 1,
                                               FUN = quantile, probs = 0.975)

  message(curr.func.name, paste("Completed read of extra MCMC output."))

  extra.mcmc
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
                        model.dir.names,
                        ret.single.list = FALSE){
  ## Load model(s) and return as a list if more than one. If only one,
  ## return that object or if ret.single.list is TRUE, return a 1-element list.
  ret.list = NULL
  model.rdata.files <- file.path(model.dir, model.dir.names, paste0(model.dir.names, ".Rdata"))
  for(i in 1:length(model.rdata.files)){
    load(model.rdata.files[i])
    ret.list[[i]] <- model
    rm(model)
  }
  if(length(model.dir.names) == 1){
    if(ret.single.list){
      ret.list
    }else{
      ret.list[[1]]
    }
  }else{
    ret.list
  }
}
