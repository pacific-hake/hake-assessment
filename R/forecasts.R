#' Run forecasting for the model supplied
#'
#' @details If there is no mcmc component to the model, an error will be given and the program will be stopped
#' 
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast.yrs A vector of years to forecast
#' @param forecast.probs A vector of quantiles
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#' @param exe.file.name SS executable file name
#'
#' @return [base::invisible()]
#' @export
run.forecasts <- function(model = NA,
                          forecast.yrs = NA,
                          forecast.probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                          catch.levels = NA,
                          exe.file.name = "ss"){

  stopifnot(!is.na(model),
            !is.na(forecast.yrs),
            !is.na(forecast.probs))
  
  mcmc.path <- model$mcmcpath
  
  # Calculate and add on model-custom catch levels
  catch.levels.path <- file.path(mcmc.path, "catch-levels")
  if(!dir.exists(catch.levels.path)){
    # TODO
    calc.catch.levels(model,
                      forecast.yrs,
                      catch.levels,
                      catch.levels.path = "catch-levels",
                      default.hr.path = "default-hr",
                      stable.catch.path = "stable-catch",
                      spr.100.path = "spr-100")
  }
  catch.levels <- fetch.catch.levels(model, catch.levels)
  
  # Extract the catch level names from the list into a vector
  catch.levels.names <- sapply(catch.levels, "[[", 3)
  # Make the catch level values a matrix where the columns represent the cases in catch.names
  catch.levels <- sapply(catch.levels, "[[", 1)
  forecasts.path <- file.path(mcmc.path, "forecasts")
  
  message("Running forecasts for model located in ", mcmc.path, "...\n")
  dir.create(forecasts.path, showWarnings = FALSE)
  
  for(i in 1:length(forecast.yrs)){
    fore.path <- file.path(forecasts.path, paste0("forecast-year-", forecast.yrs[i]))
    dir.create(fore.path, showWarnings = FALSE)
    for(level.ind in 1:ncol(catch.levels)){
      # Create a new sub-directory for each catch projection
      name <- catch.levels.names[level.ind]
      new.forecast.dir <- file.path(fore.path, name)
      dir.create(new.forecast.dir, showWarnings = FALSE)
      
      # Copy all model files into this new forecast directory
      file.copy(file.path(mcmc.path, list.files(mcmc.path)),
                file.path(new.forecast.dir, list.files(mcmc.path)), copy.mode = TRUE)
      
      # Insert fixed catches into forecast file (depending on i)
      forecast.file <- file.path(new.forecast.dir, "forecast.ss")
      fore <- SS_readforecast(forecast.file,
                              Nfleets = 1,
                              Nareas = 1,
                              nseas = 1,
                              verbose = FALSE)
      fore$Ncatch <- length(forecast.yrs[1:i])
      fore$ForeCatch <- data.frame(Year = forecast.yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = catch.levels[,level.ind][1:i])
      
      SS_writeforecast(fore, dir = new.forecast.dir, overwrite = TRUE, verbose = FALSE)
      
      # Evaluate the model using mceval option of ADMB, and retrieve the output
      unlink(file.path(new.forecast.dir, "derived_posteriors.sso"), force = TRUE)
      unlink(file.path(new.forecast.dir, "posteriors.sso"), force = TRUE)
      shell.command <- paste0("cd ", new.forecast.dir, " & ", exe.file.name, " -mceval")
      shell(shell.command)
    }
  }
  message("Finished running forecasts for model located in ", model$path, "...\n")
  invisible()
}

#' Fetch the output from previously-run forecasting using [run.forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem loading the forecasts, return NA
#' 
#' @param mcmc.path Path of the MCMC output
#' @param forecast.yrs A vector of years to forecast
#' @param forecast.probs A vector of quantiles
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
fetch.forecasts <- function(mcmc.path = NA,
                            forecast.yrs = NA,
                            catch.levels = NA,
                            forecast.probs = c(0.05, 0.25, 0.5, 0.75, 0.95)){

  # Extract the catch level names from the list into a vector
  catch.levels.names <- sapply(catch.levels, "[[", 3)
  
  # outputs.list <- vector(mode = "list", length = length(catch.levels))
  outputs.list <- vector(mode = "list", length = length(forecast.yrs))
  for(i in 1:length(forecast.yrs)){
    outputs.list[[i]] <- vector(mode = "list", length = length(catch.levels))
  }
  forecasts.path <- file.path(mcmc.path, "forecasts")
  if(is.na(mcmc.path) | !dir.exists(forecasts.path)){
    return(NA)
  }

  # Get the directory listing and choose the last one for loading
  dir.listing <- dir(forecasts.path)
  
  for(i in 1:length(forecast.yrs)){
    fore.path <- file.path(forecasts.path, paste0("forecast-year-", forecast.yrs[i]))
    # fore.path <- file.path(forecasts.path, dir.listing[length(dir.listing)])
    # Get the directory listing of the last year's forecasts directory and make sure
    #  it matches what the catch levels are.
    dir.listing <- dir(fore.path)
    if(!identical(catch.levels.names, dir.listing)){
      stop("There is a discrepancy between what you have set ",
           "for the catch.levels names \n and what appears in the forecasts directory '",
           fore.path,"'. \n Check the names in both and try again.\n\n", call. = FALSE)
    }
    for(level.ind in 1:length(catch.levels.names)){
      fore.level.path <- file.path(fore.path, catch.levels.names[level.ind])
      mcmc.out <- SSgetMCMC(dir = fore.level.path, writecsv = FALSE)
      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary
      sb <- mcmc.out[,grep("Bratio_",names(mcmc.out))]
      spr <- mcmc.out[,grep("SPRratio_",names(mcmc.out))]
      
      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      names(sb) <- gsub("Bratio_", "",names(sb))
      names(spr) <- gsub("SPRratio_", "",names(spr))
      
      # Now, filter out the projected years only
      sb.proj.cols <- sb[,names(sb) %in% forecast.yrs]
      spr.proj.cols <- spr[,names(spr) %in% forecast.yrs]
      
      outputs.list[[i]][[level.ind]]$biomass <- t(apply(sb.proj.cols, 2, quantile, probs = forecast.probs))
      outputs.list[[i]][[level.ind]]$spr <- t(apply(spr.proj.cols, 2, quantile, probs = forecast.probs))
      outputs.list[[i]][[level.ind]]$mcmccalcs <- calc.mcmc(mcmc.out)
      outputs.list[[i]][[level.ind]]$outputs <- mcmc.out
      names(outputs.list[[i]]) <- catch.levels.names
    }
  }
  names(outputs.list) <- forecast.yrs
  outputs.list
}

#' Calculate the probablities of being under several reference points from one forecast year to the next
#'
#' @param forecast.outputs A list as output by [fetch_forecasts()]
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#' @param forecast.yrs A vector of years to forecast

#' @return A list of length 1 less than the number of forecast years. Each element
#' is a data.frame of catch levels holding the probabilities. For example, list element 1 will hold the
#'  probabilities for each catch.level of being under several reference points for the first two years
#'  in the forecast.yrs vector. If forecast.outputs is NA, NA will be returned, otherwise the risk.list
#'  will be returned
#' @export
calc.risk <- function(forecast.outputs = NA,
                      catch.levels = NA,
                      forecast.yrs = NA){

  stopifnot(!is.na(catch.levels),
            !is.na(forecast.yrs))
  
  # Make the catch level values a matrix where the columns represent the cases in catch.names
  catch.levels <- sapply(catch.levels, "[[", 1)
  
  if(is.na(forecast.outputs)[1]){
    return(NA)
  }

  metric <- function(case.ind, x, yr, yr.ind){
    out <- NULL
    out[1] <- catch.levels[yr.ind, case.ind]
    out[2] <- sum(x[, paste0("SSB_", yr + 1)] < x[, paste0("SSB_", yr)]) / nrow(x) * 100.0
    out[3] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.40) / nrow(x) * 100.0
    out[4] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.25) / nrow(x) * 100.0
    out[5] <- sum(x[, paste0("Bratio_", yr + 1)] < 0.10) / nrow(x) * 100.0
    out[6] <- sum(x[, paste0("SPRratio_", yr)] > 1.00) / nrow(x) * 100.0
    out[7] <- sum(x[, paste0("ForeCatch_", yr + 1)] < out[1]) / nrow(x) * 100.0
    ## DFO values
    out[8] <- sum(x[, paste0("SSB_", yr)] > x[, "SSB_MSY"]) / nrow(x) * 100.0
    out[9] <- sum(x[, paste0("SSB_", yr)] > 0.4 * x[, "SSB_MSY"]) / nrow(x) * 100.0
    out[10] <- sum(x[, paste0("SSB_", yr)] > 0.8 * x[, "SSB_MSY"]) / nrow(x) * 100.0
    names(out) <- c(paste0("ForeCatch_", yr),
                    paste0("SSB_", yr + 1, "<SSB_", yr),
                    paste0("Bratio_", yr + 1, "<0.40"),
                    paste0("Bratio_", yr + 1, "<0.25"),
                    paste0("Bratio_", yr + 1, "<0.10"),
                    paste0("SPRratio_", yr, ">1.00"),
                    paste0("ForeCatch_", yr + 1, "<ForeCatch_", yr),
                    ## DFO values
                    paste0("SSB_", yr, ">SSB_MSY"),
                    paste0("SSB_", yr, ">0.4SSB_MSY"),
                    paste0("SSB_", yr, ">0.8SSB_MSY"))
    
    out
  }
  risk.list <- vector(mode = "list", length = length(forecast.yrs) - 1)
  for(yr in 1:(length(forecast.yrs) - 1)){
    # outputs is a list of one data frame per case, for the current year yr
    outputs <- lapply(forecast.outputs[[yr]], "[[", "outputs")
    # This call calculates the metrics for each element in the list (each catch case)
    #  and binds them together into a data frame. If there was a problem,
    #  (e.g. a bridge model is set up for forecasting) it will be set to NA.
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
  
  risk.list
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast.yrs A vector of forecast years
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#' @param catch.levels.path The path for the catch-levels output
#' @param default.hr.path The path for the default hr output
#' @param ss.executable The name of the SS executable
#'
#' @export
calc_catch_levels_default_hr <- function(model,
                                         forecast.yrs,
                                         catch.levels,
                                         catch.levels.path = "catch-levels",
                                         default.hr.path = "stable-catch",
                                         ss.executable = "ss.exe"){
  
  
  mcmc.path <- model$mcmcpath
  catch.levels.path <- file.path(mcmc.path, catch.levels.path)
  default.hr.path <- file.path(catch.levels.path, default.hr.path)
  dir.create(catch.levels.path, showWarnings = FALSE)
  dir.create(default.hr.path, showWarnings = FALSE)
  
  file.copy(file.path(mcmc.path,
                      list.files(mcmc.path)),
            file.path(default.hr.path,
                      list.files(mcmc.path)),
            copy.mode = TRUE)
  
  forecast.file <- file.path(default.hr.path, "forecast.ss")
  default.hr.catch <- vector(length = length(forecast.yrs), mode = "numeric")
  for(i in 1:length(forecast.yrs)){
    out <- read.table(file.path(default.hr.path,
                                "derived_posteriors.sso"),
                      header = TRUE)
    default.hr.catch[i] <- median(as.numeric(out[paste0("ForeCatch_",
                                                        forecast.yrs[i])][[1]]))
    fore <- SS_readforecast(forecast.file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast.yrs[1:i])
    fore$ForeCatch <- data.frame(Year = forecast.yrs[1:i],
                                 Seas = 1,
                                 Fleet = 1,
                                 Catch_or_F = default.hr.catch[1:i])
    SS_writeforecast(fore,
                     dir = default.hr.path,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(default.hr.path, "derived_posteriors.sso"),
           force = TRUE)
    shell.command <- paste0("cd ", default.hr.path, " & ", ss.executable, " -mceval")
    shell(shell.command)
  }
}



#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast.yrs A vector of forecast years
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#' @param tol The tolerance in the spr away from 1.
#' @param catch.tol The iterations will stop if the difference between the catch and zero is less than this
#' @param max.iter The maximum number of iterations to do. If this is reached, then no stable catch could 
#' be found within the tolerance
#' @param spr.100.path The path for the spr-100 output
#' @param catch.levels.path The path for the catch-levels output
#' @param ss.executable The name of the SS executable
#'
#' @export
calc_catch_levels_spr_100 <- function(model,
                                      forecast.yrs,
                                      catch.levels,
                                      tol = 0.0001,
                                      catch.tol = 100,
                                      max.iter = 20,
                                      catch.levels.path = "catch-levels",
                                      spr.100.path = "spr-100",
                                      ss.executable = "ss.exe"){
  
  mcmc.path <- model$mcmcpath
  catch.levels.path <- file.path(mcmc.path, catch.levels.path)
  spr.100.path <- file.path(catch.levels.path, spr.100.path)
  dir.create(catch.levels.path, showWarnings = FALSE)
  dir.create(spr.100.path, showWarnings = FALSE)
  
  file.copy(file.path(mcmc.path,
                      list.files(mcmc.path)),
            file.path(spr.100.path,
                      list.files(mcmc.path)),
            copy.mode = TRUE)
  
  forecast.file <- file.path(spr.100.path, "forecast.ss")
  spr.100.catch <- vector(length = length(forecast.yrs), mode = "numeric")
  for(i in 1:length(forecast.yrs)){
    fore <- SS_readforecast(forecast.file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast.yrs[1:i])
    upper <- spr.100.catch[i] <- median(model$mcmc[paste0("ForeCatch_",
                                                          forecast.yrs[i])][[1]])
    lower <- 0
    iter <- 1
    repeat{
      fore$ForeCatch <- data.frame(Year = forecast.yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = spr.100.catch[1:i])
      SS_writeforecast(fore,
                       dir = spr.100.path,
                       overwrite = TRUE,
                       verbose = FALSE)
      unlink(file.path(spr.100.path, "derived_posteriors.sso"),
             force = TRUE)
      shell.command <- paste0("cd ", spr.100.path, " & ", ss.executable, " -mceval")
      shell(shell.command)
      out <- read.table(file.path(spr.100.path,
                                  "derived_posteriors.sso"),
                        header = TRUE)
      spr <- median(as.numeric(out[paste0("SPRratio_", forecast.yrs[i])][[1]]))
      if(abs(spr - 1) < tol |
         abs(upper - lower) < catch.tol){
        ## Sometimes, upper and lower can end up close to equal,
        ##  but the tolerance is still not met. In this case, assume
        ##  the catch creates an SPR of 100% even though it is slightly off.
        break
      }
      if(iter == max.iter){
        warning("The maximum number of iterations (", max.iter,") was reached for forecast year ", forecast.yrs[i],
                ". The SPR difference in the last iteration was ", spr - 1)
        break
      }
      
      if(spr - 1 > 0){
        upper <- spr.100.catch[i]
        spr.100.catch[i] <- (lower + spr.100.catch[i]) / 2.0
      }else{
        lower <- spr.100.catch[i]
        spr.100.catch[i] <- (upper + spr.100.catch[i]) / 2.0
      }
      iter <- iter + 1
    }
  }
}

#' Run the model iteratively reducing the difference between the first and second year projections to
#' find a stable catch within the the given tolerance
#'
#' @param model The SS model output as loaded by [load.ss.files()]
#' @param forecast.yrs A vector of forecast years
#' @param catch.levels The catch levels list as defined in forecast-catch-levels.R
#' @param stable.catch.tol The tolerance in tonnes. The iterationis will stop if the difference between the
#' projected biomass between the first and second years is less than this
#' @param max.iter The maximum number of iterations to do. If this is reached, then no stable catch could 
#' be found within the tolerance
#' @param catch.levels.path The path for the catch-levels output
#' @param stable.catch.path The path for the stable-catch output
#' @param ss.executable The name of the SS executable
#'
#' @export
calc_catch_levels_stable_catch <- function(model,
                                           forecast.yrs,
                                           catch.levels,
                                           stable.catch.tol = 100,
                                           max.iter = 20,
                                           catch.levels.path = "catch-levels",
                                           stable.catch.path = "stable-catch",
                                           ss.executable = "ss.exe"){
  mcmc.path <- model$mcmcpath
  catch.levels.path <- file.path(mcmc.path, catch.levels.path)
  dir.create(catch.levels.path, showWarnings = FALSE)
  stable.catch.path <- file.path(catch.levels.path, stable.catch.path)
  dir.create(stable.catch.path, showWarnings = FALSE)
  
  file.copy(file.path(mcmc.path,
                      list.files(mcmc.path)),
            file.path(stable.catch.path,
                      list.files(mcmc.path)),
            copy.mode = TRUE)
  
  forecast.file <- file.path(stable.catch.path, "forecast.ss")
  stable.catch <- vector(length = length(forecast.yrs), mode = "numeric")
  out <- read.table(file.path(stable.catch.path,
                              "derived_posteriors.sso"),
                    header = TRUE)
  iter <- 1
  repeat{
    out <- read.table(file.path(stable.catch.path,
                                "derived_posteriors.sso"),
                      header = TRUE)
    stable.catch[1] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast.yrs[1])][[1]]))
    stable.catch[2] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast.yrs[2])][[1]]))
    stable.catch[3] <- median(as.numeric(out[paste0("ForeCatch_",
                                                    forecast.yrs[3])][[1]]))
    if(abs(stable.catch[1] - stable.catch[2]) < stable.catch.tol){
      break
    }
    if(iter == max.iter){
      warning("The maximum number of iterations (", max.iter,") was reached. The catch difference in the last iteration was ",
              abs(stable.catch[1] - stable.catch[2]))
      break
    }
    fore <- SS_readforecast(forecast.file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast.yrs[1])
    fore$ForeCatch <- data.frame(Year = forecast.yrs[1],
                                 Seas = 1,
                                 Fleet = 1,
                                 Catch_or_F = (stable.catch[1] + stable.catch[2]) / 2)
    SS_writeforecast(fore,
                     dir = stable.catch.path,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(file.path(stable.catch.path, "derived_posteriors.sso"),
           force = TRUE)
    shell.command <- paste0("cd ", stable.catch.path, " & ", ss.executable, " -mceval")
    shell(shell.command)
    iter <- iter + 1
  }
  fore <- SS_readforecast(forecast.file,
                          Nfleets = 1,
                          Nareas = 1,
                          nseas = 1,
                          verbose = FALSE)
  fore$Ncatch <- length(forecast.yrs[1:length(forecast.yrs)])
  fore$ForeCatch <- data.frame(Year = forecast.yrs[1:length(forecast.yrs)],
                               Seas = 1,
                               Fleet = 1,
                               Catch_or_F = stable.catch[1:length(forecast.yrs)])
  SS_writeforecast(fore,
                   dir = stable.catch.path,
                   overwrite = TRUE,
                   verbose = FALSE)
  unlink(file.path(stable.catch.path, "derived_posteriors.sso"),
         force = TRUE)
  shell.command <- paste0("cd ", stable.catch.path, " & ", ss.executable, " -mceval")
  shell(shell.command)
}

calc.catch.levels <- function(model, 
                              ovr.stable = FALSE,
                              ovr.spr = FALSE,
                              ovr.hr = FALSE,
                              ...){
  
  calc_catch_levels_default_hr(model, ...)
  calc_catch_levels_spr_100(model, ...)
  calc_catch_levels_stable_catch(model, ...)
  
}
