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
  
  ## Objects to store total biomass and age 2+ biomass (summary biomass)
  Bio_all <- NULL
  Bio_smry <- NULL
  
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
    Bio_smry <- cbind(Bio_smry, ts$Bio_smry)
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
  extra.mcmc$timeseries$Bio_smry <- apply(Bio_smry, MARGIN = 1, FUN = median)
  extra.mcmc$timeseries$Bio_smry.0.025 <- apply(Bio_smry, MARGIN = 1,
                                                FUN = quantile, probs = 0.025)
  extra.mcmc$timeseries$Bio_smry.0.975 <- apply(Bio_smry, MARGIN = 1,
                                                FUN = quantile, probs = 0.975)
  
  message(curr.func.name, paste("Completed read of extra MCMC output."))
  
  extra.mcmc
}
