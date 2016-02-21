## Pacific Hake Joint Technical Committee, January-February 2016
## all.r - Source this file to load all data and functions,
##  to run the forecasting and retrospectives for the base model,
##  then save the R environment to .RData in this directory. This
##  will be read in by knitr as a binary file so that multiple
##  loads don't happen during the latex/knitr build (they are
##  very slow compared to loading the binary once at the beginning).

remove.all.except <- function(vars  = c("models")){
  ## Removes every object in the workspace except for what is in the vars list.
  ## Upon finishing, the workspace will contain whatever is in the vars list,
  ##  plus the objects 'remove.all.except' (this function) and 'models.loaded'.
  ## That tells the software that the model has already been loaded.
  vars <- c(vars, "remove.all.except")
  keep <- match(x = vars, table = ls(all = TRUE, envir = .GlobalEnv))
  if(any(is.na(keep))){
    models.loaded <<- FALSE
  }else{
    rm(list=ls(all = TRUE, envir = .GlobalEnv)[-keep], envir = .GlobalEnv)
    models.loaded <<- TRUE
  }
}
remove.all.except()

## Need to source utilities.r before everything because it contains the function
##  install.packages.if.needed
source("utilities.r")
install.packages.if.needed("devtools", "devtools", github=FALSE)
install.packages.if.needed("nwfscSurvey", "nwfsc-assess/nwfscSurvey", github=TRUE)
install.packages.if.needed("nwfscMapping", "nwfsc-assess/nwfscMapping", github=TRUE)
install.packages.if.needed("date", "date", github=FALSE)
install.packages.if.needed("r4ss", "r4ss/r4ss", github=TRUE)
install.packages.if.needed("xtable", "xtable", github=FALSE)
install.packages.if.needed("PBSmapping", "PBSmapping", github=FALSE)
install.packages.if.needed("PBSmodelling", "PBSmodelling", github=FALSE)
install.packages.if.needed("maps", "maps", github=FALSE)
install.packages.if.needed("coda", "coda", github=FALSE)
install.packages.if.needed("dplyr", "dplyr", github = FALSE)
install.packages.if.needed("maptools", "maptools", github = FALSE)
install.packages.if.needed("gtools", "gtools", github = FALSE)
install.packages.if.needed("lubridate", "lubridate", github = FALSE)

require(nwfscSurvey)
require(nwfscMapping)
require(date)
require(r4ss)
require(xtable)
require(PBSmapping)
require(PBSmodelling)
require(maps)
require(dplyr)
require(coda)
require(gtools)
require(maptools)
require(lubridate)

source("catches.r")      ## Contains the code to catch/TAC data and figure and table-making code for catch/TAC
source("load-models.r")  ## Contains the code to load the models list from the model directories
source("survey.r")       ## Contains the table-making code for survey
source("load-data.r")    ## Contains the code to load data tables, including survey data table

source("figures-timeseries.r")
source("figures-compare-forecasts.r")
source("figures-mcmc-diagnostics.r")
source("figures-age-comps.r")
source("figures-selex.r")
source("figures-stock-recruitment.r")
source("figures-mle-mcmc.r")
source("figures-overview-map.r")
source("figures-data.r")
source("figures-assessment-history.r")

source("tables-timeseries.r")
source("tables-reference-points.r")
source("tables-decisions.r")
source("tables-age.r")
source("tables-parameters.r")
source("tables-sampling.r")
source("tables-maturity.r")

## verbose applies to the SS loading functions as well as this project's functions and the system call
verbose <- TRUE

data.path <- file.path("..","..","data")
models.path <- file.path("..","..","models")

can.age.file <- "can-age-data.csv"
catch.data.file <- "landings-tac-history.csv"
further.tac.file <- "further-tac-details.csv"
survey.history.file <- "survey-history.csv"
survey.comparison.file <- "survey-comparison.csv"
sampling.history.file <- "fishery-sampling-history.csv"
ovary.samples.file <- "ovary-samples.csv"
age.1.file <- "age-1.csv"
assessment.history.file <- "assessment-history.csv"
## The following are used for cumulative catch plot in the data/fisheries presentation, not the assessment document
us.shore.catch.by.month.file <- "us-shore-catch-by-month.csv"
us.cp.catch.by.month.file <- "us-cp-catch-by-month.csv"
us.ms.catch.by.month.file <- "us-ms-catch-by-month.csv"
us.research.catch.by.month.file <- "us-research-catch-by-month.csv"
can.shore.catch.by.month.file <- "can-shore-catch-by-month.csv"
can.ft.catch.by.month.file <- "can-ft-catch-by-month.csv"
## The following are used for the age comp-by fleet plot in the data/fisheries presentation, not the assessment document
us.shore.age.data.file <- "us-shore-age-data.csv"
us.cp.age.data.file <- "us-cp-age-data.csv"
us.ms.age.data.file <- "us-ms-age-data.csv"

exe.file.name <- "ss3.exe"
starter.file.name <- "starter.ss"
forecast.file.name <- "forecast.ss"
weight.at.age.file.name <- "wtatage.ss"

SSversion <- "3.24U"

## These will be used to generate the keyposteriors.csv file,
##  the remaining ones will be put into nuisanceposteriors.csv.
key.posteriors <- c("NatM_p_1_Fem_GP_1",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD_2_Acoustic_Survey")
key.posteriors.file <- "keyposteriors.csv"
nuisance.posteriors.file <- "nuisanceposteriors.csv"

################################################################################
## Key values that pertain to the base model
################################################################################

## IMPORTANT - If any of these do not match up with what the models are set up
##  for, the build will fail. The only exception is that end.yr must actually
##  be the end year of the model + 1.

## recruitment deviations start year
recruit.dev.start.yr <- 1946
## Unfished equilibrium year.
unfished.eq.yr  <- 1964
## Start year for the models
start.yr        <- 1966
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr          <- 2016
## First year in the survey timeseries
survey.start.yr <- 1998
## Last year in the survey timeseries
survey.end.yr   <- 2015
## The last year an assessment was done
last.assess.yr  <- end.yr - 1
## current assessment year
assess.yr       <- end.yr
## Final year of data (This is what is the end year is in the model data files)
last.data.yr    <- end.yr - 1

## The forecasting yrs and probs can be set to whatever is required, the
## latex/knitr code is set up to automatically accomodate changes
forecast.yrs <- end.yr:(end.yr + 2)
forecast.probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

################################################################################
## Base model, this year and last
################################################################################

## Set up models lists - NOTE all are *required* to build the document.
models.dir.list <- dir(models.path)

base.model.name <- "39_preSRGbase_updated"
## Last year's base model. This is used for the parameter estimates table which compares
##  last year's to this year's parameter estimates.
last.year.base.model.name <- "00_2015hake_basePreSRG"

## Indicies models as found in the directory.
base.model.ind <- grep(base.model.name, models.dir.list)
if(length(base.model.ind) == 0){
  stop("Base model '", base.model.name, "' not found. Check the name and try again.\n")
}
if(verbose){
  cat("\nDEBUG: Loading model ", base.model.name, " as base model\n\n")
}
last.year.base.model.ind <- grep(last.year.base.model.name, models.dir.list)
if(length(last.year.base.model.ind) == 0){
  stop("Last year's base model '", last.year.base.model.name, "' not found. Check the name and try again.\n")
}

################################################################################
## Bridge models
################################################################################
if(verbose){
  cat("\nDEBUG: Loading bridge models \n\n")
}

bridge.model.dir.names.1 <- c(last.year.base.model.name,
                              "03_UpdatePre2015WtAge")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.1 <- c(paste0(last.assess.yr, " Base model"),
                          "Update data")
bridge.model.dir.names.2 <- c(last.year.base.model.name,
                              "40_Add2015Survey_withExtrap_update",
                              "41_Add2015Catch_FishAcomps_withExtrap_update")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.2 <- c(paste0(last.assess.yr, " Base model"),
                          paste0("Add ", survey.end.yr, " survey series"),
                          paste0("Add ", last.data.yr, " fishery data"))
bridge.model.dir.names.3 <- c("41_Add2015Catch_FishAcomps_withExtrap_update",
                              "41.01_adjustBiasRamping_update",
                              "41.02_ChangeSurveyTuning_update",
                              "41.03_ChangeAllTuning_update")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.3 <- c("Base model pretune",
                          "Adjust bias ramp",
                          "Change survey comp weights",
                          "Change all comp weights")

## Bridge model indices are used to tell knitr which elements of the models list are to
## be plotted together.
inds <- 1:length(models.dir.list)
names(inds) <- models.dir.list
bridge.model.inds.1 <- inds[bridge.model.dir.names.1]
bridge.model.inds.2 <- inds[bridge.model.dir.names.2]
bridge.model.inds.3 <- inds[bridge.model.dir.names.3]

if((length(bridge.model.inds.1) != length(bridge.model.dir.names.1)) |
   (length(bridge.model.inds.2) != length(bridge.model.dir.names.2)) |
   (length(bridge.model.inds.3) != length(bridge.model.dir.names.3))){
  stop("One or more of the bridge model directory names were not found. Check the names and try again. Directory names listed in all.r are:\n",
       paste0(bridge.model.dir.names.1, "\n"),
       "\n",
       paste0(bridge.model.dir.names.2, "\n"),
       "\n",
       paste0(bridge.model.dir.names.3, "\n"))
}
if((length(bridge.model.names.1) != length(bridge.model.dir.names.1)) |
   (length(bridge.model.names.2) != length(bridge.model.dir.names.2)) |
   (length(bridge.model.names.3) != length(bridge.model.dir.names.3))){
  stop("One of the bridge.model.names vectors in all.r has a different length than its bridge.model.dir.names counterpart. Make sure these two vectors match in length and try again.\n")
}

################################################################################
## Sensitivity models
################################################################################
if(verbose){
  cat("\nDEBUG: Loading sensitivity models \n\n")
}
sens.model.dir.names.1 <- c("46_Sensbase_sigmaR_1.0_update",
                            "47_Sensbase_sigmaR_2.0_update",
                            "48_Sensbase_h_0.5prior_update",
                            "49_Sensbase_h_1.0fix_update",
                            "51_Sensbase_M_SD0.2_update",
                            "52_Sensbase_M_SD0.3_update")
## Sens model names will be used to make the sensitivity model plot and its caption.
## Make sure they are the same length as sens.model.dir.names
sens.model.names.1 <- c("Sigma R 1.0",
                        "Sigma R 2.0",
                        "Steepness prior mean 0.5",
                        "Steepness fixed mean 1.0",
                        "Natural mortality SD 0.2",
                        "Natural mortality SD 0.3")

sens.model.dir.names.2 <- c("42_Sensbase_Survey_noExtrap_update",
                            "50_Sensbase_Age1Index_update")
sens.model.names.2 <- c("No extrapolation on survey",
                        "Include age-1 index")
sens.model.dir.names.3 <- c("43_Sensbase_Selmaxage5_update",
                            "44_Sensbase_Selmaxage7_update",
                            "45_Sensbase_Selmaxage12_update")
sens.model.names.3 <- c("Max. age of selectivity 5",
                        "Max. age of selectivity 7",
                        "Max. age of selectivity 12")

sens.model.dir.names.4 <- "53_Sensbase_AgeError_noCohort_update"
sens.model.names.4 <- "No cohort ageing error"

## Sensitivity model indices are used to tell knitr which elements of the models list are to
## be plotted together.
sens.model.inds.1 <- grep(paste(sens.model.dir.names.1, collapse = "|"), models.dir.list)
sens.model.inds.2 <- grep(paste(sens.model.dir.names.2, collapse = "|"), models.dir.list)
sens.model.inds.3 <- grep(paste(sens.model.dir.names.3, collapse = "|"), models.dir.list)
sens.model.inds.4 <- grep(paste(sens.model.dir.names.4, collapse = "|"), models.dir.list)
if((length(sens.model.inds.1) != length(sens.model.dir.names.1)) |
   (length(sens.model.inds.2) != length(sens.model.dir.names.2)) |
   (length(sens.model.inds.3) != length(sens.model.dir.names.3)) |
   (length(sens.model.inds.4) != length(sens.model.dir.names.4))){
  stop("One or more of the sensitivity model directory names were not found. Check the names and try again. Directory names listed in all.r are:\n",
       paste0(sens.model.dir.names.1, "\n"),
       "\n",
       paste0(sens.model.dir.names.2, "\n"),
       "\n",
       paste0(sens.model.dir.names.3, "\n"),
       "\n",
       paste0(sens.model.dir.names.4, "\n"))
}
if((length(sens.model.names.1) != length(sens.model.dir.names.1)) |
   (length(sens.model.names.2) != length(sens.model.dir.names.2)) |
   (length(sens.model.names.3) != length(sens.model.dir.names.3)) |
   (length(sens.model.names.4) != length(sens.model.dir.names.4))){
  stop("One of the sens.model.names vectors in all.r has a different length than its sens.model.dir.names counterpart. Make sure these two vectors match in length and try again.\n")
}

################################################################################
## Forecasting
################################################################################
if(verbose){
  cat("\nDEBUG: Reading forecast values \n\n")
}

## catch.levels is a list of N catch levels to run forecasts for
## Each element of the list is a vector of length the same as the
## number of elements in forcast.yrs
catch.levels <- list(rep(0.01, 3),
                     rep(180000,3),
                     rep(350000,3),
                     rep(440000,3),
                     c(760000,855000,750000),
                     c(804399,889918,785036),
                     c(873000,873000,773907))

## Index for the forecasts list, which one above is the TAC case?
## This is used in the one-page summary and the plot comparing several catch cases
catch.tac.ind <- 4
## The catch as calculated using the default harvest policy.
catch.default.policy.ind <- 6
catch.default.policy <- catch.levels[[catch.default.policy.ind]]

## catch.levels.names is a list of N names for the catch levels given in catch.levels
##  to be used in plots (Pretty names)
catch.levels.names <- c("No Fishing",
                        "180,000 t",
                        "350,000 t",
                        paste0(last.data.yr, " TAC: 440,000 t"),
                        "SPR100",
                        paste0("Default: ",fmt0(catch.default.policy[1])," t"),
                        "stableCatch")

## catch.levels.dir.names is a list of N names for the catch levels given in catch.levels,
##  to be used as the directory names (OS-naming friendly). Use prefixed numbers so that
##  the list order is the same as the directory order.
catch.levels.dir.names <- c("01_0",
                            "02_180000",
                            "03_350000",
                            "04_440000",
                            "05_SPR100",
                            "06_DefaultHR",
                            "07_stableCatch")

################################################################################
## Model loading questions
################################################################################

reload.models <- readline(prompt = "Reload models (only necessary first time or if you add new models to the models directory)? [y/n] ")
if(reload.models == "y" | reload.models == "Y"){
  smart.load <- FALSE
  if(exists("models")){ ## Only ask if the models list exists
    smart.load <- readline(prompt = "   Use smart load (will only reload newly-added models, thus keeping any forecasting done previously)? [y/n] ")
    if(smart.load != "y" & smart.load != "Y"){
      sure <- readline(prompt = "      Are you sure (your entire models list will be deleted and re-populated)? [y/n] ")
      if(sure != "y" & sure != "Y"){
        stop("I'm stopping because you aren't sure. Re-source to try again.\n\n")
      }
      smart.load <- TRUE
    }
  }
}
run.forecasts <- readline(prompt = "Run forecasting for base model (only necessary after fully reloading or if you changed the base model [takes 10 minutes])? [y/n] ")
run.partest <- readline(prompt = "Run partest for base model (only necessary if you changed the base model [takes 15 minutes])? [y/n] ")
run.retros <- readline(prompt = "Run retrospectives for base model? [y/n] ")
retro.yrs <- 1:15

################################################################################
## Data table loading
################################################################################

cat("\nLoading all data tables (csv files) from ", data.path,"\n")
catches <- load.catches(file.path(data.path, catch.data.file))
landings.vs.tac <- catches[[2]]
catches <- catches[[1]]
survey.history <- load.survey.history(file.path(data.path, survey.history.file))
survey.comparison <- read.csv(file.path(data.path, survey.comparison.file), stringsAsFactors = FALSE)
sampling.history <- load.sampling.history(file.path(data.path, sampling.history.file))
further.tac <- further.tac.details(file.path(data.path, further.tac.file))
can.ages <- load.can.age.data(file.path(data.path, can.age.file))
ovary.samples <- read.csv(file.path(data.path, ovary.samples.file), stringsAsFactors = FALSE)
age.1.index <- read.csv(file.path(data.path, age.1.file), stringsAsFactors = FALSE)
assessment.history <- read.csv(file.path(data.path, assessment.history.file), stringsAsFactors = FALSE)
## For cumulative catch plots in the data presentation
us.shore.catch.by.month <- read.csv(file.path(data.path, us.shore.catch.by.month.file), stringsAsFactors = FALSE)
us.cp.catch.by.month <- read.csv(file.path(data.path, us.cp.catch.by.month.file), stringsAsFactors = FALSE)
us.ms.catch.by.month <- read.csv(file.path(data.path, us.ms.catch.by.month.file), stringsAsFactors = FALSE)
us.research.catch.by.month <- read.csv(file.path(data.path, us.research.catch.by.month.file), stringsAsFactors = FALSE)
can.shore.catch.by.month <- read.csv(file.path(data.path, can.shore.catch.by.month.file), stringsAsFactors = FALSE)
can.ft.catch.by.month <- read.csv(file.path(data.path, can.ft.catch.by.month.file), stringsAsFactors = FALSE)
## For age comps-by fleet plots in the data presentation
can.shore.age <- can.ages[[1]]
can.ft.age <- can.ages[[2]]
us.shore.age <- load.us.age.data(file.path(data.path, us.shore.age.data.file))
us.cp.age <- load.us.age.data(file.path(data.path, us.cp.age.data.file))
us.ms.age <- load.us.age.data(file.path(data.path, us.ms.age.data.file))
wt.at.age <- load.wt.at.age(models[[base.model.ind]], weight.at.age.file.name)
cat("All data tables have been loaded ", data.path,"\n")

################################################################################
## Model loading
################################################################################

if(reload.models == "y" | reload.models == "Y"){
  cat("\n\nLoading models...\n\n")
  if(!exists("models")){
    models <- NULL
  }
  models <- load.models(models.path, yr = end.yr, smart.load = smart.load, model.list = models)
  cat("\n\nAll models have been loaded.\n\n")
}else{
  cat("\n\nModels have NOT been re-loaded.\n\n")
}

if(!exists("models")){
  stop("Cannot continue... Models must be loaded first.\n\n")
}

################################################################################
## Forecast model runs
################################################################################

if(run.forecasts == "y" | run.forecasts == "Y"){
  cat("\n\nRunning forecasts for model located in ",models[[base.model.ind]]$path,"...\n\n")
  forecasts.path <- file.path(models[[base.model.ind]]$path, "mcmc", "forecasts")

  forecasts <- calc.forecast(models[[base.model.ind]]$mcmc,
                             models[[base.model.ind]]$path,
                             forecast.yrs,
                             catch.levels,
                             catch.levels.dir.names,
                             probs = forecast.probs)

  models[[base.model.ind]]$forecasts$biomass <- forecasts[[1]]
  models[[base.model.ind]]$forecasts$spr <- forecasts[[2]]
  models[[base.model.ind]]$forecasts$mcmccalcs <- forecasts[[3]]
  models[[base.model.ind]]$forecasts$outputs <- forecasts[[4]]

  metrics <- create.metrics(models[[base.model.ind]]$mcmc,
                            models[[base.model.ind]]$path,
                            forecast.yrs[-length(forecast.yrs)],
                            catch.levels,
                            catch.levels.dir.names)

  models[[base.model.ind]]$metrics$outputs <- metrics

  if(verbose){
    cat("\nDEBUG: Calculated forecasts and metrics\n\n")
  }

  ## calc.risk assumes the forecasting step was done correctly
  risks <- calc.risk(models[[base.model.ind]]$metrics$outputs,
                     forecast.yrs,
                     catch.levels,
                     catch.levels.dir.names)

  models[[base.model.ind]]$risks <- risks

  if(verbose){
    cat("\nDEBUG: Calculated risks\n\n")
  }

  cat("\n\nForecast calculations completed.\n\n")
}

################################################################################
## Get Report and CompReport files for each posterior sample
################################################################################

if(run.partest == "y" | run.partest == "Y"){
  if(verbose){
    cat("\nDEBUG: Running partest\n\n")
  }
  run.partest.model(models[[base.model.ind]], output.file = "model-partest.RData", verbose = verbose)
  if(verbose){
    cat("\nDEBUG: Partest Completed\n\n")
  }
}

################################################################################
## Retrospective model runs
################################################################################

if(run.retros == "y" | run.retros == "Y"){
  models[[base.model.ind]]$retros <- run.retrospectives(models[[base.model.ind]], yrs = retro.yrs, verbose = verbose)
}

## Number of retro years for the plot and table. Assumes you've run them.
plot.retro.yrs <- 1:5
retro.model.names <- c("Base model", sapply(plot.retro.yrs, function(x) paste0("-", x, if(x == 1) " year" else " years")))
## Need to re-assemble the list with the base as the first element
retro.list <- list(models[[base.model.ind]])
for(i in plot.retro.yrs){
  retro.list[[i + 1]] <- models[[base.model.ind]]$retros[[i]]
}

################################################################################
## Set up lists to use for sensitivity plots and tables
################################################################################
if(verbose){
  cat("\nDEBUG: Setting up lists for sensitivities \n\n")
}

## A vector of all sensitivities for the MLE parameters, derived quantiles, and reference points table
sens.model.inds.1.for.table <- sens.model.inds.1
sens.model.names.1.for.table <- c("Base model", sens.model.names.1)
sens.models.1.for.table <- list(models[[base.model.ind]])
i <- 1
for(sens.model in sens.model.inds.1.for.table){
  sens.models.1.for.table[[i + 1]] <- models[[sens.model]]
  i <- i + 1
}
sens.model.inds.2.for.table <- c(sens.model.inds.2, sens.model.inds.3)
sens.model.names.2.for.table <- c("Base model", sens.model.names.2, sens.model.names.3)
sens.models.2.for.table <- list(models[[base.model.ind]])
i <- 1
for(sens.model in sens.model.inds.2.for.table){
  sens.models.2.for.table[[i + 1]] <- models[[sens.model]]
  i <- i + 1
}
## sens.models.1.for.table now contains the base case and sensitivity group 1 models
## sens.model.names.1.for.table now contains "Base model" sensitivity group 1 models
## sens.models.2.for.table now contains the base case and sensitivity groups 2 and 3 models
## sens.model.names.2.for.table now contains "Base model" sensitivity groups 2 and 3 models

################################################################################
## Variables to be used in the knitr code chunks
################################################################################
if(verbose){
  cat("\nDEBUG: formatting variables\n")
}

if(verbose){
  cat("DEBUG: Attainment\n\n")
}

## A simpler variable for the base model
base.model <- models[[base.model.ind]]
cat("Base model is ", base.model$path, "\n\n")

## Allotments by country
can.allotment.percent <- 26.12
us.allotment.percent <- 73.88

## Attainment, used in the management performance section
usa.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),8]), 1)
can.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),9]), 1)
tot.last.5.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-5):(end.yr-1),10]), 1)
tot.last.10.years.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-10):(end.yr-1),10]), 1)
tot.last.year.attainment <- fmt0(mean(landings.vs.tac[landings.vs.tac$Year == (end.yr-1),"ATTAIN"]), 1)

if(verbose){
  cat("DEBUG: Catches\n\n")
}
## Recent catches
last.5.years.of.catch.data <- (max(catches$Year)-4):max(catches$Year)
last.5.years.total.catch <- catches[catches$Year %in% last.5.years.of.catch.data, "TOTAL"]
long.term.avge.catch <- mean(catches$TOTAL)
last.5.years.above.avge <- last.5.years.of.catch.data[last.5.years.total.catch > long.term.avge.catch]
last.5.years.below.avge <- last.5.years.of.catch.data[last.5.years.total.catch < long.term.avge.catch]

if(verbose){
  cat("DEBUG: Last year's values\n\n")
}
## last year's values (mostly for the one-page-summary and introduction)
last.year.landings <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TOTAL), 0)
last.year.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TAC)
last.year.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$ATTAIN), 1)

## US landings, TAC, and attainments
last.year.us.landings <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$Ustotal))
last.year.us.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained <- fmt0(as.numeric(100 - landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$USATTAIN), 1)
last.year.us.not.attained.tonnes <- filter(landings.vs.tac, Year == last.data.yr)$TACUSA - filter(landings.vs.tac, Year == last.data.yr)$Ustotal
last.year.us.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACUS)
## Not doing fmt0 here since want to do further calculations
last.year.us.tribal <- filter(further.tac, Year == last.data.yr)$us.tribal.quota
last.year.us.research <- filter(further.tac, Year == last.data.yr)$us.research.quota
last.year.us.non.tribal <- filter(further.tac, Year == last.data.yr)$us.nontribal.quota
last.year.us.tribal.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.tribal.quota.reallocated
last.year.us.tribal.reallocate.dates <- filter(further.tac, Year == last.data.yr)$us.tribal.reallocate.dates
last.year.us.tribal.max.landed <- filter(further.tac, Year == last.data.yr)$us.tribal.max.landed
last.year.us.shore.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.shore.reallocated
last.year.us.cp.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.cp.reallocated
last.year.us.ms.quota.reallocated <- filter(further.tac, Year == last.data.yr)$us.ms.reallocated
## Last year US catches by fleet
last.year.us.research.catch <- filter(catches, Year == last.data.yr)$USresearch
last.year.us.cp.catch <- filter(catches, Year == last.data.yr)$atSea_US_CP
last.year.us.ms.catch <- filter(catches, Year == last.data.yr)$atSea_US_MS
last.year.us.shore.catch <- filter(catches, Year == last.data.yr)$US_shore
## Last year US percent of TAC caught by fleet
last.year.us.research.catch.percent <- fmt0(last.year.us.research.catch / last.year.us.research * 100, 1)
last.year.us.cp.catch.percent <- fmt0(last.year.us.cp.catch / last.year.us.cp.quota.reallocated * 100, 1)
last.year.us.ms.catch.percent <- fmt0(last.year.us.ms.catch / last.year.us.ms.quota.reallocated * 100, 1)
last.year.us.shore.catch.percent <- fmt0(last.year.us.shore.catch / last.year.us.shore.quota.reallocated * 100, 1)
last.year.us.tribal.catch.percent <- fmt0(last.year.us.tribal.max.landed / last.year.us.tribal.quota.reallocated * 100, 1)

## Last year Canadian catch and tac
last.year.can.carryover <- fmt0(filter(further.tac, Year == last.data.yr)$can.carried.over)
last.year.can.attained <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANATTAIN), 1)   # the percentage
last.year.can.landings <- fmt0(as.numeric(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$CANtotal))
last.year.can.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN)
last.year.can.tac.jv <- fmt0(filter(further.tac, Year == last.data.yr)$can.jv.tac)
last.year.can.shoreside.tac <- fmt0(landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN - filter(further.tac, Year == last.data.yr)$can.jv.tac)

latest.year.can.jv <- max(filter(catches, CAN_JV > 0)$Year)  # latest year of JV in Canada
last.year.can.shore <- fmt0(filter(catches, Year == last.data.yr)$CAN_Shoreside)
last.year.can.freezer <- fmt0(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl)
last.year.can.jv <- fmt0(filter(catches, Year == last.data.yr)$CAN_JV)
last.year.can.shore.percent <- fmt0(filter(catches, Year == last.data.yr)$CAN_Shoreside /
                                       (landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN - filter(further.tac, Year == last.data.yr)$can.jv.tac)
                                        * 100.0, 1)
last.year.can.freezer.percent <- fmt0(filter(catches, Year == last.data.yr)$CAN_FreezeTrawl /
                                       (landings.vs.tac[landings.vs.tac$Year %in% (end.yr-1),]$TACCAN - filter(further.tac, Year == last.data.yr)$can.jv.tac)
                                        * 100.0, 1)
last.year.can.jv.percent <- fmt0(filter(catches, Year == last.data.yr)$CAN_JV /
                                                                      filter(further.tac, Year == last.data.yr)$can.jv.tac * 100.0, 1)
years.Can.JV.catch.eq.0.recent = years.Can.JV.catch.eq.0(catches)

## Survey values
survey.biomass <- survey.history$biomass
names(survey.biomass) <- as.character(survey.history$year)
survey.comps <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy==2,]
rownames(survey.comps) <- survey.comps$Yr
## Survey extrapolation percentages and years
survey.extrap.percent <- 100 * (survey.comparison$with.extrap - survey.comparison$no.extrap) / survey.comparison$with.extrap
names(survey.extrap.percent) <- as.character(survey.comparison$year)
survey.extrap.percent <- survey.extrap.percent[!is.na(survey.extrap.percent)]

survey.largest.extrap.percent <- fmt0(max(survey.extrap.percent), 2)
survey.year.of.largest.extrap <- names(survey.extrap.percent[survey.extrap.percent == max(survey.extrap.percent)])

survey.smallest.extrap.percent <- fmt0(min(survey.extrap.percent), 2)
survey.year.of.smallest.extrap <- names(survey.extrap.percent[survey.extrap.percent == min(survey.extrap.percent)])

survey.average.extrap.percent <- fmt0(mean(survey.extrap.percent), 2)



## New depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: New depletion and spawning biomass estimates\n\n")
}
curr.depl.lower <- fmt0(base.model$mcmccalcs$dlower[names(base.model$mcmccalcs$dlower) %in% end.yr] * 100, 1)
curr.depl.median <- fmt0(base.model$mcmccalcs$dmed[names(base.model$mcmccalcs$dmed) %in% end.yr] * 100, 1)
curr.depl.upper <- fmt0(base.model$mcmccalcs$dupper[names(base.model$mcmccalcs$dupper) %in% end.yr] * 100, 1)

curr.bio.lower <- fmt0(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% end.yr], 3)
curr.bio.median <- fmt0(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% end.yr], 3)
curr.bio.upper <- fmt0(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% end.yr], 3)

## Estimates of spawning biomass for previous year (calculated in this assessment):
prev.bio.lower <- fmt0(base.model$mcmccalcs$slower[names(base.model$mcmccalcs$slower) %in% last.data.yr], 3)      # last.data.yr = end.yr-1
prev.bio.median <- fmt0(base.model$mcmccalcs$smed[names(base.model$mcmccalcs$smed) %in% last.data.yr], 3)
prev.bio.upper <- fmt0(base.model$mcmccalcs$supper[names(base.model$mcmccalcs$supper) %in% last.data.yr], 3)


## First forecast year depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: First forecast year depletion and spawning biomass estimates\n\n")
}
fore.tac.mcmc <- base.model$forecasts$mcmccalcs[[catch.tac.ind]]
next.depl.lower.tac.based <- fmt0(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 1)] * 100, 1)
next.depl.median.tac.based <- fmt0(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 1)] * 100, 1)
next.depl.upper.tac.based <- fmt0(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 1)] * 100, 1)

next.bio.lower.tac.based <- fmt0(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 1)] * 100, 1)
next.bio.median.tac.based <- fmt0(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 1)] * 100, 1)
next.bio.upper.tac.based <- fmt0(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 1)] * 100, 1)

## Calculations for exec summary and assessment-section.rnw:
##  number of mcmc samples, minimum median biomass,
##  years when fishing intensity > 1
if(verbose){
  cat("DEBUG: Calculations for exec summary and assessment-section.rnw:\n\n")
}
num.mcmc.samples <- dim(base.model$mcmc)[1]
median.bio.min  <- fmt0(min(base.model$mcmccalcs$smed), 3)  # min median biomass
median.bio.min.year <- names(which.min(base.model$mcmccalcs$smed)) # year of min
median.intensity <- base.model$mcmccalcs$pmed
median.intensity.2007.to.2011 <- median.intensity[c("2007", "2008", "2009", "2010", "2011")]
median.intensity.2007.to.2011.min <- fmt0(min(median.intensity.2007.to.2011)*100, 0)
median.intensity.2007.to.2011.max <- fmt0(max(median.intensity.2007.to.2011)*100, 0)
# Could replace some of the following ..pmed with median.intensity:
median.intensity.above.one.all.years <- names(which(base.model$mcmccalcs$pmed > 1))    # includes > end.yr
median.intensity.above.one.years <- median.intensity.above.one.all.years[
         median.intensity.above.one.all.years < end.yr]  # ones to mention
median.intensity.2010 <- fmt0(base.model$mcmccalcs$pmed["2010"] * 100, 1)
median.intensity.penult.yr <- fmt0(base.model$mcmccalcs$pmed[as.character(end.yr-1)] * 100, 1)

median.relative.bio <- base.model$mcmccalcs$dmed
median.relative.bio.2007.to.2011 <- median.relative.bio[c("2007", "2008", "2009", "2010", "2011")]
median.relative.bio.2007.to.2011.min <- fmt0(min(median.relative.bio.2007.to.2011), 2)
median.relative.bio.2007.to.2011.max <- fmt0(max(median.relative.bio.2007.to.2011), 2)
median.relative.bio.below.target <- median.relative.bio[median.relative.bio < 0.4]     # when below target
median.relative.bio.above.target.since <- as.numeric(max(names(median.relative.bio.below.target)))+1   # has been above target since
# Prob biomass declines next year to year after with zero catch:
zero.catch.prob.bio.down.1 <- fmt0(base.model$risks[[1]][1,2])
# Prob biomass declines year after next to year after that with zero catch:
zero.catch.prob.bio.down.2 <- fmt0(base.model$risks[[2]][1,2])



## Second forecast year depletion and spawning biomass estimates
if(verbose){
  cat("DEBUG: Second forecast year depletion and spawning biomass estimates\n\n")
}
next2.depl.lower.tac.based <- fmt0(fore.tac.mcmc$dlower[names(fore.tac.mcmc$dlower) %in% (end.yr + 2)] * 100, 1)
next2.depl.median.tac.based <- fmt0(fore.tac.mcmc$dmed[names(fore.tac.mcmc$dmed) %in% (end.yr + 2)] * 100, 1)
next2.depl.upper.tac.based <- fmt0(fore.tac.mcmc$dupper[names(fore.tac.mcmc$dupper) %in% (end.yr + 2)] * 100, 1)

next2.bio.lower.tac.based <- fmt0(fore.tac.mcmc$slower[names(fore.tac.mcmc$slower) %in% (end.yr + 2)] * 100, 1)
next2.bio.median.tac.based <- fmt0(fore.tac.mcmc$smed[names(fore.tac.mcmc$smed) %in% (end.yr + 2)] * 100, 1)
next2.bio.upper.tac.based <- fmt0(fore.tac.mcmc$supper[names(fore.tac.mcmc$supper) %in% (end.yr + 2)] * 100, 1)

## number.to.word function located in utilities.r
catches.below.200000.since.1986 <- number.to.word(length(filter(catches, TOTAL <= 200000, Year > 1986)$Year))

## Age composition data for data section
survey.age.years <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy == 2,]$Yr
max.survey.age.prop <- make.age.comp.bubble.plot(base.model,
                                                 subplot = 2,
                                                 do.plot = FALSE)
max.fishery.age.prop <- make.age.comp.bubble.plot(base.model,
                                                  subplot = 1,
                                                  do.plot = FALSE)

catch.limit.quantiles <- fmt0(make.forecast.catch.posterior.plot(base.model,
                                   fore.yr = end.yr, do.plot = FALSE) * 1000)
                # 2.5%, median and 97.5% quantiles of catch limit for assess.yr
                #  using the default harvest policy; tonnes

## Estimated numbers at age for fishery for Recruitment section in Exec Summary and main text
##  From make.age.comp.fit.plot() which in turn calls age.fits()
if(verbose){
  cat("DEBUG: Estimated numbers at age for fishery for Recruitment section\n\n")
}
fishery.estimated.age.comp <- base.model$agedbase[base.model$agedbase$Fleet==1,]  #I think that this has ageing error incorporated
year.class.2010.in.2013 <- fmt0(filter(fishery.estimated.age.comp, Yr==2013, Bin==3)$Exp * 100)
year.class.2010.in.2014 <- fmt0(filter(fishery.estimated.age.comp, Yr==2014, Bin==4)$Exp * 100)
year.class.2010.in.2015 <- fmt0(filter(fishery.estimated.age.comp, Yr==2015, Bin==5)$Exp * 100)

tmp <- base.model$catage[base.model$catage$Fleet==1,-(1:10)]  #This does not have ageing error
fishery.estimated.age.comp <- cbind(base.model$catage[base.model$catage$Fleet==1,(1:10)],t(apply(tmp,1,function(x){x/sum(x)})))
year.class.2010.in.2013 <- fmt0(filter(fishery.estimated.age.comp, Yr==2013)$"3" * 100)
year.class.2010.in.2014 <- fmt0(filter(fishery.estimated.age.comp, Yr==2014)$"4" * 100)
year.class.2010.in.2015 <- fmt0(filter(fishery.estimated.age.comp, Yr==2015)$"5" * 100)

catcher.processor.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$atSea_US_CP / (last.year.us.cp.quota.reallocated), 1)
mothership.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$atSea_US_MS / (last.year.us.ms.quota.reallocated), 1)
shore.based.catch <- fmt0(100 * filter(catches, Year == last.data.yr)$US_shore / (last.year.us.shore.quota.reallocated), 1)

## Canadian age data variables
if(verbose){
  cat("DEBUG: Canadian age data variables\n\n")
}
get.age.prop <- function(vec, place = 1){
  ## returns the age prop and the age itself for the place,
  ## where place is 1=max, 2-second highest, etc.
  prop <- rev(sort(vec))
  prop <- prop[place]
  age <- as.numeric(names(vec[vec == prop]))
  return(c(age, prop))
}

## Canadian Freezer trawlers
if(verbose){
  cat("DEBUG: Canadian Freezer trawlers\n\n")
}
last.year.can.ages.ft <- can.ages[[2]][rownames(can.ages[[2]]) == last.data.yr,]
get.age.prop(last.year.can.ages.ft, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 1)
max.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
max.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 2)
second.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
second.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 3)
third.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
third.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 4)
fourth.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
fourth.freezer.trawler.age.prop <- fmt0(ft.age.prop.holder[2] * 100, 1)
## Canadian Shoreside
if(verbose){
  cat("DEBUG: Canadian Shoreside\n\n")
}
last.year.can.ages.ss <- can.ages[[1]][rownames(can.ages[[1]]) == last.data.yr,]
get.age.prop(last.year.can.ages.ss, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 1)
max.shoreside.age.prop.age <- ss.age.prop.holder[1]
max.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 2)
second.shoreside.age.prop.age <- ss.age.prop.holder[1]
second.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 3)
third.shoreside.age.prop.age <- ss.age.prop.holder[1]
third.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)
ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 4)
fourth.shoreside.age.prop.age <- ss.age.prop.holder[1]
fourth.shoreside.age.prop <- fmt0(ss.age.prop.holder[2] * 100, 1)

## Years for which median recruitment is below the mean of the median
##  recruitments for years >2010 and <(end.yr-1) ; end.yr-1 won't be
##  well estimated
recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[ which(as.numeric(names(base.model$mcmccalcs$rmed)) > 2010 & as.numeric(names(base.model$mcmccalcs$rmed)) < (end.yr-1))]
years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])

## Exploitation values
if(verbose){
  cat("DEBUG: Exploitation values\n\n")
}
exploitation.med.2010 <- fmt0(base.model$mcmccalcs$fmed["2010"],2)
exploitation.med.penult.yr <- fmt0(base.model$mcmccalcs$fmed[as.character(end.yr-1)],2)

## Survey comparisons of biomass from year to year. Use the table, not the value of survey.end.year
## Next year, we should set survey.end.yr to be what is in the table. Not going to attempt it
##  with only hours left to submission.
last.survey.year <- survey.history[nrow(survey.history),]$year
last.survey.year.biomass <- fmt0(survey.history[nrow(survey.history),]$biomass * 10, 2) ## millions of tonnes
penult.survey.year <- survey.history[nrow(survey.history) - 1,]$year
penult.survey.year.biomass <- fmt0(survey.history[nrow(survey.history) - 1,]$biomass * 10, 2)
antepenult.survey.year <- survey.history[nrow(survey.history) - 2,]$year
antepenult.survey.year.biomass <- fmt0(survey.history[nrow(survey.history) - 2,]$biomass * 10, 2)
## How many times higher is the last survey than the one before it?
last.factor.penult <- fmt0(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 1,]$biomass, 1)
## How many times higher is the last survey than the one that was two before it?
last.factor.antepenult <- fmt0(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 2,]$biomass, 1)

## Get priors informtaion
split.prior.info <- function(prior.str, dec.points = 1, first.to.lower = FALSE){
  ## Parses a string like Lognormal(2.0,1.01) and returns a vector of length 3:
  ## "Lognormal", 2.0, 1.01
  ## if first.to.lower = TRUE, makes the first letter of the name of the prior lower case.
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- fmt0(as.numeric(p[1]), dec.points)
  p.sd <- fmt0(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}
param.details <- make.parameters.estimated.summary.table(base.model,
                                                         start.rec.dev.yr = recruit.dev.start.yr,
                                                         end.rec.dev.yr = end.yr,
                                                         return.xtable = FALSE)
m.prior <- split.prior.info(param.details[rownames(param.details) == "m.vals",][4], dec.points = 2, first.to.lower = TRUE)
## Now use m.prior[1] for name of prior, m.prior[1] for mean, and m.prior[3] for SD.

cohortCatch <- function(cohort,catage,ages=0:20) {
  cohortYrs <- cohort+ages
  tmp <- as.matrix(catage[catage$Yr %in% cohortYrs,as.character(ages)])
  wtatage <- as.matrix(base.model$wtatage[base.model$wtatage$fleet==1 & base.model$wtatage$yr %in% (-1*cohortYrs),paste("X",ages,sep="")])
  catchWtAtAge <- tmp * wtatage

  ind <- 1:(nrow(tmp)+1)
  if(length(ind) > length(ages)) {ind <- 1:nrow(tmp)}
  catchCoh <- diag(catchWtAtAge[,ind])
  names(catchCoh) <- cohortYrs[1:(nrow(tmp))]
  return(catchCoh)
}
cohort.catch.1999 <- sum(cohortCatch(1999,base.model$catage))
cohort.catch.2010 <- sum(cohortCatch(2010,base.model$catage))


## This chunk must stay last in the file
if(reload.models == "y" | reload.models == "Y" |
   run.forecasts == "y" | run.forecasts == "Y" |
   run.retros == "y" | run.retros == "Y"){
  cat("Saving the image to the .RData file.\n\n")
  save.image()
}else{
  cat("You should call save.image() if you made changes to the R code which need to be seen by the latex/knitr document.\n\n")
}
