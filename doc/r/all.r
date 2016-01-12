## Pacific Hake Joint Technical Committee, January 2016
## all.r - Source this file to load all data and functions,
## then save the R environment to .RData in this directory. This
## will be read in by knitr as a binary file so that multiple
## loads don't happen during the latex/knitr build (they are
## very slow compared to loading the binary once at the beginning).

## Check for specific packages and install them if they
## are not yet installed. Note this will be really slow the
## first time you run it, especially if they have dependencies.

if(!("nwfscSurvey" %in% rownames(installed.packages()))){
  devtools::install_github("nwfsc-assess/nwfscSurvey")
}

if(!("nwfscMapping" %in% rownames(installed.packages()))){
  devtools::install_github("nwfsc-assess/nwfscMapping")
}

if(!("r4ss" %in% rownames(installed.packages()))){
  devtools::install_github("r4ss/r4ss")
}

## if(!("PBSmapping" %in% rownames(installed.packages()))){
##  install.packages("PBSmapping")
##}

require(nwfscSurvey)
require(nwfscMapping)
require(date)
require(r4ss)
require(xtable)
## require(PBSmapping)

source("utilities.r")
source("catches.r")
source("load-models.r")
source("survey.r")
source("figures-timeseries.r")
source("tables-timeseries.r")
source("tables-reference-points.r")
source("tables-decisions.r")

catch.data.file <- "2016HakeCatches_preliminary_2016.01.04.csv"
landings.vs.tac.data.file <- "Landings_vs_TAC.csv"

## Index of the base model as found in the directory.
## i.e. 00_Modelname is index 1, 01_Modelname is index 2, etc.
base.model.ind <- 1

## Unfished equilibrium year
unfished.eq.yr  <- 1964
## Start year for the models
start.yr        <- 1966
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr          <- 2015
## First year in the survey timeseries
survey.start.yr <- 1995
## Last year in the survey timeseries
survey.end.yr   <- 2015
assess.yr       <- end.yr
## The last year an assessment was done
last.assess.yr  <- 2014

forecast.yrs <- 2015:2017
forecast.probs <- c(0.05,0.25,0.5,0.75,0.95)
## catch.levels is a list of N catch levels to run forecasts for
## Each element of the list is a vector of length the same as the
## number of elements in forcast.yrs
catch.levels <- list(c(0.01,0.01,0.01),
                     c(180000,180000,0.01),
                     rep(300000,3),
                     rep(428000,3),
                     rep(710000,3),
                     c(730000,650000,600000),
                     c(804576,682782,0))
## catch.levels.names is a list of N names for the catch levels given in catch.levels
catch.levels.names <- c("0",
                        "medBsame",
                        "300",
                        "428",
                        "stableCatch",
                        "SPR100",
                        "defaultHR")

data.path <- file.path("..","..","data")
models.path <- file.path("..","..","models")

reload.models <- readline(prompt="Reload all models and data? [y/n] ")
run.forecasts <- readline(prompt="Run forecasting for base model (for decision tables)? [y/n] ")
run.risks <- readline(prompt="Run risk calculations for base model (for risk tables)? [y/n] ")

if(reload.models == "y" | reload.models == "Y"){
  catches <- load.catches(file.path(data.path, catch.data.file))
  landings.vs.tac <- load.landings.tac(file.path(data.path, landings.vs.tac.data.file))
  models <- load.models(models.path, yr = end.yr)
  cat("\n\nAll models and data have been reloaded and all code has been re-sourced.\n\n")
}else{
  cat("\n\nModels and data have NOT been reloaded, but all code has been re-sourced.\n\n")
}

if(run.forecasts == "y" | run.forecasts == "Y"){
  ## Delete any old forecasts by removing whole forecasts directory recursively
  forecasts.path <- file.path(models[[base.model.ind]]$path, "mcmc", "forecasts")

  forecasts <- calc.forecast(models[[base.model.ind]]$mcmc,
                             models[[base.model.ind]]$path,
                             forecast.yrs,
                             catch.levels,
                             catch.levels.names,
                             probs = forecast.probs)

  models[[base.model.ind]]$forecasts$biomass <- forecasts[[1]]
  models[[base.model.ind]]$forecasts$spr <- forecasts[[2]]
  models[[base.model.ind]]$forecasts$outputs <- forecasts[[3]]

  cat("\n\nForecast calculations completed for (base) model located in ",models[[base.model.ind]]$path,"\n\n")
}

if(run.risks == "y" | run.risks == "Y"){
  if(is.null(models[[base.model.ind]]$forecasts$outputs)){
    stop("\n\nError - You must successfully run the forecasting step before calculating risks.\n\n")
  }
  ## calc.risk assumes the forecasting step was done correctly
  risks <- calc.risk(models[[base.model.ind]]$forecasts$outputs,
                     forecast.yrs,
                     catch.levels,
                     catch.levels.names)

  models[[base.model.ind]]$risks <- risks
  cat("\n\nRisk calculations completed for (base) model located in ",models[[base.model.ind]]$path,"\n\n")
}
