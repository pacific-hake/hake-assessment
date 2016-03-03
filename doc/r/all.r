## Pacific Hake Joint Technical Committee
## all.r - Source this file to load all data and functions,
##  to run the forecasting and retrospectives for the base model,
##  then save the R environment to .RData in this directory. This
##  will be read in by knitr as a binary file so that multiple
##  loads don't happen during the latex/knitr build (they are
##  very slow compared to loading the binary once at the beginning).

rm(list = ls(all = TRUE))

## The purpose of the r-functions directory is to separate the
##  r code which is not commonly changed so that the files which are
##  can be mode clearly seen.
func.dir <- "r-functions"

## Need to source utilities.r before everything because it contains the function
##  install.packages.if.needed
source(file.path(func.dir, "utilities.r"))
install.packages.if.needed("devtools", "devtools", github=FALSE)
install.packages.if.needed("nwfscSurvey", "nwfsc-assess/nwfscSurvey", github=TRUE)
install.packages.if.needed("nwfscMapping", "nwfsc-assess/nwfscMapping", github=TRUE)
install.packages.if.needed("date", "date", github=FALSE)
install.packages.if.needed("r4ss", "r4ss/r4ss", github=TRUE)
install.packages.if.needed("xtable", "xtable", github=FALSE)
install.packages.if.needed("PBSmapping", "PBSmapping", github=FALSE)
install.packages.if.needed("maps", "maps", github=FALSE)
install.packages.if.needed("coda", "coda", github=FALSE)
install.packages.if.needed("dplyr", "dplyr", github = FALSE)
install.packages.if.needed("maptools", "maptools", github = FALSE)
install.packages.if.needed("gtools", "gtools", github = FALSE)

require(nwfscSurvey)
require(nwfscMapping)
require(date)
require(r4ss)
require(xtable)
require(PBSmapping)
require(maps)
require(dplyr)
require(coda)
require(gtools)
require(maptools)

source(file.path(func.dir, "catches.r"))      ## Contains the code to catch/TAC data and figure and table-making code for catch/TAC
source(file.path(func.dir, "load-models.r"))  ## Contains the code to load the models list from the model directories
source(file.path(func.dir, "survey.r"))       ## Contains the table-making code for survey
source(file.path(func.dir, "load-data.r"))    ## Contains the code to load data tables, including survey data table
source(file.path(func.dir, "read-list.r"))    ## Contains the code to read a user file into an R list (for model setup)

source(file.path(func.dir, "figures-timeseries.r"))
source(file.path(func.dir, "figures-compare-forecasts.r"))
source(file.path(func.dir, "figures-mcmc-diagnostics.r"))
source(file.path(func.dir, "figures-age-comps.r"))
source(file.path(func.dir, "figures-selex.r"))
source(file.path(func.dir, "figures-stock-recruitment.r"))
source(file.path(func.dir, "figures-mle-mcmc.r"))
source(file.path(func.dir, "figures-overview-map.r"))
source(file.path(func.dir, "figures-data.r"))
source(file.path(func.dir, "figures-assessment-history.r"))

source(file.path(func.dir, "tables-timeseries.r"))
source(file.path(func.dir, "tables-reference-points.r"))
source(file.path(func.dir, "tables-decisions.r"))
source(file.path(func.dir, "tables-age.r"))
source(file.path(func.dir, "tables-parameters.r"))
source(file.path(func.dir, "tables-sampling.r"))
source(file.path(func.dir, "tables-maturity.r"))

source(file.path(func.dir, "verify.r"))
source("model-setup.r")
source("retrospective-setup.r")

stop()
################################################################################
## Variables to be used in the knitr code chunks
################################################################################

## Simpler variables for the models, so we can get rid of models list
base.model <- models[[base.model.ind]]
cat("Base model is ", base.model$path, "\n\n")
bridge.models.1 <- models[bridge.model.inds.1]
bridge.models.2 <- models[bridge.model.inds.2]
bridge.models.3 <- models[bridge.model.inds.3]
sens.models.1 <- models[sens.model.inds.1]
sens.models.2 <- models[sens.model.inds.2]
sens.models.3 <- models[sens.model.inds.3]
