## -----------------------------------------------------------------------------
## Set verbosity for this project (R code) and SS shell calls
## -----------------------------------------------------------------------------
verbose <- TRUE
ss.verbose <- FALSE

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- as.numeric(substr(Sys.Date(), 1, 4))
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## -----------------------------------------------------------------------------
model.dir <- file.path("..", "..", "models")
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
exe.file.name <- "ss3.exe"
if(verbose) cat0("SS executable file: \n  ", exe.file.name)
starter.file.name <- "starter.ss"
if(verbose) cat0("SS starter file: \n  ", starter.file.name)
forecast.file.name <- "forecast.ss"
if(verbose) cat0("SS forecast file: \n  ", forecast.file.name)
weight.at.age.file.name <- "wtatage.ss"
if(verbose) cat0("SS weight-at-age file: \n  ", weight.at.age.file.name)

## -----------------------------------------------------------------------------
## The version of Stock Synthesis used in this assessment
## -----------------------------------------------------------------------------
ss.version <- "3.24U"
if(verbose) cat0("SS version: \n  ", ss.version)

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Recruitment deviations start year
recruit.dev.start.yr <- 1946
if(verbose) cat0("Recruitment deviations start year: \n  ", recruit.dev.start.yr)
## Unfished equilibrium year.
unfished.eq.yr <- 1964
if(verbose) cat0("Unfished equilibrium year: \n  ", unfished.eq.yr)
## Start year for the models
start.yr <- 1966
if(verbose) cat0("Start year for catch data: \n  ", start.yr)
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- assess.yr
if(verbose) cat0("End year for model: \n  ", end.yr)
## First year in the survey timeseries
survey.start.yr <- 1998
if(verbose) cat0("First survey year: \n  ", survey.start.yr)
## Last year in the survey timeseries
survey.end.yr <- 2015
if(verbose) cat0("Last survey year: \n  ", survey.end.yr)
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- end.yr - 1
if(verbose) cat0("Last year of model data: \n  ", last.data.yr)

## -----------------------------------------------------------------------------
## Key posteriors used in the assessment
## -----------------------------------------------------------------------------
key.posteriors <- c("NatM_p_1_Fem_GP_1",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD_2_Acoustic_Survey")
if(verbose){
  cat0("***")
  cat0("Key posteriors in this assessment:")
  cat(paste0("  ", key.posteriors), sep = "\n")
  cat0("***")
}
key.posteriors.file <- "keyposteriors.csv"
if(verbose) cat0("Key posteriors file: \n  ", key.posteriors.file)
nuisance.posteriors.file <- "nuisanceposteriors.csv"
if(verbose) cat0("Key posteriors file: \n  ", nuisance.posteriors.file)

## -----------------------------------------------------------------------------
## Base model name and directory
## -----------------------------------------------------------------------------
base.model.dir.name <- "55_2016base"
base.model.name <- paste(assess.yr, "Base model")
verify.models(model.dir, base.model.dir.name, base.model.name)
if(verbose){
  cat0("Base model directory name: \n  ", base.model.dir.name)
  cat0("Base model pretty name: \n  ", base.model.name)
}

## -----------------------------------------------------------------------------
## Last assessment year's base model name and directory
## -----------------------------------------------------------------------------
last.yr.base.model.dir.name <- "00_2015hake_basePreSRG"
last.yr.base.model.name <- paste(last.assess.yr, "Base model")
verify.models(model.dir, base.model.dir.name, base.model.name)
if(verbose){
  cat0("Last assessment year's base model directory name: \n  ",
       last.yr.base.model.dir.name)
  cat0("Last assessment year's base model pretty name: \n  ",
       last.yr.base.model.name)
}

## -----------------------------------------------------------------------------
## Bridge models group 1
## -----------------------------------------------------------------------------
bridge.model.dir.names.1 <- c(last.yr.base.model.dir.name,
                              "03_UpdatePre2015WtAge")
bridge.model.names.1 <- c(last.yr.base.model.name,
                          "Update data")
verify.models(model.dir, bridge.model.dir.names.1, bridge.model.names.1)
if(verbose){
  print.model.message(bridge.model.dir.names.1, bridge.model.names.1, 1, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Bridge models group 2
## -----------------------------------------------------------------------------
bridge.model.dir.names.2 <- c(last.yr.base.model.dir.name,
                              "56_Add2015Survey_withExtrap",
                              "57_Add2015Catch_FishAcomps_withExtrap")
bridge.model.names.2 <- c(last.yr.base.model.name,
                          paste0("Add ", survey.end.yr, " survey series"),
                          paste0("Add ", last.data.yr, " fishery data"))
verify.models(model.dir, bridge.model.dir.names.2, bridge.model.names.2)
if(verbose){
  print.model.message(bridge.model.dir.names.2, bridge.model.names.2, 2, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Bridge models group 3
## -----------------------------------------------------------------------------
bridge.model.dir.names.3 <- c("57_Add2015Catch_FishAcomps_withExtrap",
                              "57.01_adjustBiasRamping",
                              "57.02_ChangeSurveyTuning",
                              "57.03_ChangeAllTuning")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.3 <- c("Base model pretune",
                          "Adjust bias ramp",
                          "Change survey comp weights",
                          "Change all comp weights")
verify.models(model.dir, bridge.model.dir.names.3, bridge.model.names.3)
if(verbose){
  print.model.message(bridge.model.dir.names.3, bridge.model.names.3, 3, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.model.dir.names.1 <- c("62_Sensbase_sigmaR_1.0",
                            "63_Sensbase_sigmaR_2.0",
                            "64_Sensbase_h_0.5prior",
                            "65_Sensbase_h_1.0fix",
                            "67_Sensbase_M_SD0.2",
                            "68_Sensbase_M_SD0.3")
sens.model.names.1 <- c("Sigma R 1.0",
                        "Sigma R 2.0",
                        "Steepness prior mean 0.5",
                        "Steepness fixed mean 1.0",
                        "Natural mortality SD 0.2",
                        "Natural mortality SD 0.3")
verify.models(model.dir, sens.model.dir.names.1, sens.model.names.1)
if(verbose){
  print.model.message(sens.model.dir.names.1, sens.model.names.1, 1, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
sens.model.dir.names.2 <- c("58_Sensbase_Survey_noExtrap",
                            "66_Sensbase_Age1Index")
sens.model.names.2 <- c("No extrapolation on survey",
                        "Include age-1 index")
verify.models(model.dir, sens.model.dir.names.2, sens.model.names.2)
if(verbose){
  print.model.message(sens.model.dir.names.2, sens.model.names.2, 2, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## -----------------------------------------------------------------------------
sens.model.dir.names.3 <- c("59_Sensbase_Selmaxage5",
                            "60_Sensbase_Selmaxage7",
                            "61_Sensbase_Selmaxage12")
sens.model.names.3 <- c("Max. age of selectivity 5",
                        "Max. age of selectivity 7",
                        "Max. age of selectivity 12")
verify.models(model.dir, sens.model.dir.names.3, sens.model.names.3)
if(verbose){
  print.model.message(sens.model.dir.names.3, sens.model.names.3, 3, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- "69_Sensbase_AgeError_noCohort"
sens.model.names.4 <- "No cohort ageing error"
verify.models(model.dir, sens.model.dir.names.4, sens.model.names.4)
if(verbose){
  print.model.message(sens.model.dir.names.4, sens.model.names.4, 4, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 5
## -----------------------------------------------------------------------------
sens.model.dir.names.5 <- "70_no2015FisheryComps"
sens.model.names.5 <- paste("No", last.data.yr, "fishery ages")
verify.models(model.dir, sens.model.dir.names.5, sens.model.names.5)
if(verbose){
  print.model.message(sens.model.dir.names.5, sens.model.names.5, 5, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Vector of directory names for all models referenced above
## -----------------------------------------------------------------------------
## ALL models must be in this list!
## Each model directory listed here will have an RData file in it,
##  or one will be created depending on what is found in the directory.
##  i.e. mcmc, retrospective, or forecast directories.
model.dir.names <- c(base.model.dir.name,
                     last.yr.base.model.dir.name,
                     bridge.model.dir.names.1,
                     bridge.model.dir.names.2,
                     bridge.model.dir.names.3,
                     sens.model.dir.names.1,
                     sens.model.dir.names.2,
                     sens.model.dir.names.3,
                     sens.model.dir.names.4,
                     sens.model.dir.names.5)

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model         <<- load.models(model.dir, base.model.dir.name)
  last.yr.base.model <<- load.models(model.dir, last.yr.base.model.dir.name)
  bridge.models.1    <<- load.models(model.dir, bridge.model.dir.names.1)
  bridge.models.2    <<- load.models(model.dir, bridge.model.dir.names.2)
  bridge.models.3    <<- load.models(model.dir, bridge.model.dir.names.3)
  sens.models.1      <<- load.models(model.dir, sens.model.dir.names.1)
  sens.models.2      <<- load.models(model.dir, sens.model.dir.names.2)
  sens.models.3      <<- load.models(model.dir, sens.model.dir.names.3)
  sens.models.4      <<- load.models(model.dir, sens.model.dir.names.4)
  sens.models.5      <<- load.models(model.dir, sens.model.dir.names.5)

  ## Lists of sensitivities for the MLE parameters, derived quantiles,
  ##  and reference points table
  ## First set includes base and sensitivity group1
  sens.models.1.for.table <<- c(list(base.model), sens.models.1)
  sens.model.names.1.for.table <<- c("Base model", sens.model.names.1)
  ## Second set includes base and sensitivity groups 2 and 3
  sens.models.2.for.table <<- c(list(base.model), sens.models.2)
  sens.models.2.for.table <<- c(sens.models.2.for.table,
                                sens.models.3)
  sens.model.names.2.for.table <<- c("Base model", sens.model.names.2,
                                     sens.model.names.3)

  load("model-partest.RData")
  model.partest <<- model.partest
}
