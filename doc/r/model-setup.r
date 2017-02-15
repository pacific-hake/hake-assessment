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
ss.version <- "3.24"
## The version of ADMB that this version of SS was compiled with
admb.version <- "11.2"
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
survey.start.yr <- 1995
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
base.model.dir.name <- "45_BasePreSRG_v4"
## "01_2016base_converted_to_SSv3.30"
base.model.name <- paste(assess.yr, "Base model")
verify.models(model.dir, base.model.dir.name, base.model.name)
if(verbose){
  cat0("Base model directory name: \n  ", base.model.dir.name)
  cat0("Base model pretty name: \n  ", base.model.name)
}

## -----------------------------------------------------------------------------
## Last assessment year's base model name and directory
## -----------------------------------------------------------------------------
last.yr.base.model.dir.name <- "00_55_2016base"
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
                              "10_UpdatePre2016catches",
                              "11_UpdatePre2016FishComps",
                              "15_Add2016Catch",
                              "16_Add2016FishComps")
bridge.model.names.1 <- c(last.yr.base.model.name,
                          "Update pre-2016 catches",
                          "Update pre-2016 fishery age comps",
                          "Add 2016 catch",
                          "Add 2016 fishery age comps")
verify.models(model.dir, bridge.model.dir.names.1, bridge.model.names.1)
if(verbose){
  print.model.message(bridge.model.dir.names.1, bridge.model.names.1, 1, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Bridge models group 2
## -----------------------------------------------------------------------------
bridge.model.dir.names.2 <- c(last.yr.base.model.dir.name,
                              "12_Update1998SurveyIndex",
                              "13_Update2015SurveyIndexCVonly",
                              "14_Add1995SurveyIndex")
bridge.model.names.2 <- c(last.yr.base.model.name,
                          "Update 1998 survey index",
                          "Update 2015 survey cv",
                          "Add 1995 survey index")
verify.models(model.dir, bridge.model.dir.names.2, bridge.model.names.2)
if(verbose){
  print.model.message(bridge.model.dir.names.2, bridge.model.names.2, 2, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Bridge models group 3
## -----------------------------------------------------------------------------
bridge.model.dir.names.3 <- c(last.yr.base.model.dir.name,
                              "17_AdjustBiasRamp",
                              "40_SenRecdevMain2014",
                              "41_BasePreSRG_v3",
                              "45_BasePreSRG_v4")
## Bridge model names will be used to make the bridge model plot and its caption.
bridge.model.names.3 <- c(last.yr.base.model.name,
                          "Adjust recr. bias ramp",
                          "Include 2014 in vector of main recr. devs",
                          "More flexible time-varying selectivity",
                          "Adjust effective sample sizes")
verify.models(model.dir, bridge.model.dir.names.3, bridge.model.names.3)
if(verbose){
  print.model.message(bridge.model.dir.names.3, bridge.model.names.3, 3, model.type = "Bridge")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.model.dir.names.1 <- c("57_Sen45_sigmaR_1.0",
                            "58_Sen45_sigmaR_2.0",
                            "62_Sen45_sigmaR_1.51")
sens.model.names.1 <- c("Sigma R 1.0",
                        "Sigma R 2.0",
                        "Sigma R 1.51")
verify.models(model.dir, sens.model.dir.names.1, sens.model.names.1)
if(verbose){
  print.model.message(sens.model.dir.names.1, sens.model.names.1, 1, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
sens.model.dir.names.2 <- "46_Sen45_AdjustBiasRampEnd2014"
sens.model.names.2 <- "Adjust bias ramp to 2014"
verify.models(model.dir, sens.model.dir.names.2, sens.model.names.2)
if(verbose){
  print.model.message(sens.model.dir.names.2, sens.model.names.2, 2, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## -----------------------------------------------------------------------------
sens.model.dir.names.3 <- c("47_Sen45MaxSelectAge5",
                            "48_Sen45MaxSelectAge7",
                            "49_Sen45MaxSelectAge10")
sens.model.names.3 <- c("Max. age selectivity 5",
                        "Max. age selectivity 7",
                        "Max. age selectivity 10")
verify.models(model.dir, sens.model.dir.names.3, sens.model.names.3)
if(verbose){
  print.model.message(sens.model.dir.names.3, sens.model.names.3, 3, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- c("50_Sen45h0.5prior",
                            "51_Sen45h1.0fix",
                            "52_Sen45M0.2SD",
                            "53_Sen45M0.3SD")
sens.model.names.4 <- c("Steepness prior mean 0.5",
                        "Fix steepness 1.0",
                        "Natural mortality SD 0.2",
                        "Natural mortality SD 0.3")
verify.models(model.dir, sens.model.dir.names.4, sens.model.names.4)
if(verbose){
  print.model.message(sens.model.dir.names.4, sens.model.names.4, 4, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 5
## -----------------------------------------------------------------------------
sens.model.dir.names.5 <- c("54_Sen45AgeErrorNoCohort",
                            "55_Sen45AgeError2014noAdjust",
                            "56_Sen45AddAge1Index")
sens.model.names.5 <- c("Ageing error: cohort invariant",
                        "Ageing error: standard for 2014",
                        "Include age-1 index")
verify.models(model.dir, sens.model.dir.names.5, sens.model.names.5)
if(verbose){
  print.model.message(sens.model.dir.names.5, sens.model.names.5, 5, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 6
## -----------------------------------------------------------------------------
sens.model.dir.names.6 <- c("59_Sen45_phi003",
                            "60_Sen45_phi010",
                            "61_Sen45_phi030")
sens.model.names.6 <- c("Selectivity SD 0.03",
                        "Selectivity SD 0.10",
                        "Selectivity SD 0.30")
verify.models(model.dir, sens.model.dir.names.6, sens.model.names.6)
if(verbose){
  print.model.message(sens.model.dir.names.6, sens.model.names.6, 6, model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 7 - Request from SRG
## -----------------------------------------------------------------------------
sens.model.dir.names.7 <- c("56_Sen45AddAge1Index",
                            "63_Sen_phi003_age1_index",
                            "64_Sen_phi010_age1_index",
                            "65_Sen_phi030_age1_index")
sens.model.names.7 <- c("Include age-1 index",
                        "Selectivity SD 0.03 w/age1",
                        "Selectivity SD 0.10 w/age1",
                        "Selectivity SD 0.30 w/age1")
verify.models(model.dir, sens.model.dir.names.7, sens.model.names.7)
if(verbose){
  print.model.message(sens.model.dir.names.7, sens.model.names.7, 7, model.type = "Sensitivity")
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
                     sens.model.dir.names.5,
                     sens.model.dir.names.6,
                     sens.model.dir.names.7)

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model         <<- load.models(model.dir, base.model.dir.name)
  ## Error checks:
  if(is.null(base.model$mcmccalcs)){
    stop("Error - base.model$mcmccalcs is NULL. Make sure the directory\n",
            file.path(base.model$path, "mcmc"), " exists and contains valid\n",
            "   mcmc output, set ovwrt.rdata = TRUE in the create.rdata.file() calls\n",
            "   within build() in model-setup.r, and try again.\n")
  }
  if(is.null(base.model$risks)){
    stop("Error - base.model$risks is NULL. Maybe you forgot to run the forecasting?\n",
           "   Make sure to setup running and/or loading of forecasts, and\n",
           "   set ovwrt.rdata = TRUE in the create.rdata.file() calls\n",
           "   within build() in model-setup.r and try again.\n")
  }

  last.yr.base.model <<- load.models(model.dir, last.yr.base.model.dir.name)
  bridge.models.1    <<- load.models(model.dir, bridge.model.dir.names.1)
  bridge.models.2    <<- load.models(model.dir, bridge.model.dir.names.2)
  bridge.models.3    <<- load.models(model.dir, bridge.model.dir.names.3)
  sens.models.1      <<- load.models(model.dir, sens.model.dir.names.1)
  sens.models.2      <<- load.models(model.dir, sens.model.dir.names.2, TRUE)
  sens.models.3      <<- load.models(model.dir, sens.model.dir.names.3)
  sens.models.4      <<- load.models(model.dir, sens.model.dir.names.4)
  sens.models.5      <<- load.models(model.dir, sens.model.dir.names.5)
  sens.models.6      <<- load.models(model.dir, sens.model.dir.names.6)
  sens.models.7      <<- load.models(model.dir, sens.model.dir.names.7)

  ## Lists of sensitivities for the MLE parameters, derived quantiles,
  ##  and reference points table
  ## First set includes base and sensitivity group 1 and 3
  sens.models.1.for.table <<- c(list(base.model), sens.models.5, sens.models.6)
  sens.model.names.1.for.table <<- c("Base model", sens.model.names.5,sens.model.names.6)
  ## Second set includes base and sensitivity groups 2 and 3
  sens.models.2.for.table <<- c(list(base.model), sens.models.1, sens.models.4)
  #sens.models.2.for.table <<- c(sens.models.2.for.table,sens.models.3)
  sens.model.names.2.for.table <<- c("Base model", sens.model.names.1,sens.model.names.4)

}

build <- function(run.fore = FALSE,
                  run.retro = FALSE,
                  run.extra.mcmc = FALSE){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.

  ## Delete old directories for all models
  if(run.extra.mcmc){
    delete.dirs(sub.dir = file.path("extra-mcmc"))
  }
  if(run.fore){
    delete.dirs(sub.dir = file.path("mcmc", "forecasts"))
  }
  if(run.retro){
    delete.dirs(sub.dir = file.path("retrospectives"))
  }

  ## Base model
  create.rdata.file(model.name = base.model.dir.name,
                    ovwrt.rdata = ifelse(any(run.fore, run.retro, run.extra.mcmc),
                                         TRUE,
                                         FALSE),
                    run.forecasts = run.fore,
                    fore.yrs = forecast.yrs,
                    forecast.probs = forecast.probs,
                    forecast.catch.levels = catch.levels,
                    run.retros = run.retro,
                    my.retro.yrs = retro.yrs,
                    run.extra.mcmc = run.extra.mcmc,
                    key.posteriors = key.posteriors,
                    verbose = ss.verbose)

  ## Bridge and sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the FOR loop to work right
  mnv <- c(unlist(bridge.model.dir.names.1),
           unlist(bridge.model.dir.names.2),
           unlist(bridge.model.dir.names.3),
           unlist(sens.model.dir.names.1),
           unlist(sens.model.dir.names.2),
           unlist(sens.model.dir.names.3),
           unlist(sens.model.dir.names.4),
           unlist(sens.model.dir.names.5),
           unlist(sens.model.dir.names.6),
           unlist(sens.model.dir.names.7))

  ## Remove base model from the bridge/sensitivity list
  mnv <- mnv[-(grep(base.model.dir.name, mnv))]
  model.names.list <- as.list(unique(mnv))

  ## Bridge/sensitivity models
  for(model.nm in model.names.list){
    create.rdata.file(
      model.name = model.nm,
      ovwrt.rdata = ifelse(run.extra.mcmc,
                           TRUE,
                           FALSE),
      run.forecasts = FALSE,
      fore.yrs = forecast.yrs,
      forecast.probs = forecast.probs,
      forecast.catch.levels = catch.levels,
      run.retros = FALSE,
      my.retro.yrs = retro.yrs,
      run.extra.mcmc = run.extra.mcmc,
      key.posteriors = key.posteriors,
      verbose = ss.verbose)
  }
}
