## -----------------------------------------------------------------------------
## Set verbosity for this project (R code) and SS shell calls
## -----------------------------------------------------------------------------
verbose <- TRUE
ss.verbose <- FALSE

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- 2019
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## -----------------------------------------------------------------------------
model.dir <- rootd.models
if(verbose) cat0("Models directory: \n  ", model.dir)

## Output CSV directory for outputs of at-age which are calculated by the
## make.est.numbers.at.age.table function (in r-functions/tables-age.r)
output.csv.dir <- file.path(rootd, "out-csv")

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
## The version of SS and ADMB used in this assessment
## -----------------------------------------------------------------------------
ss.version <- "3.30"
if(verbose) cat0("SS version: \n  ", ss.version)
admb.version <- "11.6"
if(verbose) cat0("ADMB version: \n  ", admb.version)

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
## Start year for the fishery age comps
start.yr.age.comps <- 1975
if(verbose) cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- assess.yr
if(verbose) cat0("End year for model: \n  ", end.yr)
## First year in the survey timeseries
survey.start.yr <- 1995
if(verbose) cat0("First survey year: \n  ", survey.start.yr)
## Last year in the survey timeseries
survey.end.yr <- 2017
## Years in which the survey took place
surv.yrs <- c(1995,
              1998,
              2001,
              2003,
              2005,
              2007,
              2009,
              2011,
              2012,
              2013,
              2015,
              2017)

# tick marks for time series plot
big.ticks <- seq(1970, end.yr + 4, 5)
little.ticks <- start.yr:max(big.ticks)

if(verbose) cat0("Last survey year: \n  ", survey.end.yr)
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- end.yr - 1
last.age.yr <- end.yr - 2
if(verbose) cat0("Last year of model data: \n  ", last.data.yr)
if(verbose) cat0("Last year of age data: \n  ", last.age.yr)

## -----------------------------------------------------------------------------
## Key posteriors used in the assessment
## -----------------------------------------------------------------------------
key.posteriors <- c("NatM",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD")
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
base.model.dir.name <- "2019.02.00_base_model"
base.model.name <- paste0(assess.yr, " Base model")
if(verbose){
  cat0("Base model directory name: \n  ", base.model.dir.name)
  cat0("Base model pretty name: \n  ", base.model.name)
}

## -----------------------------------------------------------------------------
## Alternative base model name and directory
## -----------------------------------------------------------------------------
alt.base.model.1.dir.name <- "2019.02.36_fecundity"
alt.base.model.1.name <- paste0(assess.yr, " Short-term pre-1975 wt at age")
alt.base.model.2.dir.name <- "2019.02.32_fecundity"
alt.base.model.2.name <- paste0(assess.yr, " Long-term pre-1975 wt at age")
alt.base.model.3.dir.name <- "2019.02.38_fecundity"
alt.base.model.3.name <- paste0(assess.yr, " TV Fec, short-term pre-1975 wt at age")

## -----------------------------------------------------------------------------
## Last assessment year's base model name and directory
## -----------------------------------------------------------------------------
##last.yr.base.model.dir.name <- "00_45_2017base"
last.yr.base.model.dir.name <- "2018.40_base_model"
last.yr.base.model.name <- paste(last.assess.yr, "Base model")
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
                              "2019.02.40_update_historic_catch",
                              "2019.02.41_add_2018_catch",
                              "2019.02.42_wtatage_2018",
                              "2019.02.43_h_INIT_095",
                              "2019.02.44_proj_wtatage")
bridge.model.names.1 <- c(last.yr.base.model.name,
                          "Corrected historic catch",
                          "Add 2018 total catch",
                          "Add 2018 weight-at-age",
                          "Change initial steepness",
                          "Use 2018 wtatage for projections")
bridge.model.end.yr.1 <- end.yr - c(1, 1, 0, 0, 0, 0) # subtract 1 year from first two models


## -----------------------------------------------------------------------------
## Bridge models group 2
## -----------------------------------------------------------------------------
bridge.model.dir.names.2 <- c(last.yr.base.model.dir.name,
                              "2019.02.44_proj_wtatage",
                              "2019.02.45_fecundity_time_varying")
bridge.model.names.2 <- c(last.yr.base.model.name,
                          "Result of first bridging model set",
                          "Time-varying fecundity (= base model)")
bridge.model.end.yr.2 <- end.yr - c(1, 0, 0) # subtract 1 year from first 2 models

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.model.dir.names.1 <- c("2019.02.01_h_prior_mean_low",
                            "2019.02.02_h_fix_high",
                            "2019.02.03_sigmaR_fix_low",
                            "2019.02.04_sigmaR_fix_high",
                            "2019.02.05_M_0.2SD",
                            "2019.02.06_M_0.3SD")
sens.model.names.1 <- c("Steepness Mean Prior Low (0.5)",
                        "Steepness Fix 1.0",
                        "Sigma R 1.0",
                        "Sigma R 1.8",
                        "Natural Mortality (SD=0.2)",
                        "Natural Mortality (SD=0.3)")

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
sens.model.dir.names.2 <- c("2019.02.16_age1Survey",
                            "2019.02.14_ageError_noCohort",
                            "2019.02.15_compWeight_HarmonicMean")
sens.model.names.2 <- c("Add Age 1 Index",
                        "Ageing Error (cohort invariant)",
                        "Harmonic Mean Data Weighting")

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## -----------------------------------------------------------------------------
sens.model.dir.names.3 <- c("2019.02.07_maxSel_Age5",
                            "2019.02.08_maxSel_Age7",
                            "2019.02.09_maxSel_Age10",
                            "2019.02.11_tvSelect_phi_xtralow",
                            "2019.02.12_tvSelect_phi_low",
                            "2019.02.13_tvSelect_phi_high")
sens.model.names.3 <- c("Max. age selectivity 5",
                        "Max. age selectivity 7",
                        "Max. age selectivity 10",
                        "Phi t.v. selectivity (0.21)",
                        "Phi t.v. selectivity (0.70)",
                        "Phi t.v. selectivity (2.10)")

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- c("2019.02.10_semiPara_tvSelect_sig0.695",
                            "2019.02.20_semiPara_tvSelect_sig1.0")
sens.model.names.4 <- c("Semi-Parametric t.v selectivity (0.695)",
                        "Semi-Parametric t.v. selectivity (1.0)")

## -----------------------------------------------------------------------------
## Sensitivity models group 5  - Different weight-at-age schemes
## -----------------------------------------------------------------------------
sens.model.dir.names.5 <- c("2019.02.35_fecundity")
sens.model.names.5 <- c("Early weight-at-age as 1975-1979 average")

## -----------------------------------------------------------------------------
## Sensitivity models group 6 - more weight-at-age options, may merge with 5
## -----------------------------------------------------------------------------
sens.model.dir.names.6 <- c("2019.02.37_fecundity",
                            "2019.02.33_fecundity",
                            "2019.02.35_fecundity",
                            "2019.02.34_fecundity")
sens.model.names.6 <- c("Constant fecundity, run 37",
                        "Constant fecundity, run 33",
                        "Run 35",
                        "Run 34")

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
  sens.models.1      <<- load.models(model.dir, sens.model.dir.names.1)
  sens.models.2      <<- load.models(model.dir, sens.model.dir.names.2, TRUE)
  sens.models.3      <<- load.models(model.dir, sens.model.dir.names.3)
  sens.models.4      <<- load.models(model.dir, sens.model.dir.names.4)
  sens.models.5      <<- load.models(model.dir, sens.model.dir.names.5)
  sens.models.6      <<- load.models(model.dir, sens.model.dir.names.6)

  ## Lists of sensitivities for the MLE parameters, derived quantiles,
  ##  and reference points table
  ## First set includes base and sensitivity group 1 and 2
  sens.models.1.for.table <<- c(list(base.model), sens.models.1, sens.models.2)
  sens.model.names.1.for.table <<- c("Base model", sens.model.names.1,sens.model.names.2)
  ## Second set includes base and sensitivity groups 3 and 4

  ## Removing the sens group 4 from this because it's causing problems when
  ## running make.short.parameter.estimates.sens.table()
  ##sens.models.2.for.table <<- c(list(base.model), sens.models.1, sens.models.4)
  ##sens.model.names.2.for.table <<- c("Base model", sens.model.names.1,sens.model.names.4)
  sens.models.2.for.table <<- c(list(base.model), sens.models.3, sens.models.4)
  sens.model.names.2.for.table <<- c("Base model", sens.model.names.3, sens.model.names.4)

  ## Third set
  sens.models.3.for.table <<- c(list(base.model), list(sens.models.5), sens.models.6)
  sens.model.names.3.for.table <<- c("Base model", list(sens.model.names.5), sens.model.names.6)
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
                    ss.version = ss.version,
                    verbose = ss.verbose)

  ## Bridge and sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the FOR loop to work right
  mnv <- c(unlist(bridge.model.dir.names.1),
           unlist(bridge.model.dir.names.2),
           unlist(sens.model.dir.names.1),
           unlist(sens.model.dir.names.2),
           unlist(sens.model.dir.names.3),
           unlist(sens.model.dir.names.4),
           unlist(sens.model.dir.names.5),
           unlist(sens.model.dir.names.6))

  model.names.list <- as.list(unique(mnv))

  ## Bridge/sensitivity models
  for(model.nm in model.names.list){
    create.rdata.file(
      model.name = model.nm,
      ovwrt.rdata = ifelse(run.extra.mcmc,
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
      ss.version = ss.version,
      verbose = ss.verbose)
  }
}
