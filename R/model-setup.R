## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- assess_yr
message("Assessment year: ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
message("Last assessment year: ", last.assess.yr)

## Output CSV directory for outputs of at-age which are calculated by the
## atage_table() function (in r-functions/tables-age.r)
output.csv.dir <- file.path(rootd, "out-csv")

## -----------------------------------------------------------------------------
## The versions of software used in this assessment
## -----------------------------------------------------------------------------
ss.version <- "3.30.18"
ss.commit <- "a0f982"
ss.commit.date <- "January 10, 2022"

admb.version <- "12.3"
admb.commit <- "1376cb"
admb.commit.date <- "September 11, 2021"

r4ss.commit <- "1e70b9"
adnuts.commit <- "0bc76b"

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Recruitment deviations start year
recruit.dev.start.yr <- 1946
message("Recruitment deviations start year: ", recruit.dev.start.yr)
## Unfished equilibrium year.
unfished.eq.yr <- 1964
message("Unfished equilibrium year: ", unfished.eq.yr)
## Start year for the models
start.yr <- 1966
message("Start year for catch data: ", start.yr)
## Start year for the fishery age comps
start.yr.age.comps <- 1975
message("Start year for fishery age comps data: ", start.yr.age.comps)
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- assess.yr
message("End year for model: ", end.yr)
## First year in the survey timeseries
survey.start.yr <- 1995
message("First survey year: ", survey.start.yr)
## Last year in the survey timeseries
survey.end.yr <- 2021
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
              2017,
              2019,
              2021)

# tick marks for time series plot (not catch time series though)
big.ticks <- seq(1970, end.yr + 4, 5)
little.ticks <- start.yr:max(big.ticks)

message("Last survey year: ", survey.end.yr)
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- end.yr - 1
message("Last year of model data: ", last.data.yr)

## -----------------------------------------------------------------------------
## Key posteriors used in the assessment
## -----------------------------------------------------------------------------
key.posteriors <- c("NatM",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD_Acoustic_Survey",
                    "ln\\(DM_theta\\)_1",
                    "ln\\(DM_theta\\)_2")
key.posteriors.titles <- c("Natural mortality",
                           "LN(R0)",
                           "Steepness",
                           "Survey extra SD",
                           "Dirichlet-Multinomial fishery",
                           "Dirichlet-Multinomial survey")
message("Key posteriors in this assessment: ", key.posteriors)

key.posteriors.file <- "keyposteriors.csv"
message("Key posteriors file: ", key.posteriors.file)
nuisance.posteriors.file <- "nuisanceposteriors.csv"
message("Key posteriors file: ", nuisance.posteriors.file)

## -----------------------------------------------------------------------------
## Base model name and directory
## -----------------------------------------------------------------------------
base.model.dir.name <- "2022.01.10_base_v2"
base.model.name <- paste0(assess.yr, " Base model")

message("Base model directory name: ", base.model.dir.name)
message("Base model pretty name: ", base.model.name)


## -----------------------------------------------------------------------------
## Alternative base model names and directories (runs we want MCMC results for,
##  not necessarily considering as alt runs for 2019).
## -----------------------------------------------------------------------------
## alt.base.model.1.dir.name <- "2019.02.36_fecundity"
## alt.base.model.1.name <- paste0(assess.yr, " Short-term pre-1975 wt at age")
## alt.base.model.2.dir.name <- "2019.02.32_fecundity"
## alt.base.model.2.name <- paste0(assess.yr, " Long-term pre-1975 wt at age")
## alt.base.model.3.dir.name <- "2019.02.38_fecundity"
## alt.base.model.3.name <- paste0(assess.yr, " TV Fec, short-term pre-1975 wt at age")

## -----------------------------------------------------------------------------
## Last assessment year's base model name and directory
## -----------------------------------------------------------------------------
last.yr.base.model.dir.name <- "2021.00.04_base_v1"
last.yr.base.model.name <- paste(last.assess.yr, "Base model")
message("Last assessment year's base model directory name: ", last.yr.base.model.dir.name)
message("Last assessment year's base model pretty name: ", last.yr.base.model.name)

## -----------------------------------------------------------------------------
## Bridge models group 1
## -----------------------------------------------------------------------------
## First one must be last.yr.base.model.dir.name:
bridge.model.dir.names.1 <- c(last.yr.base.model.dir.name,
                              "2022.01.03_newcatchage",
                              "2022.01.05_updatesurvey",
                              "2022.01.06_newsurvey",
                              "2022.01.07_newwtatage")
bridge.model.names.1 <- c(last.yr.base.model.name,
                          "Update all fishery catch and comps",
                          "Update pre-2021 survey data",
                          "Add 2021 survey data",
                          "Update wt-at-age data")
bridge.model.end.yr.1 <- end.yr - c(1, 0, 0, 0, 0) # subtract 1 year from all 4 models


## -----------------------------------------------------------------------------
## Bridge models group 2
## -----------------------------------------------------------------------------
bridge.model.dir.names.2 <- c("2022.01.07_newwtatage",
                              "2022.01.09_age1index")
bridge.model.names.2 <- c("Update wt-at-age data",
                          "Add age 1 index = 2022 base model")
bridge.model.end.yr.2 <- end.yr - c(0, 0)

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
## NOTE: If any sensitivity models change order or definition, then check that
##  definitions are still correct in custom-knitr-variables.R (seach that for
##  'sens.model'); also see load_models_rds() defined at the end of this file.
## NOTE: For ADNUTS diagnostics document, update the list in adnuts-diagnostics.rnw if
##  sensitivity models get added or removed here.
sens.model.dir.names.1 <- c("2022.01.15_h_prior_mean_low",
                            "2022.01.16_h_fix_high",
                            "2022.01.17_sigmR_fix_low",
                            "2022.01.18_sigmR_fix_high",
                            "2022.01.20_M_0.2SD",
                            "2022.01.21_M_0.3SD")
sens.model.names.1 <- c("Steepness Mean Prior Low (0.5)",
                        "Steepness Fix 1.0",
                        "Sigma R 1.0",
                        "Sigma R 1.6",
                        "Natural Mortality (SD=0.2)",
                        "Natural Mortality (SD=0.3)")

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
## **For 2022: change first one (and its name) to be the run that excludes age1 survey, then
## full appendix (and other results) will be somewhat automatic (even if some
## variables have 'age1' in them, we know this to be the sens that removes age-1
## index instead of adding it (as in the past).**
sens.model.dir.names.2 <- c("2022.01.23_age1Survey",
                            "2022.01.24_compWeight_HarmonicMean")
sens.model.names.2 <- c("Remove Age 1 Index",
                        "Downweight Fishery Comps")

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- c("2022.01.27_tvSelect_phi_extralow",
                            "2022.01.28_tvSelect_phi_low",
                            "2022.01.29_tvSelect_phi_high")
sens.model.names.4 <- c("Phi t.v. selectivity (0.21)",
                        "Phi t.v. selectivity (0.70)",
                        "Phi t.v. selectivity (2.10)")

## -----------------------------------------------------------------------------
## Sensitivity models group 5  -
## -----------------------------------------------------------------------------
## sens.model.dir.names.5 <- c("2022.01.28_tvSelect_phi_low")
## sens.model.names.5 <- c("DUMMY FOR NOW")

## -----------------------------------------------------------------------------
## Sensitivity models group 6  -
## -----------------------------------------------------------------------------
sens.model.dir.names.6 <- c("2022.01.43_maxSel_Age5",
                            "2022.01.44_maxSel_Age7",
                            "2022.01.45_maxSel_Age8")
sens.model.names.6 <- c("Max. age selectivity 5",
                        "Max. age selectivity 7",
                        "Max. age selectivity 8")

## -----------------------------------------------------------------------------
## Sensitivity models group 7  - MH mcmc
## -----------------------------------------------------------------------------
## sens.model.dir.names.7 <- c("2022.01.28_tvSelect_phi_low")
## sens.model.names.7 <- c("DUMMY FOR NOW")

model_list <- c(base.model.dir.name,
                unlist(bridge.model.dir.names.1),
                unlist(bridge.model.dir.names.2),
                unlist(sens.model.dir.names.1),
                unlist(sens.model.dir.names.2),
                unlist(sens.model.dir.names.4),
                # unlist(sens.model.dir.names.5),
                unlist(sens.model.dir.names.6))
                # unlist(sens.model.dir.names.7))
model_list <- model_list[! model_list %in% last.yr.base.model.dir.name]
model_list <- as.list(unique(model_list))

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load_models_rds <- function(){
  base.model <<- load_models(base.model.dir.name)
  if(is.null(base.model$mcmccalcs)){
    stop("Error - base.model$mcmccalcs is NULL. Make sure the directory\n",
            file.path(base.model$path, "mcmc"), " exists and contains valid\n",
            "   mcmc output, set ovwrt.rdata = TRUE in the create.rdata.file() calls\n",
            "   within build_rds() in model-setup.r, and try again.\n")
  }
  if(is.null(base.model$risks)){
    stop("Error - base.model$risks is NULL. Maybe you forgot to run the forecasting?\n",
           "   Make sure to setup running and/or loading of forecasts, and\n",
           "   set ovwrt.rdata = TRUE in the create.rdata.file() calls\n",
           "   within build_rds() in model-setup.r and try again.\n")
  }

  last.yr.base.model <<- load_models(last.yr.base.model.dir.name)

  # For 2022 assessment only due to changes in SS3 to make figures and tables work.
  #  Can remove in 2023:
  if(last.yr.base.model.name == "2021 Base model"){
    last.yr.base.model$mcmc[, grep("SSB", names(last.yr.base.model$mcmc))] <<-
      last.yr.base.model$mcmc[, grep("SSB", names(last.yr.base.model$mcmc))]/2
  }

  bridge.models.1 <<- load_models(bridge.model.dir.names.1)
  # For 2022 assessment only due to changes in SS3 to make figures and tables work.
  #  Can remove in 2023:
  if(bridge.model.names.1[1] == "2021 Base model"){
    bridge.models.1[[1]]$mcmc[, grep("SSB", names(bridge.models.1[[1]]$mcmc))] <<-
      bridge.models.1[[1]]$mcmc[, grep("SSB", names(bridge.models.1[[1]]$mcmc))]/2
  }

  bridge.models.2 <<- load_models(bridge.model.dir.names.2)
  sens.models.1 <<- load_models(sens.model.dir.names.1)
  sens.models.2 <<- load_models(sens.model.dir.names.2, TRUE)
  sens.models.4 <<- load_models(sens.model.dir.names.4)
  # sens.models.5 <<- load_models(sens.model.dir.names.5)
  sens.models.6 <<- load_models(sens.model.dir.names.6)
  # sens.models.7 <<- load_models(sens.model.dir.names.7)
#  srg_day2_req1_model <<- load_models("2022.01.47_day2request_wtatage")
#  srg_day2_req2_model <<- load_models("2022.01.46_base_v1_4yrForecast")

  ## Lists of sensitivities for the MLE parameters, derived quantiles,
  ##  and reference points table
  ## First set includes base and sensitivity group 1 and 2
  sens.models.1.for.table <<- c(list(base.model), sens.models.1, sens.models.2)
  sens.model.names.1.for.table <<- c("Base model", sens.model.names.1,sens.model.names.2)
  ## Second set includes base and sensitivity groups 3 and 4

  ## Removing the sens group 4 from this because it's causing problems when
  ## running param_est_table()
  sens.models.2.for.table <<- c(list(base.model),
                                sens.models.4,
                                sens.models.6)
  sens.model.names.2.for.table <<- c("Base model",
                                     sens.model.names.4,
                                     sens.model.names.6)

  ## Third set
  # sens.models.3.for.table <<- c(list(base.model), list(sens.models.5), sens.models.6, list(sens.models.7))
  # sens.model.names.3.for.table <<- c("Base model", sens.model.names.5, sens.model.names.6, sens.model.names.7)

  ## Fourth set
  # sens.models.4.for.table <<- c(list(base.model), list(sens.models.6))
  # sens.model.names.4.for.table <<- c("Base model", sens.model.names.6)
}
