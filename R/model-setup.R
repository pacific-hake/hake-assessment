## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- 2020
message("Assessment year: ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
message("Last assessment year: ", last.assess.yr)

## Output CSV directory for outputs of at-age which are calculated by the
## make.est.numbers.at.age.table function (in r-functions/tables-age.r)
output.csv.dir <- file.path(rootd, "out-csv")

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
ss_executable <- "ss.exe"
message("SS executable file: ", ss_executable)
starter_file_name <- "starter.ss"
message("SS starter file: ", starter_file_name)
forecast_file_name <- "forecast.ss"
message("SS forecast file: ", forecast_file_name)
weight_at_age_file_name <- "wtatage.ss"
message("SS weight-at-age file: ", weight_at_age_file_name)

## -----------------------------------------------------------------------------
## The version of SS and ADMB used in this assessment
## -----------------------------------------------------------------------------
ss.version <- "3.30.14.08"
message("SS version: ", ss.version)
admb.version <- "12.0"
message("ADMB version: ", admb.version)

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
survey.end.yr <- 2019
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
              2019)

# tick marks for time series plot
big.ticks <- seq(1970, end.yr + 4, 5)
little.ticks <- start.yr:max(big.ticks)

message("Last survey year: ", survey.end.yr)
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- end.yr - 1
last.age.yr <- end.yr - 2
message("Last year of model data: ", last.data.yr)
message("Last year of age data: ", last.age.yr)

## -----------------------------------------------------------------------------
## Key posteriors used in the assessment
## -----------------------------------------------------------------------------
key.posteriors <- c("NatM",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD")
message("Key posteriors in this assessment: ", key.posteriors)

key.posteriors.file <- "keyposteriors.csv"
message("Key posteriors file: ", key.posteriors.file)
nuisance.posteriors.file <- "nuisanceposteriors.csv"
message("Key posteriors file: ", nuisance.posteriors.file)

## -----------------------------------------------------------------------------
## Base model name and directory
## -----------------------------------------------------------------------------
base.model.dir.name <- "2020.01.09_DMprior_base"
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
##last.yr.base.model.dir.name <- "00_45_2017base"
last.yr.base.model.dir.name <- "2019.03.00_base"
last.yr.base.model.name <- paste(last.assess.yr, "Base model")
message("Last assessment year's base model directory name: ", last.yr.base.model.dir.name)
message("Last assessment year's base model pretty name: ", last.yr.base.model.name)

## -----------------------------------------------------------------------------
## Bridge models group 1
## -----------------------------------------------------------------------------
bridge.model.dir.names.1 <- c(last.yr.base.model.dir.name,
                              "2019.03.91_3.30.14.08",
                              "2019.03.92_simpledevs")
bridge.model.names.1 <- c(last.yr.base.model.name,
                          "Update stock synthesis",
                          "Use simple rec devs")
bridge.model.end.yr.1 <- end.yr - c(1, 1, 1) # subtract 1 year from all 4 models


## -----------------------------------------------------------------------------
## Bridge models group 2
## -----------------------------------------------------------------------------
bridge.model.dir.names.2 <- c("2019.03.92_simpledevs",
                              "2020.00.01_update_pre2019_data",
                              "2020.00.02_add_2019_catch",
                              "2020.00.04_add_fishery_comps")
bridge.model.names.2 <- c("Use simple rec devs",
                          "Update pre-2019 data",
                          "Add 2019 catch",
                          "Add 2019 fishery comps and weights")
bridge.model.end.yr.2 <- end.yr - c(1, 0, 0, 0) # subtract 1 year from first 1 models

## -----------------------------------------------------------------------------
## Bridge models group 3
## -----------------------------------------------------------------------------
bridge.model.dir.names.3 <- c("2020.00.04_add_fishery_comps",
                              "2020.00.06_add_2019_survey_bio",
                              "2020.00.07_add_2019_survey_ages",
                              "2020.01.09_DMprior_base")
bridge.model.names.3 <- c("Add 2019 comps and weights",
                          "Update and add 2019 survey biomass",
                          "Add 2019 survey comps",
                          "Add Dirichlet prior and tune (=base model)")
bridge.model.end.yr.3 <- end.yr - c(0, 0, 0, 0) # subtract 1 year from first 1 models

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.model.dir.names.1 <- c("2020.01.15_h_prior_mean_low",
                            "2020.01.16_h_fix_high",
                            "2020.01.17_sigmaR_fix_low",
                            "2020.01.18_sigmaR_fix_high",
                            "2020.01.20_M_0.2SD",
                            "2020.01.21_M_0.3SD")
sens.model.names.1 <- c("Steepness Mean Prior Low (0.5)",
                        "Steepness Fix 1.0",
                        "Sigma R 1.0",
                        "Sigma R 1.8",
                        "Natural Mortality (SD=0.2)",
                        "Natural Mortality (SD=0.3)")

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
sens.model.dir.names.2 <- c("2020.01.23_age1Survey",
                            "2020.01.24_compWeight_HarmonicMean",
                            "2020.01.25_compWeight_Francis")
sens.model.names.2 <- c("Add Age 1 Index",
                        "McAllister Ianelli Weighting",
                        "Francis Weighting")

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## -----------------------------------------------------------------------------
##Group 3 not used for 2019 assessment
##sens.model.dir.names.3 <- c("2019.02.07_maxSel_Age5",
##                            "2019.02.08_maxSel_Age7",
##                            "2019.02.09_maxSel_Age10",
##                            "2019.02.11_tvSelect_phi_xtralow",
##                            "2019.02.12_tvSelect_phi_low",
##                            "2019.02.13_tvSelect_phi_high")
##sens.model.names.3 <- c("Max. age selectivity 5",
##                        "Max. age selectivity 7",
##                        "Max. age selectivity 10",
##                        "Phi t.v. selectivity (0.21)",
##                        "Phi t.v. selectivity (0.70)",
##                        "Phi t.v. selectivity (2.10)")
##
## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- c("2020.01.27_tvSelect_phi_extralow",
                            "2020.01.28_tvSelect_phi_low",
                            "2020.01.29_tvSelect_phi_high",
                            "2020.01.26_semiPara_tvSelect_sig0.695",
                            "2020.01.26_semiPara_tvSelect_sig0.695")
sens.model.names.4 <- c("Phi t.v. selectivity (0.21)",
                        "Phi t.v. selectivity (0.70)",
                        "Phi t.v. selectivity (2.10)",
                        "Semi-Parametric t.v selectivity (0.695)",
                        "Semi-Parametric t.v. selectivity (1.0)")

## -----------------------------------------------------------------------------
## Sensitivity models group 5  - Different weight-at-age schemes (first group)
## -----------------------------------------------------------------------------
sens.model.dir.names.5 <- c("2020.01.30_noCohort_ageError",
                            "2020.01.32_eqFecund_early5",
                            "2020.01.33_eqFecund_early10")
sens.model.names.5 <- c("No ageing error",
                        "Fecundity early 5 yrs",
                        "Fecundity early 10 yrs")

## -----------------------------------------------------------------------------
## Sensitivity models group 6  - Different weight-at-age schemes (second group)
## -----------------------------------------------------------------------------
sens.model.dir.names.6 <- c("2020.01.32_eqFecund_early5",
                            "2020.01.32_eqFecund_early5",
                            "2020.01.32_eqFecund_early5",
                            "2020.01.32_eqFecund_early5")
sens.model.names.6 <- c("TODO: remove this",
                        "TODO: remove this",
                        "TODO: remove this",
                        "TODO: remove this")


## sens.model.names.5 <- c("Early weight-age 1975-2018 mean, late is 2016-2018 mean",             #52
##                         "Early weight-age 1975-2018 mean, late is 1975-2018 mean",             #53
##                         "TV Fecund, early weight-age 1975-2018 mean, late is 2016-2018 mean",  #54
##                         "TV Fecund, early weight-age 1975-1979 mean, late is 1975-2018 mean*", #55
##                         "Early weight-age 1975-1979 mean, late is 2016-2018 mean",             #56
##                         "Early weight-age 1975-1979 mean, late is 1975-2018 mean*",            #57
##                         "TV Fecund, early weight-age 1975-1979 mean, late is 2016-2018 mean")  #58

model_list <- c(base.model.dir.name,
                unlist(bridge.model.dir.names.1),
                unlist(bridge.model.dir.names.2),
                unlist(sens.model.dir.names.1),
                unlist(sens.model.dir.names.2),
                #unlist(sens.model.dir.names.3),
                unlist(sens.model.dir.names.4),
                unlist(sens.model.dir.names.5),
                unlist(sens.model.dir.names.6))
model_list <- model_list[! model_list %in% last.yr.base.model.dir.name]
model_list <- as.list(unique(model_list))

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model <<- load.models(rootd.models, base.model.dir.name)
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

  last.yr.base.model <<- load.models(rootd.models, last.yr.base.model.dir.name)
  bridge.models.1    <<- load.models(rootd.models, bridge.model.dir.names.1)
  bridge.models.2    <<- load.models(rootd.models, bridge.model.dir.names.2)
  sens.models.1      <<- load.models(rootd.models, sens.model.dir.names.1)
  sens.models.2      <<- load.models(rootd.models, sens.model.dir.names.2, TRUE)
  ## sens.models.3      <<- load.models(rootd.models, sens.model.dir.names.3)
  sens.models.4      <<- load.models(rootd.models, sens.model.dir.names.4)
  sens.models.5      <<- load.models(rootd.models, sens.model.dir.names.5)
  sens.models.6      <<- load.models(rootd.models, sens.model.dir.names.6)

  ## Lists of sensitivities for the MLE parameters, derived quantiles,
  ##  and reference points table
  ## First set includes base and sensitivity group 1 and 2
  sens.models.1.for.table <<- c(list(base.model), sens.models.1, sens.models.2)
  sens.model.names.1.for.table <<- c("Base model", sens.model.names.1,sens.model.names.2)
  ## Second set includes base and sensitivity groups 3 and 4

  ## Removing the sens group 4 from this because it's causing problems when
  ## running make.short.parameter.estimates.sens.table()
  sens.models.2.for.table <<- c(list(base.model), sens.models.4)
  sens.model.names.2.for.table <<- c("Base model", sens.model.names.4)
  ## sens.models.2.for.table <<- c(list(base.model), sens.models.3, sens.models.4)
  ## sens.model.names.2.for.table <<- c("Base model", sens.model.names.3, sens.model.names.4)

  ## Third set
  sens.models.3.for.table <<- c(list(base.model), sens.models.5, sens.models.6)
  sens.model.names.3.for.table <<- c("Base model", sens.model.names.5, sens.model.names.6)
}

build <- function(.run_forecasts = FALSE,
                  .run_retrospectives = FALSE,
                  .run_extra_mcmc = FALSE,
                  .run_catch_levels_default_hr = FALSE,
                  .run_catch_levels_spr_100 = FALSE,
                  .run_catch_levels_stable_catch = FALSE,
                  .model_list = model_list,
                  .models_path = here::here("models"),
                  .catch_levels_path = "catch-levels",
                  .default_hr_path = "default-hr",
                  .stable_catch_path = "stable-catch",
                  .spr_100_path = "spr-100",
                  .forecasts_path = "forecasts",
                  .retrospectives_path = "retrospectives",
                  .extra_mcmc_path = "extra-mcmc",
                  .forecast_yrs = forecast_yrs,
                  .retrospective_yrs = retrospective_yrs,
                  .catch_levels = catch_levels,
                  .catch_levels_spr_tol = 0.0001,
                  .catch_levels_catch_tol = 1,
                  .catch_levels_max_iter = 20,
                  .ss_executable = ss_executable,
                  .starter_file_name = starter_file_name,
                  .forecast_file_name = forecast_file_name,
                  .weight_at_age_file_name = weight_at_age_file_name,
                  .ovwrt_rdata = TRUE){

  lapply(.model_list, function(.model_name){
    if(.run_forecasts |
       .run_retrospectives |
       .run_extra_mcmc |
       .run_catch_levels_default_hr |
       .run_catch_levels_spr_100 |
       .run_catch_levels_stable_catch){
      run(models_path = .models_path,
          model_name = .model_name,
          run_catch_levels_default_hr = .run_catch_levels_default_hr,
          run_catch_levels_spr_100 = .run_catch_levels_spr_100,
          run_catch_levels_stable_catch = .run_catch_levels_stable_catch,
          run_forecasts = .run_forecasts,
          run_retrospectives = .run_retrospectives,
          retrospective_yrs = .retrospective_yrs,
          run_extra_mcmc = .run_extra_mcmc,
          forecast_yrs = .forecast_yrs,
          catch_levels = .catch_levels,
          catch_levels_spr_tol = .catch_levels_spr_tol,
          catch_levels_catch_tol = .catch_levels_catch_tol,
          catch_levels_max_iter = .catch_levels_max_iter,
          catch_levels_path = .catch_levels_path,
          default_hr_path = .default_hr_path,
          stable_catch_path = .stable_catch_path,
          spr_100_path = .spr_100_path,
          forecasts_path = .forecasts_path,
          retrospectives_path = .retrospectives_path,
          extra_mcmc_path = .extra_mcmc_path,
          ss_executable = .ss_executable,
          starter_file_name = .starter_file_name,
          forecast_file_name = .forecast_file_name,
          weight_at_age_file_name = .weight_at_age_file_name)
    }
  })
  lapply(.model_list, function(.model_name){
    create_rdata_file(models_path = .models_path,
                      model_name = .model_name,
                      ovwrt_rdata = .ovwrt_rdata,
                      forecast_yrs = .forecast_yrs,
                      retrospective_yrs = .retrospective_yrs,
                      catch_levels = .catch_levels,
                      catch_levels_path = .catch_levels_path,
                      default_hr_path = .default_hr_path,
                      stable_catch_path = .stable_catch_path,
                      spr_100_path = .spr_100_path,
                      forecasts_path = .forecasts_path,
                      retrospectives_path = .retrospectives_path,
                      extra_mcmc_path = .extra_mcmc_path)
  })
  message("\nCompleted build.")
  invisible()
}  
