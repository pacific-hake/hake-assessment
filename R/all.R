# Pacific Hake Joint Technical Committee
# This is the master file - it loads all packages and sources all other R
# source code files.
#
# To debug in an R session, run these 3 commands first:
# source(here::here("R/all.R"));load_models_rds();source(here::here("R/custom-knitr-variables.R"))

assess_yr <- 2023
last_assess_yr <- assess_yr - 1
model_version <- "01"

# Show non-scientific notation
options(max.print = 999999)

# To install rnaturalearthhires (it was hard to find with Google):
# devtools::install_github("ropensci/rnaturalearthhires")
# devtools::install_github("pbs-assess/gfutilities")
message("Loading R packages...")
pacman::p_load(adnuts, cli, coda, crayon, coda, cowplot, data.tree, date, dplyr,
               future, furrr, here, gfutilities, ggplot2, ggpubr, ggrepel,
               grDevices, grid, gridExtra, gtools, here, kableExtra, knitr,
               lubridate, maps, maptools, matrixcalc, parallel, purrr, r4ss,
               RColorBrewer,
               reshape2, rgeos, rnaturalearth, rnaturalearthhires, rstan,
               scales, shinystan, sf, snowfall, stringr, testthat, tictoc,
               tidyverse, xtable)
message("Finished loading R packages...")

rootd <- here::here()
rootd_r <- file.path(rootd, "R")
rootd_admin <- file.path(rootd, "admin")
rootd_data <- file.path(rootd, "data")
rootd_map_data <- file.path(rootd_data, "map-data")
rootd_data_prep <- file.path(rootd, "data-prep")
rootd_doc <- file.path(rootd, "doc")
rootd_extra_calcs <- file.path(rootd, "extra-calculations")
rootd_pres <- file.path(rootd, "beamer")

sys_info <- Sys.info()
computer_name <- sys_info[["nodename"]]
os_name <- sys_info[["sysname"]]
user_name <- sys_info[["user"]]

if(computer_name == "hake-precision"){
  models_dir <- file.path("/srv",
                          "hake",
                          "models",
                          assess_yr,
                          paste0(model_version, "-version"))
  last_yr_models_dir <- file.path("/srv",
                                  "hake",
                                  "models",
                                  last_assess_yr,
                                  paste0(model_version, "-version"))
}else{
  models_dir <- file.path(rootd, "models",
                          assess_yr,
                          paste0(model_version, "-version"))
  last_yr_models_dir <- file.path(rootd, "models_last_yr",
                                  last_assess_yr,
                                  paste0(model_version, "-version"))
}

catch_levels_path <- "catch-levels"
default_hr_path <- "default-hr"
stable_catch_path <- "stable-catch"
spr_100_path <- "spr-100"
forecasts_path <- "forecasts"
retrospectives_path <- "retrospectives"

ss_executable <- "ss3"
starter_file_name <- "starter.ss"
par_file_name <- "ss.par"
forecast_file_name <- "forecast.ss"
weight_at_age_file_name <- "wtatage.ss"
posts_file_name <- "posteriors.sso"
derposts_file_name <- "derived_posteriors.sso"
report_file_name <- "Report.sso"
compreport_file_name <- "CompReport.sso"

# Custom catch levels calculations
# The tolerance in the spr away from 1 for the calculation of catch for SPR = 1
catch_levels_spr_tol <- 0.001
# The tolerance in tonnes. The iterations will stop if the difference between the
#  projected biomass between the first and second years is less than this
catch_levels_catch_tol <- 50
# The maximum number of iterations to do. If this is reached, then no catch value could
#  be found within the tolerances above
catch_levels_max_iter <- 20

forecast_yrs <- assess_yr:(assess_yr + 3)
forecast_yrs_extra <- assess_yr:(assess_yr + 3)
forecast_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

retrospective_yrs <- 1:10
plot_retro_yrs <- 1:5

show_ss_output <- FALSE

source_all <- function(lst){
  walk(lst, ~{
    source(file.path(rootd_r, .x))
  })
}

src_lst <- c("utilities.R", "add-alt-text.R", "catches.R",
             "run-catch-levels.R", "run-forecasts.R", "run-retrospectives.R",
             "create-sens-dirs.R", "create-rds-file.R", "build-doc.R",
             "delete-files.R", "extra-mcmc.R", "extract-sigma-r.R",
             "set-dirs.R", "load-models.R", "run-adnuts.R", "survey.R",
             "load-data.R", "read-list.R", "figures-timeseries.R",
             "figures-timeseries-squidhist.R", "figures-compare-forecasts.R",
             "figures-mcmc-diagnostics.R", "figures-age-comps.R",
             "figures-selex.R", "figures-stock-recruitment.R",
             "figures-mle-mcmc.R", "figures-mcmc-param-stats.R",
             "figures-overview-map.R", "figures-data.R",
             "figures-assessment-history.R", "figures-age-comp-forecast.R",
             "figures-SPR-illustration-appendix.R",
             "figures-selectivity-parameterizations.R", "figures-size-at-age.R",
             "figures-maturity-ogive.R", "figures-management.R",
             "figures-R0-vs-meanRecruitment.R", "figures-makebox.R",
             "plotcolour.R", "plot-color.R", "plot-depl-fore-comparison.R",
             "s3.R", "tables-cohort.R", "tables-timeseries.R",
             "tables-reference-points.R", "tables-decisions.R", "tables-age.R",
             "tables-assessmentchanges.R", "tables-parameters.R",
             "tables-sampling.R", "tables-squid.R", "tables-maturity.R",
             "theme.R", "model-setup.R", "forecast-catch-levels.R",
             "data-tables.R", "useful-quantities.R", "historical-probs.R",
             "recruitment-question-responses.R", "run.R", "build-rds.R",
             "append-retros.R", "run-adnuts.R", "run-adnuts-timed.R")
source_all(src_lst)

theme_set(hake_theme())

# -----------------------------------------------------------------------------
# Data start and endpoint variables
# -----------------------------------------------------------------------------
recruit_dev_start_yr <- 1946
unfished_eq_yr <- 1964
start_yr <- 1966
start_yr_age_comps <- 1975
end_yr <- assess_yr
last_data_yr <- end_yr - 1
survey_start_yr <- 1995
survey_end_yr <- 2021
surv_yrs <- c(1995, 1998, 2001, 2003, 2005, 2007,
              2009, 2011, 2012, 2013, 2015, 2017,
              2019, 2021)

# tick marks for time series plot (not catch time series though)
big_ticks <- seq(1970, end_yr + 4, 5)
small_ticks <- start_yr:max(big_ticks)

# -----------------------------------------------------------------------------
# Key posteriors used in the assessment
# -----------------------------------------------------------------------------
key_posteriors <- c("NatM",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD_Acoustic_Survey",
                    "ln\\(DM_theta\\)_1",
                    "ln\\(DM_theta\\)_2")
key_posteriors_titles <- c("Natural mortality",
                           "LN(R0)",
                           "Steepness",
                           "Survey extra SD",
                           "Dirichlet-Multinomial fishery",
                           "Dirichlet-Multinomial survey")
key_posteriors_file <- "keyposteriors.csv"
nuisance_posteriors_file <- "nuisanceposteriors.csv"

# Don't allow partial matches using the dollar operator
options(warnPartialMatchDollar = TRUE)
# Error out on all warnings
#options(warn = 0)
