# Pacific Hake Joint Technical Committee
# This is the master file - it loads all packages and sources all
#  other R source code files.
#
# To debug in an R session, run these 3 commands first:
# source(here::here("R/all.R"));load_models_rds();source(here::here("R/custom-knitr-variables.R"))

# This is so the s3_dir() command shows all results
options(max.print = 999999)

library(adnuts)
library(aws.s3)
library(coda)
library(cowplot)
library(data.tree)
library(date)
library(dplyr)
library(future)
library(furrr)
library(here)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(grDevices)
library(grid)
library(gridExtra)
library(gtools)
library(here)
library(kableExtra)
library(knitr)
library(lubridate)
library(maps)
library(maptools)
library(matrixcalc)
library(parallel)
library(purrr)
library(r4ss)
library(reshape2)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthhires)  # devtools::install_github("ropensci/rnaturalearthhires")
library(rstan)
library(scales)
library(shinystan)
library(sf)
library(snowfall)
library(stringr)
library(testthat)
library(tictoc)
library(tidyverse)
library(xtable)

models_path <- Sys.getenv("MODELS_DIR")
if(models_path == ""){
  models_path <- "models"
}

rootd <- here::here()
rootd.R <- file.path(rootd, "R")
rootd.admin <- file.path(rootd, "admin")
rootd.data <- file.path(rootd, "data")
rootd.map.data <- file.path(rootd.data, "map-data")
rootd.data.prep <- file.path(rootd, "data-prep")
rootd.doc <- file.path(rootd, "doc")
rootd.extra.calcs <- file.path(rootd, "extra-calculations")
rootd.models <- file.path(rootd, models_path)
rootd.pres <- file.path(rootd, "beamer")

catch_levels_path <- "catch-levels"
default_hr_path <- "default-hr"
stable_catch_path <- "stable-catch"
spr_100_path <- "spr-100"
forecasts_path <- "forecasts"
retrospectives_path <- "retrospectives"

ss_executable <- "ss"
starter_file_name <- "starter.ss"
par_file_name <- "ss.par"
forecast_file_name <- "forecast.ss"
weight_at_age_file_name <- "wtatage.ss"
posts_file_name <- "posteriors.sso"
derposts_file_name <- "derived_posteriors.sso"
report_file_name <- "Report.sso"
compreport_file_name <- "CompReport.sso"
# For linking commands together in a shell
cmd_link <- " && "

# Custom catch levels calculations
# The tolerance in the spr away from 1 for the calculation of catch for SPR = 1
catch_levels_spr_tol <- 0.001
# The tolerance in tonnes. The iterations will stop if the difference between the
#  projected biomass between the first and second years is less than this
catch_levels_catch_tol <- 50
# The maximum number of iterations to do. If this is reached, then no catch value could
#  be found within the tolerances above
catch_levels_max_iter <- 20

assess_yr <- 2022
forecast_yrs <- assess_yr:(assess_yr + 3)
forecast_yrs_extra <- assess_yr:(assess_yr + 3)
forecast_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

retrospective_yrs <- 1:10
plot.retro.yrs <- 1:5

show_ss_output <- FALSE

source(file.path(rootd.R, "utilities.R"))
source(file.path(rootd.R, "add-alt-text.R"))
source(file.path(rootd.R, "catches.R"))
source(file.path(rootd.R, "run-catch-levels.R"))
source(file.path(rootd.R, "run-forecasts.R"))
source(file.path(rootd.R, "run-retrospectives.R"))
source(file.path(rootd.R, "runs_sens_base.R"))
source(file.path(rootd.R, "create-rds-file.R"))
source(file.path(rootd.R, "build-doc.R"))
source(file.path(rootd.R, "delete-files.R"))
source(file.path(rootd.R, "extra-mcmc.R"))
source(file.path(rootd.R, "extract-sigma-r.R"))
source(file.path(rootd.R, "load-models.R"))
source(file.path(rootd.R, "run-adnuts.R"))
source(file.path(rootd.R, "survey.R"))
source(file.path(rootd.R, "load-data.R"))
source(file.path(rootd.R, "read-list.R"))
source(file.path(rootd.R, "figures-timeseries.R"))
source(file.path(rootd.R, "figures-timeseries-squidhist.R"))
source(file.path(rootd.R, "figures-compare-forecasts.R"))
source(file.path(rootd.R, "figures-mcmc-diagnostics.R"))
source(file.path(rootd.R, "figures-age-comps.R"))
source(file.path(rootd.R, "figures-selex.R"))
source(file.path(rootd.R, "figures-stock-recruitment.R"))
source(file.path(rootd.R, "figures-mle-mcmc.R"))
source(file.path(rootd.R, "figures-mcmc-param-stats.R"))
source(file.path(rootd.R, "figures-overview-map.R"))
source(file.path(rootd.R, "figures-data.R"))
source(file.path(rootd.R, "figures-assessment-history.R"))
source(file.path(rootd.R, "figures-age-comp-forecast.R"))
source(file.path(rootd.R, "figures-SPR-illustration-appendix.R"))
source(file.path(rootd.R, "figures-selectivity-parameterizations.R"))
source(file.path(rootd.R, "figures-size-at-age.R"))
source(file.path(rootd.R, "figures-maturity-ogive.R"))
source(file.path(rootd.R, "figures-management.R"))
source(file.path(rootd.R, "figures-R0-vs-meanRecruitment.R"))
source(file.path(rootd.R, "figures-makebox.R"))
source(file.path(rootd.R, "plotcolour.R"))
source(file.path(rootd.R, "s3.R"))
source(file.path(rootd.R, "tables-cohort.R"))
source(file.path(rootd.R, "tables-timeseries.R"))
source(file.path(rootd.R, "tables-reference-points.R"))
source(file.path(rootd.R, "tables-decisions.R"))
source(file.path(rootd.R, "tables-age.R"))
source(file.path(rootd.R, "tables-assessmentchanges.R"))
source(file.path(rootd.R, "tables-parameters.R"))
source(file.path(rootd.R, "tables-sampling.R"))
source(file.path(rootd.R, "tables-squid.R"))
source(file.path(rootd.R, "tables-maturity.R"))
source(file.path(rootd.R, "theme.R"))
source(file.path(rootd.R, "model-setup.R"))
source(file.path(rootd.R, "forecast-catch-levels.R"))
source(file.path(rootd.R, "data-tables.R"))
source(file.path(rootd.R, "useful-quantities.R"))
source(file.path(rootd.R, "historical-probs.R"))
source(file.path(rootd.R, "recruitment-question-responses.R"))
theme_set(hake_theme())
