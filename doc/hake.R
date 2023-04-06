## ----setup, echo=FALSE, cache=FALSE, message=FALSE, results='hide', warning=FALSE-------------------
library(knitr)
devtools::load_all(here::here())
if (is_latex_output()) {
  knitr_figs_dir <- "knitr-figs-pdf/"
  knitr_cache_dir <- "knitr-cache-pdf/"
  fig_out_type <- "png"
} else {
  knitr_figs_dir <- "knitr-figs-docx/"
  knitr_cache_dir <- "knitr-cache-docx/"
  fig_out_type <- "png"
}
fig_asp <- 0.618
fig_width <- 9
fig_out_width <- "6in"
fig_dpi <- 180
fig_align <- "center"
fig_pos <- "htb"
opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.path = knitr_figs_dir,
  cache.path = knitr_cache_dir,
  fig.asp = fig_asp,
  fig.width = fig_width,
  out.width = fig_out_width,
  echo = FALSE,
  #  autodep = TRUE,
  #  cache = TRUE,
  cache.comments = FALSE,
  dev = fig_out_type,
  dpi = fig_dpi,
  fig.align = fig_align,
  fig.pos = fig_pos
)


## ----load-packages-chunk, echo = FALSE--------------------------------------------------------------

# To install rnaturalearthhires (it was hard to find with Google):
# devtools::install_github("ropensci/rnaturalearthhires")
# devtools::install_github("pbs-assess/gfutilities")
message("Loading R packages...")
pacman::p_load(
  # Alphabetical order
  adnuts,
  cli, coda, cowplot, crayon,
  data.tree, date, dplyr,
  future, furrr,
  gfutilities, ggh4x, ggplot2, ggpubr, ggrepel, grDevices, grid,
  gridGraphics, gridExtra, gtools,
  here,
  kableExtra, knitr,
  lubridate,
  maps, maptools, matrixcalc,
  parallel, purrr,
  r4ss, RColorBrewer, readr, reshape2, rgeos, rnaturalearth, rnaturalearthhires, rstan,
  scales, shinystan, sf, stringr,
  testthat, tictoc, tidyr, tools,
  xtable)
message("Finished loading R packages...")


## ----load-globals-chunk, echo = FALSE---------------------------------------------------------------

curr_month <- month(Sys.Date())
curr_year <- year(Sys.Date())

# If in September 01 to December 31, the assess_yr will be set to the year
# that  begins in January. If in January 01 to August 31 the assess_yr will
# be the current year
assess_yr <- ifelse(curr_month %in% 9:12, curr_year + 1, curr_year)
last_assess_yr <- assess_yr - 1
model_version <- "01"
last_yr_model_version <- "01"

sys_info <- Sys.info()
computer_name <- sys_info[["nodename"]]
os_name <- sys_info[["sysname"]]
user_name <- sys_info[["user"]]

rootd_doc <- here::here("doc")
rootd_data <- here::here("inst/extdata/data")
rootd_models <- ifelse(computer_name == "hake-precision",
                       "/srv/hake/models",
                       here::here("models"))
models_dir <- file.path(rootd_models,
                        assess_yr,
                        paste0(model_version, "-version"))
last_yr_models_dir <- file.path(rootd_models,
                                last_assess_yr,
                                paste0(last_yr_model_version, "-version"))

output_csv_dir <- here::here(rootd_doc, "out-csv")

ct_levels_path <- "catch-levels"
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
ct_levels_spr_tol <- 0.001
# The tolerance in tonnes. The iterations will stop if the difference between the
#  projected biomass between the first and second years is less than this
ct_levels_catch_tol <- 100
# The maximum number of iterations to do. If this is reached, then no catch value could
#  be found within the tolerances above
ct_levels_max_iter <- 20

forecast_yrs <- assess_yr:(assess_yr + 3)
forecast_yrs_extra <- assess_yr:(assess_yr + 3)
forecast_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

retrospective_yrs <- 1:10
plot_retro_yrs <- 1:5

show_ss_output <- FALSE

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
                    "ln\\(DM_theta\\)_Age_P1",
                    "ln\\(DM_theta\\)_Age_P2")
key_posteriors_titles <- c("Natural mortality",
                           expression(ln(R[0])),
                           "Steepness",
                           "Survey extra SD",
                           "Dirichlet-multinomial fishery",
                           "Dirichlet-multinomial survey")
key_posteriors_file <- "keyposteriors.csv"
nuisance_posteriors_file <- "nuisanceposteriors.csv"


## ----load-data-tables-chunk, echo = FALSE-----------------------------------------------------------
library(readr)
# Data file names and loading ----
load_dir <- system.file("extdata", "data", package = "hake")
if(load_dir == ""){
  stop("The directory containing the data tables does not exist. Install the ",
       "`hake` package and try again",
       call. = FALSE)
}
if(!dir.exists(load_dir)){
  stop("The directory `", load_dir, "` does not exist. Install the `hake` ",
       "package and try again",
       call. = FALSE)
}
message("Loading all data tables (csv files) from `",
        load_dir,
        "`")

# Assessment history and changes ----
pkg <- "hake"
assess_history_df <-
  read_csv(file.path(load_dir, "assessment-history.csv"),
           col_types = cols(),
           show_col_types = FALSE)
assess_history_probs_df <-
  read_csv(file.path(load_dir, "assessment-history-probs.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
assess_changes_df <-
  read_csv(file.path(load_dir, "assessment-changes.csv"),
           col_types = cols(),
           show_col_types = FALSE)
assess_history_disp_df <-
  read_csv(file.path(load_dir, "assessment-history-SSBdispersion.csv"),
           col_types = cols(),
           show_col_types = FALSE)

# Maturity and weight-at-age ----
ovary_samples_df <-
  read_csv(file.path(load_dir, "ovary-samples.csv"),
           col_types = cols(),
           show_col_types = FALSE)
maturity_ogives_df <-
  read_csv(file.path(load_dir, "maturity-table.csv"),
           col_types = cols(),
           show_col_types = FALSE)
maturity_samples_df <-
  read_csv(file.path(load_dir, "hake-maturity-data.csv"),
           guess_max = Inf,
           show_col_types = FALSE)
weight_age_extrapolation_mask <-
  read_csv(file.path(load_dir, "wtatage_all_samplesize.csv"),
           col_types = cols(),
           show_col_types = FALSE)

# Catch and TAC ----
ct <-
  load_catches(file.path(load_dir, "landings-tac-history.csv"))
catch_targets_df <-
  read_csv(file.path(load_dir, "catch-targets-biomass.csv"),
           col_types = cols(),
           show_col_types = FALSE)
further_tac_df <-
  read_csv(file.path(load_dir, "further-tac-details.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
# * Canadian catch ----
can_ft_catch_by_month_df <-
  read_csv(file.path(load_dir, "can-ft-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ss_catch_by_month_df <-
  read_csv(file.path(load_dir, "can-ss-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_jv_catch_by_month_df <-
  read_csv(file.path(load_dir, "can-jv-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
# * US catch ----
us_ss_catch_by_month_df <-
  read_csv(file.path(load_dir, "us-shore-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_cp_catch_by_month_df <-
  read_csv(file.path(load_dir, "us-cp-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_ms_catch_by_month_df <-
  read_csv(file.path(load_dir, "us-ms-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_ti_ct_by_month_df <-
  read_csv(file.path(load_dir, "us-ti-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_research_catch_by_month_df <-
  read_csv(file.path(load_dir, "us-research-catch-by-month.csv"),
           col_types = cols(),
           show_col_types = FALSE)

# Sampling data ----
sampling_history_df <-
  read_csv(file.path(load_dir, "fishery-sampling-history.csv"),
           col_types = cols(),
           show_col_types = FALSE)
# * Canada sampling ----
can_ages_lst <-
  load_can_age_data(file.path(load_dir, "can-age-data.csv"))
can_ft_num_fish <-
  read_csv(file.path(load_dir, "can-ft-num-fish-aged.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ss_num_fish <-
  read_csv(file.path(load_dir, "can-ss-num-fish-aged.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_jv_num_fish <-
  read_csv(file.path(load_dir, "can-jv-num-fish-aged.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ss_age_df <- can_ages_lst[[1]]
can_ft_age_df <- can_ages_lst[[2]]
# * US sampling ----
us_ss_age_df <-
  read_csv(file.path(load_dir, "us-shore-age-data.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_cp_age_df <-
  read_csv(file.path(load_dir, "us-cp-age-data.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_ms_age_df <-
  read_csv(file.path(load_dir, "us-ms-age-data.csv"),
           col_types = cols(),
           show_col_types = FALSE)

# Survey data ----
kriging_pars_df <-
  read_csv(file.path(load_dir, "kriging-parameters.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
survey_history_df <-
  read_csv(file.path(load_dir, "survey-history.csv"),
           col_types = cols(),
           show_col_types = FALSE)
survey_by_country_df <-
  read_csv(file.path(load_dir, "survey-by-country.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)

# Depth data ----
# * Canada depths ----
can_ft_bottom_depth_df <-
  read_csv(file.path(load_dir, "depth-can-ft-bottom.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ss_bottom_depth_df <-
  read_csv(file.path(load_dir, "depth-can-ss-bottom.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ft_gear_depth_df <-
  read_csv(file.path(load_dir, "depth-can-ft-gear.csv"),
           col_types = cols(),
           show_col_types = FALSE)
can_ss_gear_depth_df <-
  read_csv(file.path(load_dir, "depth-can-ss-gear.csv"),
           col_types = cols(),
           show_col_types = FALSE)
# * US depths ----
us_atsea_fishing_depth_df <-
  read_csv(file.path(load_dir, "depth-us-atsea-fishing.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_atsea_bottom_depth_df <-
  read_csv(file.path(load_dir, "depth-us-atsea-bottom.csv"),
           col_types = cols(),
           show_col_types = FALSE)

# At-age output data table file names ----
out_est_naa_file <- "estimated-numbers-at-age.csv"
out_est_eaa_file <- "estimated-exploitation-at-age.csv"
out_est_caa_file <- "estimated-catch-at-age.csv"
out_est_caa_bio_file <- "estimated-catch-at-age-biomass.csv"
out_est_baa_file <- "estimated-biomass-at-age.csv"


## ----source-load-models, echo = FALSE---------------------------------------------------------------
source(here::here("doc/load-document-variables/03-load-models.R"))


## ----load-custom-knitr-variables-chunk, echo = FALSE------------------------------------------------
# Put any variables you intend to use in the text here.
# The function f() is for formatting and is defined in
# r-functions/utilities.r

# -----------------------------------------------------------------------------
# The forecasting yrs and probabilities can be set to whatever is required, the
#  code is set up to automatically accommodate changes
#  Change them in all.R
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ct_levels is a list of N 3-item lists of catch levels with values:
#  1. values for catch to forecast
#  2. their pretty names to appear in the document
#  3. their directory names
# Each element of the list is a list of length equal to the
# number of elements in forecast_yrs.
# See the calc.ct.levels() and fetch.ct.levels() functions
#  is load-models.R for details. The NA's below will be populated in each model
#  by those two functions.

# -----------------------------------------------------------------------------
nf <- length(forecast_yrs)
ct_levels <-
  list(list(rep(0.01, nf), "No Fishing", "01-0"),
       list(rep(180000, nf), "180,000 t", "02-180000"),
       list(rep(225000, nf), "225,000 t", "03-225000"),
       list(rep(270000, nf), "270,000 t", "04-270000"),
       list(c(320000, 288000, 259200, 233280), "320,000 t 10% red.", "05-320000-10"),
       list(rep(325000, nf), "2022 catch: 325,000 t", "06-325000"),
       list(rep(350000, nf), "350,000 t", "07-350000"),
       list(c(350000, 315000, 283500, 255150), "350,000 t 10% red.", "08-350000-10"),
       list(rep(380000, nf), "380,000 t", "09-380000"),
       list(c(380000, 342000, 307800, 277020), "380,000 t 10% red.", "10-380000-10"),
       list(rep(430000, nf), "430,000 t", "11-430000"),
       list(rep(545000, nf), "2022 TAC: 545,000 t", "12-545000"),
       list(rep(NA, nf), "FI=100%", "13-spr-100"),
       list(rep(NA, nf), "Default Harvest Policy", "14-default-hr"),
       list(rep(NA, nf), "Stable Catch", "15-stable-catch"))

# -----------------------------------------------------------------------------
# Indices for the forecasts list, which list items above are the TAC case and
#  default policy case
# This is used in the one-page summary and a plot comparing several catch cases,
#  and elsewhere
# -----------------------------------------------------------------------------
ct_levels_num <- length(ct_levels)
ct_actual_ind <- 6
ct_tac_ind <- 12
ct_spr100_ind <- 13
ct_default_policy_ind <- 14
ct_stable_ind <- 15
ct_reduction_rows <- c(5, 8, 10)
ct_constant_rows <- c(1, 2, 3, 4, 6, 7, 9, 11, 12)
ct_constant_str <- paste(letters[ct_constant_rows], collapse = ", ")
ct_reduction_str <- paste(letters[ct_reduction_rows], collapse = ", ")

# Could probably extract automatically from bridge_models_desc[[1]][1]
ss_version <- "3.30.20"

# Credible interval -----------------------------------------------------------
cred_int <- c(0.025, 0.5, 0.975)

# Shortened names -------------------------------------------------------------
mc <- base_model$mcmccalcs
extramc <- base_model$extra_mcmc

# Attainment in the past ------------------------------------------------------
ct_last10 <- ct |>
  filter(year %in% (end_yr - 10):(end_yr - 1))
ct_last5 <- ct |>
  filter(year %in% (end_yr - 5):(end_yr - 1))
ct_last2 <- ct |>
  filter(year %in% (end_yr - 2):(end_yr - 1))
ct_last1 <- ct |>
  filter(year == end_yr - 1)
ct_secondlast <- ct |>
  filter(year == end_yr - 2)
us_last_5_yrs_attainment <- ct_last5 |>
  pull(us_attain) |>
  mean() |>
  f(1)
us_last_2_yrs_attainment <- ct_last2 |>
  pull(us_attain) |>
  mean() |>
  f(0)
can_last_5_yrs_attainment <- ct_last5 |>
  pull(can_attain) |>
  mean() |>
  f(1)
can_last_2_yrs_attainment <- ct_last2 |>
  pull(can_attain) |>
  mean() |>
  f(0)
tot_last_5_yrs_attainment <- ct_last5 |>
  pull(tot_attain) |>
  mean() |>
  f(1)
tot_last_10_yrs_attainment <- ct_last10 |>
  pull(tot_attain) |>
  mean() |>
  f(1)
tot_last_yr_attainment <- ct_last1 |>
  pull(tot_attain) |>
  mean() |>
  f(1)
tot_2015_attainment <- ct |>
  filter(year == 2015) |>
  pull(tot_attain) |>
  mean() |>
  f(1)
tot_9192_attainment <- ct |>
  filter(year %in% 1991:1992) |>
  pull(tot_attain) |>
  mean() |>
  f(0)
tot_9399_attainment <- ct |>
  filter(year %in% 1993:1999) |>
  pull(tot_attain) |>
  mean() |>
  f(0)

# Allotments ------------------------------------------------------------------
can_allotment_percent <- 26.12
us_allotment_percent <- 73.88
# ... allotments in catch -----------------------------------------------------
can_allotment_percent_last_yr <- f(pull(ct_last1, can_tac) /
                                     pull(ct_last1, tot_tac) * 100, 2)
us_allotment_percent_last_yr <- f(pull(ct_last1, us_tac) /
                                    pull(ct_last1, tot_tac) * 100, 2)
# Further TAC sources ---------------------------------------------------------
ft <- further_tac_df # (defined in inst/extdata/data/02-load-data-tables.rnw)
last_yr_us_tribal <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_tribal_quota)
last_yr_us_research <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_research_quota)
last_yr_us_non_tribal <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_nontribal_quota)
last_yr_us_tribal_quota_reallocated <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_tribal_quota_reallocated)
last_yr_us_tribal_reallocate_dates <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_tribal_reallocate_dates)
last_yr_us_tribal_reallocate_dates_f <-
  format(as.Date(as.character(last_yr_us_tribal_reallocate_dates),
                 "%d-%b"),
         "%B %d")
last_yr_us_tribal_max_landed <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_tribal_max_landed)
last_yr_us_shore_quota_reallocated <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_shore_reallocated)
last_yr_us_cp_quota_reallocated <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_cp_reallocated)
last_yr_us_ms_quota_reallocated <- ft |>
  filter(Year == last_data_yr) |>
  pull(us_ms_reallocated)

# Catch -----------------------------------------------------------------------
# ... recent catch ------------------------------------------------------------
last_5_yrs_total_ct <- ct_last5 |>
  pull(tot_catch)
long_term_avge_ct <- mean(ct$tot_catch)
ct_limit_quantiles <-
  f(as.numeric(quantile(base_model$mcmc[[paste0("ForeCatch_",
                                                end_yr)]],
                        probs = cred_int)))
names(ct_limit_quantiles) <- c("lower", "median", "upper")
# ... recent catch, last year -------------------------------------------------
last_yr_landings <- ct_last1 |>
  pull(tot_catch) |>
  f(0)
last_yr_tac <- ct_last1 |>
  pull(tot_tac) |>
  f(0)
# ... catch over the last 10 years --------------------------------------------
ct_last_10yrs <- ct |>
  slice_tail(n = 10)
ct_mean_10yrs <- f(mean(ct_last_10yrs$tot_catch))
ct_us_mean_10yrs <- f(mean(ct_last_10yrs$us_catch))
ct_can_mean_10yrs <- f(mean(ct_last_10yrs$can_catch))
# ... US Catch by fleet, last year --------------------------------------------
last_yr_us_research_ct <- ct |>
  filter(year == last_data_yr) |>
  pull(us_research_xx)
last_yr_us_cp_ct <- ct |>
  filter(year == last_data_yr) |>
  pull(us_cp_xx)
last_yr_us_ms_ct <- ct |>
  filter(year == last_data_yr) |>
  pull(us_ms_xx)
last_yr_us_shore_ct <- ct |>
  filter(year == last_data_yr) |>
  pull(us_shore_xx)
last_yr_us_ti_ct <- us_ti_ct_by_month_df |>
  filter(year == last_data_yr) |>
  pull(catch) |>
  sum()
catcher_processor_ct <- ((ct_last1 |>
                            pull(us_cp_xx)) /
                           (last_yr_us_cp_quota_reallocated) * 100) |>
  f(1)
mothership_ct <- ((ct_last1 |>
                     pull(us_ms_xx)) /
                    (last_yr_us_ms_quota_reallocated) * 100) |>
  f(1)
shore_based_ct <- ((ct_last1 |>
                      pull(us_shore_xx) -
                      last_yr_us_ti_ct) /
                     (last_yr_us_shore_quota_reallocated) * 100) |>
  f(1)

# Attainment ------------------------------------------------------------------
# ... US Attainment, catch, and TAC -------------------------------------------
last_yr_us_landings <- ct_last1 |>
  pull(us_catch) |>
  f(0)
last_yr_us_attained <- ct_last1 |>
  pull(us_attain) |>
  f(1)
last_2yr_us_attained_diff <- (ct_last1 |>
                                pull(us_attain) -
                                ct_secondlast |>
                                pull(us_attain)) |>
  f(1)
last_yr_us_tac <- ct_last1 |>
  pull(us_tac) |>
  f(0)
# ... US Attainment by fleet, last year ---------------------------------------
last_yr_us_cp_ct_percent <- f(last_yr_us_cp_ct /
                                   last_yr_us_cp_quota_reallocated * 100,
                                 1)
last_yr_us_ms_ct_percent <- f(last_yr_us_ms_ct /
                                   last_yr_us_ms_quota_reallocated * 100,
                                 1)
last_yr_us_shore_ct_percent <- f(last_yr_us_shore_ct /
                                      last_yr_us_shore_quota_reallocated * 100,
                                    1)

# ... Canada Attainment, catch, and TAC ---------------------------------------
last_yr_can_carryover <- ft |>
  filter(Year == last_data_yr) |>
  pull(can_carried_over) |>
  f(0)
last_yr_can_attained <- ct_last1 |>
  pull(can_attain) |>
  f(1)
last_2yr_can_attained_diff <- ((ct_last1 |>
                                  pull(can_attain)) -
                                 (ct_secondlast |>
                                    pull(can_attain))) |>
  f(1)
last_yr_can_landings <- ct_last1 |>
  pull(can_catch) |>
  f(0)
last_yr_can_tac <- ct_last1 |>
  pull(can_tac) |>
  f(0)
last_yr_can_tac.jv <- ft |>
  filter(Year == last_data_yr) |>
  pull(can_jv_tac) |>
  f(0)
last_yr_can_shoreside_tac <- ((ct_last1 |>
                                   pull(can_tac)) -
                                  (ft |>
                                     filter(Year == last_data_yr) |>
                                     pull(can_jv_tac))) |>
  f(0)
latest_yr_can_jv <- ct |>
  filter(can_jv_xx > 0) |>
  pull(year) |>
  max()
last_yr_can_shore <- ct_last1 |>
  pull(can_shore_xx) |>
  f(0)
last_yr_can_freezer <- ct_last1 |>
  pull(can_freeze_xx) |>
  f(0)
last_yr_can_jv <- ct_last1 |>
  pull(can_jv_xx) |>
  f(0)
last_yr_can_shore_percent <- ((ct_last1 |>
                                 pull(can_shore_xx)) /
                                (ct_last1 |>
                                   pull(can_catch)) * 100) |>
  f(1)
last_yr_can_freezer_percent <- ((ct_last1 |>
                                   pull(can_freeze_xx)) /
                                  (ct_last1 |>
                                     pull(can_catch)) * 100) |>
  f(1)
last_yr_can_jv_percent <- ((ct_last1 |>
                              pull(can_jv_xx)) /
                             (ct_last1 |>
                                pull(can_catch)) * 100) |>
  f(1)
# Years since 2000 (including 2000) that JV catch has been zero
terminal_yr_us_jv_foreign <- ct |>
  filter(us_foreign_xx > 0 | us_jv_xx > 0) %>%
  slice(nrow(.)) |>
  pull(year)
first_yr_us_atsea <- ct |>
  filter(us_cp_xx > 0 | us_ms_xx > 0) |>
  slice(1) |>
  pull(year)

# Survey values ---------------------------------------------------------------
survey_biomass <- base_model$dat$CPUE |>
  filter(index == 2) |>
  pull(var = obs, name = year) / 1e6

survey_comps <- base_model$dat$agecomp |>
  filter(FltSvy == 2)
rownames(survey_comps) <- survey_comps$Yr

survey_last_yr <- survey_comps %>%
  slice(nrow(.))

survey_last_yr_age <- survey_last_yr |>
  select(matches("^a", ignore.case = FALSE)) |>
  unlist(1) |>
  sort(decreasing = TRUE)
names(survey_last_yr_age) <- gsub("^a", "", names(survey_last_yr_age))

survey_a2_prop <- f(survey_last_yr_age[names(survey_last_yr_age) == 2], 1)
last_survey_yr <- max(as.numeric(names(survey_biomass)))
# Millions of tonnes
last_survey_yr_biomass <- base_model$dat$CPUE |>
  filter(index == 2, year == max(year)) |>
  mutate(obs = obs / 1e6) |>
  pull(obs) |>
  f(2)
penult_survey_yr <- base_model$dat$CPUE |>
  filter(index == 2) |>
  pull(year) |>
  sort() %>%
  `[`(length(.) - 1)

# How many times higher is the last survey than the one before it?
last_factor_penult <- base_model$dat$CPUE |>
  filter(index == 2) |>
  mutate(new = obs / lag(obs)) |>
  filter(year %in% c(last_survey_yr)) |>
  pull(new) |>
  f(1)

# Age-1 survey
survey_age1_yrs <- base_model$dat$CPUE |>
  filter(index == 3) |>
  pull(year)
# Spawning Biomass and Depletion estimates ------------------------------------
curr_depl_lower <- f(mc$dlower[names(mc$dlower) == end_yr] * 100, 0)
curr_depl_median <- f(mc$dmed[names(mc$dmed) == end_yr] * 100, 0)
curr_depl_upper <- f(mc$dupper[names(mc$dupper) == end_yr] * 100, 0)
# These are millions of tons:
curr_bio_lower <- f(mc$slower[names(mc$slower) == end_yr], 3)
curr_bio_median <- f(mc$smed[names(mc$smed) == end_yr], 3)
curr_bio_upper <- f(mc$supper[names(mc$supper) == end_yr], 3)
# These are metric tonnes:
curr_bio_lower_tonnes <- f(mc$slower[names(mc$slower) == end_yr] * 1e6, 0)
curr_bio_median_tonnes <- f(mc$smed[names(mc$smed) == end_yr] * 1e6, 0)
curr_bio_upper_tonnes <- f(mc$supper[names(mc$supper) == end_yr] * 1e6, 0)
# ... spawning biomass for previous year --------------------------------------
# (calculated in this assessment) in millions of tonnes and then tonnes
prev_bio_lower <- f(mc$slower[names(mc$slower) == last_data_yr], 3)
prev_bio_median <- f(mc$smed[names(mc$smed) == last_data_yr], 3)
prev_bio_upper <- f(mc$supper[names(mc$supper) == last_data_yr], 3)
prev_bio_lower_tonnes <- f(mc$slower[names(mc$slower) == last_data_yr] * 1e6, 0)
prev_bio_median_tonnes <- f(mc$smed[names(mc$smed) == last_data_yr] * 1e6, 0)

ratio_bio_median_curr_last <- mc$smed[names(mc$smed) == end_yr] /
  mc$smed[names(mc$smed) == last_data_yr]
if(ratio_bio_median_curr_last > 1){
  diff_bio_median_last_curr <-
    f((mc$smed[names(mc$smed) == end_yr] /
                mc$smed[names(mc$smed) == last_data_yr] - 1) * 100)
  diff_bio_median_last_curr_text <- "higher than"
}else{
  diff_bio_median_last_curr <-
    f((mc$smed[names(mc$smed) == end_yr] /
         mc$smed[names(mc$smed) == last_data_yr]) * 100)
  diff_bio_median_last_curr_text <- "of"
}
prev_bio_upper_tonnes <- f(mc$supper[names(mc$supper) == last_data_yr] * 1e6, 0)
# ... spawning biomass for previous year (last year's assessment) -------------
prev_bio_lower_last_assess <-
  f(last_yr_base_model$mcmccalcs$slower[names(mc$slower) == last_data_yr], 3)
prev_bio_median_last_assess <-
  f(last_yr_base_model$mcmccalcs$smed[names(mc$smed) == last_data_yr], 3)
prev_bio_upper_last_assess <-
  f(last_yr_base_model$mcmccalcs$supper[names(mc$supper) == last_data_yr], 3)

# Forecasting -----------------------------------------------------------------
# ... first forecast year depletion and spawning biomass estimates ------------
fore_tac_mcmc_yr1 <- base_model$forecasts[[1]][[ct_tac_ind]]$mcmccalcs
# ... second forecast year depletion and spawning biomass estimates -----------
fore_tac_mcmc_yr2 <- base_model$forecasts[[2]][[ct_tac_ind]]$mcmccalcs
# Biomass medians for last year's TAC catch level -----------------------------
endyr_plus_3_fore <- base_model$forecasts[[as.character(end_yr + 3)]]
endyr_plus_3_fore_tac_catch <- endyr_plus_3_fore[[ct_tac_ind]]$biomass |>
  as_tibble(rownames = "year")
last_yr_tac_fore_1_biomass <- endyr_plus_3_fore_tac_catch |>
  filter(year == end_yr) |>
  mutate(`50%` = `50%` * 100) |>
  pull(`50%`)

last_yr_tac_fore_2_biomass <- endyr_plus_3_fore_tac_catch |>
  filter(year == end_yr + 1) |>
  mutate(`50%` = `50%` * 100) |>
  pull(`50%`)

last_yr_tac_fore_3_biomass <- endyr_plus_3_fore_tac_catch |>
  filter(year == end_yr + 2) |>
  mutate(`50%` = `50%` * 100) |>
  pull(`50%`)

curr_catch_tac_value <- ct_levels[[ct_tac_ind]][[1]][1]
catch_col <- sym(paste0("ForeCatch_", end_yr))
yr_prob_col <- paste0("SSB_", end_yr + 1, "<SSB_", end_yr)
last_yr_tac_risk_1_biomass_decline <-
  base_model$risks[[as.character(end_yr)]] |>
  as_tibble() |>
  filter(!!catch_col == curr_catch_tac_value) |>
  pull(yr_prob_col) |>
  f()

catch_col <- sym(paste0("ForeCatch_", end_yr + 1))
yr_prob_col <- paste0("SSB_", end_yr + 2, "<SSB_", end_yr + 1)
last_yr_tac_risk_2_biomass_decline <-
  base_model$risks[[as.character(end_yr + 1)]] |>
  as_tibble() |>
  filter(!!catch_col == curr_catch_tac_value) |>
  pull(yr_prob_col) |>
  f()

yr_prob_col <- paste0("Bratio_", end_yr + 2, "<0.40")
last_yr_tac_risk_2_bforty <-
  base_model$risks[[as.character(end_yr + 1)]] |>
  as_tibble() |>
  filter(!!catch_col == curr_catch_tac_value) |>
  pull(yr_prob_col) |>
  f()

catch_col <- sym(paste0("ForeCatch_", end_yr + 2))
yr_prob_col <- paste0("SSB_", end_yr + 3, "<SSB_", end_yr + 2)
last_yr_tac_risk_3_biomass_decline <-
  base_model$risks[[as.character(end_yr + 2)]] |>
  as_tibble() |>
  filter(!!catch_col == curr_catch_tac_value) |>
  pull(yr_prob_col) |>
  f()

# Numbers at age calculations for bubble plot caption -------------------------
median_nat_no_yr <- extramc$natage_median |>
  select(-Yr)
# Billions of fish
max_median_nat <- f(max(median_nat_no_yr) / 1e3, 1)
yr_of_max_median_nat_ind <- which(median_nat_no_yr == max(median_nat_no_yr))
yr_of_max_median_nat <- extramc$natage_median[yr_of_max_median_nat_ind, "Yr"]

# Executive Summary and Assessment section ------------------------------------
num_mcmc_samples <- dim(base_model$mcmc)[1]
median_bio_min <- f(min(mc$smed[names(mc$smed) %in% start_yr:end_yr]), 3)
median_bio_min_yr <- names(which.min(mc$smed[names(mc$smed) %in% start_yr:end_yr]))
median_intensity <- mc$pmed
median_intensity_2007_to_2010 <- median_intensity[c("2007", "2008", "2009", "2010")]
median_intensity_2007_to_2010_min <- f(min(median_intensity_2007_to_2010) * 100, 0)
median_intensity_2007_to_2010_max <- f(max(median_intensity_2007_to_2010) * 100, 0)
median_intensity_2007_to_2011 <- median_intensity[c("2007", "2008", "2009", "2010", "2011")]
median_intensity_2007_to_2011_min <- f(min(median_intensity_2007_to_2011) * 100, 0)
median_intensity_2007_to_2011_max <- f(max(median_intensity_2007_to_2011) * 100, 0)
# Includes > end_yr
median_intensity_above_one_all_yrs <- names(which(mc$pmed > 1))
median_intensity_above_one_yrs <- median_intensity_above_one_all_yrs[
         median_intensity_above_one_all_yrs < end_yr]
median_intensity_above_1_text <- paste(
  ifelse(
    test = !length(median_intensity_above_one_all_yrs),
    "for all years",
    "except for the years "
  ),
  str_flatten(
    median_intensity_above_one_all_yrs,
    collapse = ", ",
    last = ", and "
  ),
  sep = ""
)
median_intensity_2010 <- f(mc$pmed["2010"] * 100, 1)
median_intensity_2015 <- f(mc$pmed["2015"] * 100, 1)
median_intensity_2017 <- f(mc$pmed["2017"] * 100, 1)
median_intensity_2018 <- f(mc$pmed["2018"] * 100, 1)
median_intensity_2019 <- f(mc$pmed["2019"] * 100, 1)
median_intensity_2020 <- f(mc$pmed["2020"] * 100, 1)
median_intensity_2021 <- f(mc$pmed["2021"] * 100, 1)
median_intensity_2022 <- f(mc$pmed["2022"] * 100, 1)
median_intensity_penult_yr <- f(mc$pmed[as.character(end_yr - 1)] * 100, 1)
median_relative_bio <- mc$dmed
# Remove extra non-year columns to avoid warnings below
median_relative_bio <-
  median_relative_bio[grepl("^[0-9]+$",
                            names(median_relative_bio))]
median_relative_bio <-
  median_relative_bio[names(median_relative_bio) %in% start_yr:end_yr]
median_relative_bio_2007_to_2010 <-
  median_relative_bio[c("2007", "2008", "2009", "2010")]
median_relative_bio_2007_to_2010_min <-
  f(min(median_relative_bio_2007_to_2010), 2)
median_relative_bio_2007_to_2010_max <-
  f(max(median_relative_bio_2007_to_2010), 2)
median_relative_bio_2007_to_2011 <-
  median_relative_bio[c("2007", "2008", "2009", "2010", "2011")]
median_relative_bio_2007_to_2011_min <-
  f(min(median_relative_bio_2007_to_2011), 2)
median_relative_bio_2007_to_2011_max <-
  f(max(median_relative_bio_2007_to_2011), 2)
# When below target, 0.4
median_relative_bio_below_target <-
  median_relative_bio[median_relative_bio < 0.4]
# Has been above target since
median_relative_bio_above_target_since <-
  max(as.numeric(names(median_relative_bio_below_target)), na.rm = TRUE) + 1
median_relative_bio_2017 <- f(mc$dmed["2017"] * 100, 1)

# Recruitments in current assessment vs last assessment -----------------------
prev_assess_recruitment_lower  <- last_yr_base_model$mcmccalcs$rlower
prev_assess_recruitment_med  <- last_yr_base_model$mcmccalcs$rmed
prev_assess_recruitment_upper <- last_yr_base_model$mcmccalcs$rupper

# Current assessment w/o final projection year --------------------------------
# since not in previous assessment)
compareable_names <- names(mc$rlower) %in%
  names(prev_assess_recruitment_lower)
recruitment_med_to_compare <- mc$rmed[compareable_names]

# Biomass probabilities -------------------------------------------------------
# ... biomass declines next year to year after with zero catch ----------------
zero_catch_prob_bio_down_1 <- f(base_model$risks[[1]][1, 2])
# ... biomass declines year after next to year after that with 0 catch --------
zero_catch_prob_bio_down_2 <- f(base_model$risks[[2]][1, 2])
# ... biomass declines 2 years after next to year after that with 0 catch -----
zero_catch_prob_bio_down_3 <- f(base_model$risks[[3]][1,2])
# ... current biomass being above/below B40%, B25%, and B10% ------------------
probs_curr_b40 <-
  f(mean(base_model$mcmc[[paste0("Bratio_",
                                 assess_yr)]] > 0.40) * 100,
    1)
probs_curr_b25 <-
  f(mean(base_model$mcmc[[paste0("Bratio_",
                                 assess_yr)]] > 0.25) * 100,
    1)
probs_curr_b10 <-
  f(mean(base_model$mcmc[[paste0("Bratio_",
                                 assess_yr)]] > 0.10) * 100,
    0)
probs_curr_below_b40 <-
  f(mean(base_model$mcmc[[paste0("Bratio_",
                                 assess_yr)]] < 0.40) * 100,
    1)
probs_curr_below_b25 <-
  f(mean(base_model$mcmc[[paste0("Bratio_",
                                 assess_yr)]] < 0.25) * 100,
    1)

# Reference point probabilities -----------------------------------------------
# ... reference points next year given largest catch this year ----------------
largest_next_catch_index <-
  which.max(base_model$risks[[1]][, paste0("ForeCatch_", assess_yr)])
largest_next_catch <-
  f(base_model$risks[[1]][largest_next_catch_index, paste0("ForeCatch_",
                                                           assess_yr)],
    0)
prob_next_over_b10 <-
  f(100 - as.numeric(base_model$risks[[1]][largest_next_catch_index,
                                           paste0("Bratio_",
                                                  assess_yr + 1,
                                                  "<0.10")]),
    0)
prob_next_over_b40 <-
  f(100 - as.numeric(base_model$risks[[1]][largest_next_catch_index,
                                           paste0("Bratio_",
                                                  assess_yr + 1, "<0.40")]),
    0)
# ... Canadian (DFO) provisional reference points -----------------------------
dfo_probs_curr <-
  base_model$risks[[1]][ ,(ncol(base_model$risks[[1]])-2):ncol(base_model$risks[[1]])]
dfo_probs_fore <- base_model$risks[[2]][,(ncol(base_model$risks[[2]])-2):ncol(base_model$risks[[2]])]
# ... next year DFO probs given largest catch this year -----------------------
dfo_prob_next_over_40bmsy <-
  f(dfo_probs_fore[largest_next_catch_index, paste0("SSB_",
                                                    assess_yr + 1,
                                                    ">0.4SSB_MSY")])
dfo_prob_next_over_80bmsy <-
  f(dfo_probs_fore[largest_next_catch_index, paste0("SSB_",
                                                    assess_yr + 1,
                                                    ">0.8SSB_MSY")])
dfo_prob_next_over_bmsy <-
  f(dfo_probs_fore[largest_next_catch_index, paste0("SSB_",
                                                    assess_yr + 1,
                                                    ">SSB_MSY")])
# ... US (PFMC) stock size reference points based on default Treaty HCR -------
next_treaty_catch <-
  f(base_model$catch_levels[[ct_default_policy_ind]][[1]][1], 0)
pfmc_prob_next_yr_below_b40 <-
  f(base_model$risks[[1]][ct_default_policy_ind, paste0("Bratio_",
                                                        assess_yr + 1,
                                                        "<0.40")], 0)
pfmc_prob_next_yr_below_b25 <-
  f(base_model$risks[[1]][ct_default_policy_ind, paste0("Bratio_",
                                                        assess_yr + 1,
                                                        "<0.25")], 0)
same_catch_as_last_yr <-
  f(base_model$catch_levels[[ct_actual_ind]][[1]][1], 0)
same_catch_prob_next_year_below_b40 <-
  f(base_model$risks[[1]][ct_actual_ind, paste0("Bratio_",
                                                assess_yr + 1,
                                                "<0.40")], 0)
same_catch_prob_yr_after_next_below_b40 <-
  f(base_model$risks[[2]][ct_actual_ind, paste0("Bratio_",
                                                assess_yr + 2,
                                                "<0.40")], 0)
# ... Prob most recent relative fishing intensity is above target of 1 --------
probs_curr_rel_fish_intens_above_1 <-
  f(sum(base_model$mcmc[[paste0("SPRratio_", end_yr - 1)]] > 1) /
    nrow(base_model$mcmc) * 100,
    1)
catches_below_200000_since_1986 <-
  number_to_word(length(filter(ct, tot_catch <= 200000,
                               year > 1986)$year))

# Age compositions ------------------------------------------------------------
# ... age composition data for data section -----------------------------------
survey.age.years <-
  base_model$dat$agecomp[base_model$dat$agecomp$FltSvy == 2, ]$Yr
max_fishery_age_prop <- get_age_comp_limits(base_model, type = 1)
max_survey_age_prop <- get_age_comp_limits(base_model, type = 2)
# ... Canadian Freezer trawlers age data --------------------------------------
last_year_can_ages_ft <-
  can_ages_lst[[2]][rownames(can_ages_lst[[2]]) == last_data_yr, ]
ft_age_prop_holder <- get.age.prop(last_year_can_ages_ft, 1)
max_freezer_trawler_age_prop_age <- ft_age_prop_holder[1]
max_freezer_trawler_age_prop <- f(ft_age_prop_holder[2] * 100, 1)
ft_age_prop_holder <- get.age.prop(last_year_can_ages_ft, 2)
second_freezer_trawler_age_prop_age <- ft_age_prop_holder[1]
second_freezer_trawler_age_prop <- f(ft_age_prop_holder[2] * 100, 1)
ft_age_prop_holder <- get.age.prop(last_year_can_ages_ft, 3)
third_freezer_trawler_age_prop_age <- ft_age_prop_holder[1]
third_freezer_trawler_age_prop <- f(ft_age_prop_holder[2] * 100, 1)
ft_age_prop_holder <- get.age.prop(last_year_can_ages_ft, 4)
fourth_freezer_trawler_age_prop_age <- ft_age_prop_holder[1]
fourth_freezer_trawler_age_prop <- f(ft_age_prop_holder[2] * 100, 1)
# ... Canadian Shoreside age data ---------------------------------------------
last_yr_can_ages_ss <-
  can_ages_lst[[1]][rownames(can_ages_lst[[1]]) == last_data_yr, ]
ss_age_prop_holder <- get.age.prop(last_yr_can_ages_ss, 1)
max_shoreside_age_prop_age <- ss_age_prop_holder[1]
max_shoreside_age_prop <- f(ss_age_prop_holder[2] * 100, 1)
ss_age_prop_holder <- get.age.prop(last_yr_can_ages_ss, 2)
second_shoreside_age_prop_age <- ss_age_prop_holder[1]
second_shoreside_age_prop <- f(ss_age_prop_holder[2] * 100, 1)
ss_age_prop_holder <- get.age.prop(last_yr_can_ages_ss, 3)
third_shoreside_age_prop_age <- ss_age_prop_holder[1]
third_shoreside_age_prop <- f(ss_age_prop_holder[2] * 100, 1)
ss_age_prop_holder <- get.age.prop(last_yr_can_ages_ss, 4)
fourth_shoreside_age_prop_age <- ss_age_prop_holder[1]
fourth_shoreside_age_prop <- f(ss_age_prop_holder[2] * 100, 1)
# ... US age data -------------------------------------------------------------
us_age_n_cp <- us_cp_age_df[us_cp_age_df$year == last_data_yr, "n.hauls"]
us_age_n_ms <- us_ms_age_df[us_ms_age_df$year == last_data_yr, "n.hauls"]
us_last_yr_age_cp <- us_cp_age_df[us_cp_age_df$year == last_data_yr,
                                 grep("^a",
                                      colnames(us_cp_age_df))]
us_last_yr_age_cp <-
  us_last_yr_age_cp[order(unlist(us_last_yr_age_cp[1, , drop = TRUE]),
                            decreasing = TRUE)]
us_age_1_prop_age_cp <- as.numeric(gsub("^a", "", names(us_last_yr_age_cp)[1]))
us_age_1_prop_cp <- f(us_last_yr_age_cp[1] * 100, 1)
us_age_2_prop_age_cp <- as.numeric(gsub("^a", "", names(us_last_yr_age_cp)[2]))
us_age_2_prop_cp <- f(us_last_yr_age_cp[2] * 100, 1)
us_age_3_prop_age_cp <- as.numeric(gsub("^a", "", names(us_last_yr_age_cp)[3]))
us_age_3_prop_cp <- f(us_last_yr_age_cp[3] * 100, 1)
us_age_4_prop_age_cp <- as.numeric(gsub("^a", "", names(us_last_yr_age_cp)[4]))
us_age_4_prop_cp <- f(us_last_yr_age_cp[4] * 100, 1)
us_last_yr_age_ms <- us_ms_age_df[us_ms_age_df$year == last_data_yr,
                                    grep("^a",
                                         colnames(us_ms_age_df))]
us_last_yr_age_ms <-
  us_last_yr_age_ms[order(unlist(us_last_yr_age_ms[1, , drop = TRUE]),
                            decreasing = TRUE)]
us_age_1_prop_age_ms <- as.numeric(gsub("^a", "", names(us_last_yr_age_ms)[1]))
us_age_1_prop_ms <- f(us_last_yr_age_ms[1] * 100, 1)
us_age_2_prop_age_ms <- as.numeric(gsub("^a", "", names(us_last_yr_age_ms)[2]))
us_age_2_prop_ms <- f(us_last_yr_age_ms[2] * 100, 1)
us_age_3_prop_age_ms <- as.numeric(gsub("^a", "", names(us_last_yr_age_ms)[3]))
us_age_3_prop_ms <- f(us_last_yr_age_ms[3] * 100, 1)
us_age_4_prop_age_ms <- as.numeric(gsub("^a", "", names(us_last_yr_age_ms)[4]))
us_age_4_prop_ms <- f(us_last_yr_age_ms[4] * 100, 1)
us_last_yr_age_shore <- us_ss_age_df[us_ss_age_df$year == last_data_yr,
                                       grep("^a",
                                            colnames(us_ss_age_df))]

### TODO: HERE ---

us_last_yr_age_shore <-
  us_last_yr_age_shore[order(unlist(us_last_yr_age_shore[1, , drop = TRUE]),
                             decreasing = TRUE)]
us_age_1_prop_age_shore <-
  as.numeric(gsub("^a", "", names(us_last_yr_age_shore)[1]))
us_age_1_prop_shore <-
  f(us_last_yr_age_shore[1] * 100, 1)
us_age_2_prop_age_shore <-
  as.numeric(gsub("^a", "", names(us_last_yr_age_shore)[2]))
us_age_2_prop_shore <-
  f(us_last_yr_age_shore[2] * 100, 1)
us_age_3_prop_age_shore <-
  as.numeric(gsub("^a", "", names(us_last_yr_age_shore)[3]))
us_age_3_prop_shore <-
  f(us_last_yr_age_shore[3] * 100, 1)
us_age_4_prop_age_shore <-
  as.numeric(gsub("^a", "", names(us_last_yr_age_shore)[4]))
us_age_4_prop_shore <-
  f(us_last_yr_age_shore[4] * 100, 1)

# Recruitment -----------------------------------------------------------------
#  ... years median recruitment is below the mean of the median ---------------
# recruitments for years > 2010 and < (end_yr - 1) ; end_yr - 1 won't be well
# estimated
recruitment_med_since_2010 <-
  mc$rmed[which(names(mc$rmed) %in% 2010:end_yr &
                  names(mc$rmed) %in% start_yr:(end_yr - 1))]
yrs_since_2010_recruitment_med_below_mean <- names(recruitment_med_since_2010[recruitment_med_since_2010  < mean(mc$rmed)])
# ... est, recruitment in 2014 and 2016 in billions ---------------------------
recruitment_med_in_2014 <- f(mc$rmed["2014"], 3)
last_assess_recruitment_med_in_2014 <-
  f(last_yr_base_model$mcmccalcs$rmed["2014"], 3)
prob_percent_2014_rec_gt_2010_rec <-
  f(mean(base_model$mcmc$Recr_2014 > base_model$mcmc$Recr_2010) * 100, 0)
prob_percent_2016_rec_gt_2010_rec <-
  f(mean(base_model$mcmc$Recr_2016 > base_model$mcmc$Recr_2010) * 100, 1)
prob_percent_2020_rec_gt_2010_rec <-
  f(mean(base_model$mcmc$Recr_2020 > base_model$mcmc$Recr_2010) * 100, 0)
prob_percent_2014_rec_gt_2016_rec <-
  f(mean(base_model$mcmc$Recr_2014 > base_model$mcmc$Recr_2016) * 100, 0)
recruitment_lower_in_2016 <- f(mc$rlower["2016"], 3)
recruitment_med_in_2016 <- f(mc$rmed["2016"], 3)
recruitment_upper_in_2016 <- f(mc$rupper["2016"], 3)
prob_percent_2016_rec_gt_2010_rec <-
  f(mean(base_model$mcmc$Recr_2016 > base_model$mcmc$Recr_2010) * 100, 1)
sd_med_recr_dev_estimates <-
  f(sd(mc$devmed[names(mc$devmed) >= 1970 &
                   names(mc$devmed) <= (last_data_yr - 2)]), 2)
prob_percent_2010_rec_gt_1980_rec <-
  f(mean(base_model$mcmc$Recr_2010 >
           base_model$mcmc$Recr_1980) * 100, 0)
prob_percent_2010_rec_gt_1980_rec_last_yr_assess <-
  f(mean(last_yr_base_model$mcmc$Recr_2010 >
           last_yr_base_model$mcmc$Recr_1980) * 100, 0)

# Exploitation ----------------------------------------------------------------
exploitation_med_2010 <- f(mc$fmed["2010"],2)
exploitation_med_2012 <- f(mc$fmed["2012"],2)
exploitation_med_2011 <- f(mc$fmed["2011"],2)
exploitation_med_2015 <- f(mc$fmed["2015"],2)
exploitation_med_2017 <- f(mc$fmed["2017"],2)
exploitation_med_2018 <- f(mc$fmed["2018"],2)
exploitation_med_2019 <- f(mc$fmed["2019"],2)
exploitation_med_2020 <- f(mc$fmed["2020"],2)
exploitation_med_penult_yr <- f(mc$fmed[as.character(last_data_yr)], 2)

# Priors settings from the control file ---------------------------------------
param_details <- table_param_est_bounds(base_model,
                                        start.rec.dev.yr = recruit_dev_start_yr,
                                        end.rec.dev.yr = end_yr - 1,
                                        return.xtable = FALSE)
m_prior <- split_prior_info(param_details[rownames(param_details) == "m.vals", ][4],
                            dec_points = 2,
                            first_to_lower = TRUE)
# ... Dirichlet Multinomial priors --------------------------------------------
effn_priors <- base_model$parameters |>
  as_tibble() |>
  select(Label, Prior, Pr_SD) |>
  filter(grepl("DM_theta", Label))
effn_prior <- unlist(effn_priors[1, ])
sel_phi_val <- base_model$parameters |>
  as_tibble() |>
  filter(Label == "AgeSel_P3_Fishery(1)_dev_se") |>
  pull(Value)

# Cohort specifics ------------------------------------------------------------
# ... Cohort catch ------------------------------------------------------------
cohort_catch_1999 <- sum(cohort_catch(base_model, 1999))
cohort_catch_2010 <- sum(cohort_catch(base_model, 2010))
cohort_catch_2014 <- sum(cohort_catch(base_model, 2014))
cohort_catch_2016 <- sum(cohort_catch(base_model, 2016))
cohort_catch_2017 <- sum(cohort_catch(base_model, 2017))
cohort_catch_2020 <- sum(cohort_catch(base_model, 2020))
# ... Cumulative sums of Cohorts for use in JMC presentation ------------------
cohort_cum_sum_1999 <- cumsum(cohort_catch(base_model, 1999))
cohort_cum_sum_2010 <- cumsum(cohort_catch(base_model, 2010))
cohort_cum_sum_2014 <- cumsum(cohort_catch(base_model, 2014))
cohort_cum_sum_2016 <- cumsum(cohort_catch(base_model, 2016))
cohort_cum_sum_2017 <- cumsum(cohort_catch(base_model, 2017))
cohort_cum_sum_2020 <- cumsum(cohort_catch(base_model, 2020))
ages_1999 <- as.numeric(names(cohort_cum_sum_1999)) - 1999
ages_2010 <- as.numeric(names(cohort_cum_sum_2010)) - 2010
ages_2014 <- as.numeric(names(cohort_cum_sum_2014)) - 2014
ages_2016 <- as.numeric(names(cohort_cum_sum_2016)) - 2016
ages_2017 <- as.numeric(names(cohort_cum_sum_2017)) - 2017
ages_2020 <- as.numeric(names(cohort_cum_sum_2020)) - 2020
# ... Cohort medians, credible intervals --------------------------------------
rec_2010 <- get_rec_ci(last_yr_base_model, base_model, 2010)
rec_2014 <- get_rec_ci(last_yr_base_model, base_model, 2014)
rec_2016 <- get_rec_ci(last_yr_base_model, base_model, 2016)
rec_2017 <- get_rec_ci(last_yr_base_model, base_model, 2017)
rec_2020 <- get_rec_ci(last_yr_base_model, base_model, 2020)

# Estimated prop at age (numbers) of the catch in first forecast year ---------
fore_catch_prop <-
  as.data.frame(t(as.numeric(f(apply(extramc$natsel.prop, 2, median) * 100))))
names(fore_catch_prop) <- paste0("Age", 0:20)

# Credible intervals for age5 -------------------------------------------------
# (pick the biggest cohort from fore_catch_prop; note natsel.prop columns
# start with age-0).
fore_catch_prop_age3_lower <- quantile(extramc$natsel.prop[, 4],
                                       cred_int[1]) * 100
fore_catch_prop_age3_upper <- quantile(extramc$natsel.prop[, 4],
                                       cred_int[3]) * 100
fore_catch_prop_age6_lower <- quantile(extramc$natsel.prop[, 7],
                                       cred_int[1]) * 100
fore_catch_prop_age6_upper <- quantile(extramc$natsel.prop[, 7],
                                       cred_int[3]) * 100
fore_catch_prop_age7_lower <- quantile(extramc$natsel.prop[, 8],
                                       cred_int[1]) * 100
fore_catch_prop_age7_upper <- quantile(extramc$natsel.prop[, 8],
                                       cred_int[3]) * 100

# Estimated prop at age (catch) of catch in first forecast year ---------------
fore_catch_prop_wt_age2_median <- median(extramc$natselwt.prop[, 3]) * 100
fore_catch_prop_wt_age3_median <- median(extramc$natselwt.prop[, 4]) * 100
fore_catch_prop_wt_age4_median <- median(extramc$natselwt.prop[, 5]) * 100
fore_catch_prop_wt_age5_median <- median(extramc$natselwt.prop[, 6]) * 100
fore_catch_prop_wt_age6_median <- median(extramc$natselwt.prop[, 7]) * 100
fore_catch_prop_wt_age7_median <- median(extramc$natselwt.prop[, 8]) * 100
fore_catch_prop_wt_age10_median <- median(extramc$natselwt.prop[, 11]) * 100
fore_catch_prop_wt_age11_median <- median(extramc$natselwt.prop[, 12]) * 100

# Sigma_r, standard deviation of recruitment variability ----------------------
sigma_r <- f(base_model$sigma_R_in, 2)
sigma_r_sens <- sens_models[[1]][grep("Sigma R", sens_models_names[[1]])] |>
  map_dbl("sigma_R_in") |>
  f(2)

# Range of "main" recdevs -----------------------------------------------------
recr_era_main_tmp <- base_model$recruit$era == "Main"
recr_era_early_tmp <- base_model$recruit$era == "Early"
main.recdev.start <- min(base_model$recruit$Yr[recr_era_main_tmp])
main.recdev.end <- max(base_model$recruit$Yr[recr_era_main_tmp])
main.recdev.early <- min(base_model$recruit$Yr[recr_era_early_tmp])

# Range of "main" bias adjustment period for recdevs -------------------------
biasadjust_tmp <- base_model$recruit$biasadjuster ==
  max(base_model$recruit$biasadjuster)
main.recdevbias.start <-
  min(base_model$recruit$Yr[biasadjust_tmp])
main.recdevbias.end <-
  max(base_model$recruit$Yr[biasadjust_tmp])

# Weight-at-age for the base model --------------------------------------------
wt_at_age <-
  base_model$wtatage[, !grepl("comment", colnames(base_model$wtatage))] |>
  filter(Yr %in% start_yr_age_comps:(end_yr - 1),
         Fleet == 2) |>
  select(-c(Seas, Sex, Bio_Pattern, BirthSeas, Fleet)) |>
  rename(year = Yr)

# Define number of 'recent' years for several tables --------------------------
num_recent_yrs <- 10

# Dirichlet-Multinomial data weighting parameters MLE -------------------------
log_theta_fishery <-
  round(base_model$parameters["ln(EffN_mult)_1", "Value"], 3)
log_theta_survey <-
  round(base_model$parameters["ln(EffN_mult)_2", "Value"], 3)
theta_fishery <-
  exp(base_model$parameters["ln(EffN_mult)_1", "Value"])
theta_survey <-
  exp(base_model$parameters["ln(EffN_mult)_2", "Value"])
# Approximate MLE weights
dm_weight_fishery <- round(theta_fishery / (1 + theta_fishery), 3)
dm_weight_survey <- round(theta_survey / (1 + theta_survey), 3)
# MCMC medians for the fishery and survey, and quantiles (and low and high)
col_effn <- grep("^.*\\(DM_theta\\)_Age_P1$", colnames(base_model$mcmc))
# Probably shouldn't really round these values before then using them in the
#  weight calculations. Should use f() for values to be in document not round.
#  No time to look into now (Andy).
log_theta_fishery_median <-
  round(median(base_model$mcmc[, col_effn]), 3)
log_theta_fishery_lower <-
  round(quantile(base_model$mcmc[, col_effn], probs = cred_int[1]), 3)
log_theta_fishery_upper <-
  round(quantile(base_model$mcmc[, col_effn], probs = cred_int[3]), 3)
dm_weight_fishery_median <-
  f(median(exp(base_model$mcmc[, col_effn]) /
             (1 + exp(base_model$mcmc[, col_effn]))), 3)
dm_weight_fishery_lower <-
  f(exp(log_theta_fishery_lower) /
      (1 + exp(log_theta_fishery_lower)), 3)
dm_weight_fishery_upper <-
  f(exp(log_theta_fishery_upper) /
      ( 1 + exp(log_theta_fishery_upper)), 3)
col_effn <- grep("^.*\\(DM_theta\\)_Age_P2$", colnames(base_model$mcmc))
log_theta_survey_median <-
  round(median(base_model$mcmc[, col_effn]), 3)
log_theta_survey_lower <-
  round(quantile(base_model$mcmc[, col_effn],
                 probs = cred_int[1]), 3)
log_theta_survey_upper <-
  round(quantile(base_model$mcmc[, col_effn],
                 probs = cred_int[3]), 3)
dm_weight_survey_median <-
  f(median(exp(base_model$mcmc[, col_effn]) /
             (1 + exp(base_model$mcmc[, col_effn]))), 3)
dm_weight_survey_lower <-
  f(exp(log_theta_survey_lower) /
      (1 + exp(log_theta_survey_lower)), 3)
dm_weight_survey_upper <-
  f(exp(log_theta_survey_upper) /
      (1 + exp(log_theta_survey_upper)), 3)

# MCMC parameter estimates for base model -------------------------------------
# Need to change indexing if sensitivity models order changes in model-setup.R
# ... natural mortality -------------------------------------------------------
nat_m <-
  quantile(base_model$mcmc$NatM_uniform_Fem_GP_1,
           probs = cred_int)
nat_m_02 <-
  quantile(sens_models[[1]][[6]]$mcmc$NatM_uniform_Fem_GP_1,
           probs = cred_int)
nat_m_03 <-
  quantile(sens_models[[1]][[7]]$mcmc$NatM_uniform_Fem_GP_1,
           probs = cred_int)
nat_m_hamel <-
  quantile(sens_models[[1]][[8]]$mcmc$NatM_uniform_Fem_GP_1,
           probs = cred_int)

# ... steepness ---------------------------------------------------------------
steep <-
  quantile(base_model$mcmc$SR_BH_steep,
           probs = cred_int)
steep_prior_05 <-
  quantile(sens_models[[1]][[2]]$mcmc$SR_BH_steep,
           probs = cred_int)

# ... bratio ------------------------------------------------------------------
bratio_curr <-
  quantile(base_model$mcmc[[paste0("Bratio_", assess_yr)]],
           probs = cred_int)
bratio_age1 <-
  quantile(sens_models[[2]][[2]]$mcmc[[paste0("Bratio_", assess_yr)]],
           probs = cred_int)
# ... depletion ---------------------------------------------------------------
depl_curr <- mc$dmed[names(mc$dmed) == assess_yr]
# depl_no_ageerr <- sens_models_5$mcmccalcs$dmed[names(mc$dmed) == assess_yr]
# ... joint probability -------------------------------------------------------
# (%age) of being being both above the target relative fishing intensity in \Sexpr{end_yr-1}
# and below the $\Bforty$ (40\% of $B_0$) reference point at the start of \Sexpr{end_yr}
joint_percent_prob_above_below <-
  f(sum(base_model$mcmc[[paste0("Bratio_", end_yr)]] < 0.4 &
          base_model$mcmc[[paste0("SPRratio_", end_yr - 1)]] > 1) /
      nrow(base_model$mcmc) * 100,
    1)

# Probabilities for historical performance analyses ---------------------------
historical_probs_df <-
  combine_historical_probs(model = base_model,
                           hist_probs = assess_history_probs_df,
                           end = assess_yr - 1) |>
  as_tibble()

prob_decline_from_2019_to_2020_historic <-
  historical_probs_df |>
  filter(Year == 2019) |>
  select("P_decline") |>
  as.numeric() |>
  f()

prob_decline_from_2019_to_2020_curr <-
  historical_probs_df |>
  filter(Year == 2019) |>
  select("P_decline_curr") |>
  as.numeric() |>
  f()

prob_decline_from_2012_to_2013_historic <-
  historical_probs_df |>
  filter(Year == 2012) |>
  select("P_decline") |>
  as.numeric() |>
  f()

 prob_decline_from_2012_to_2013_curr <-
   historical_probs_df |>
   filter(Year == 2012) |>
   select("P_decline_curr") |>
   as.numeric() |>
   f()

 # Retrospective setup for the document ----------------------------------------
 retro_model_nms <- c(base_model_name,
                        map_chr(plot_retro_yrs, ~{
                          paste0("-", .x, ifelse(.x == 1, " year", " years"))
                        }))
 retro_lst <- list(base_model)
 for(i in plot_retro_yrs){
   retro_lst[[i + 1]] <- base_model$retros[[i]]
 }
 retro_models_end_yr <- c(end_yr, end_yr - plot_retro_yrs)
 # Assemble the retrospective list with the base as the first element
 d_obj_retro_biomass <-
   create_group_df_biomass(retro_lst,
                           retro_model_nms,
                           end_yrs = retro_models_end_yr)
 d_obj_retro_rel_biomass <-
   create_group_df_biomass(retro_lst,
                           retro_model_nms,
                           rel = TRUE,
                           end_yrs = retro_models_end_yr)
 d_obj_retro_recr <-
   create_group_df_recr(retro_lst,
                        retro_model_nms,
                        end_yrs = retro_models_end_yr)

 # Set up bridge model groups for plotting ------------------------------------
 iter <- 0
 d_obj_bridge_biomass <- map2(bridge_models, bridge_models_names, ~{
   iter <- iter + 1
   create_group_df_biomass(.x, .y,
                           end_yrs = bridge_model_end_yr[[iter]])
 })
 iter <- 0
 d_obj_bridge_rel_biomass <- map2(bridge_models, bridge_models_names, ~{
   iter <- iter + 1
   create_group_df_biomass(.x, .y,
                           rel = TRUE,
                           end_yrs = bridge_model_end_yr[[iter]])
 })
 iter <- 0
 d_obj_bridge_recdev <- map2(bridge_models, bridge_models_names, ~{
   iter <- iter + 1
   create_group_df_recr(.x, .y,
                        devs = TRUE,
                        end_yrs = bridge_model_end_yr[[iter]])
 })
 iter <- 0
 d_obj_bridge_age1_index <- map2(bridge_models, bridge_models_names, ~{
   iter <- iter + 1
   create_group_df_index(.x, .y,
                         survey_type = "age1")
 })
 iter <- 0
 d_obj_bridge_age2_index <- map2(bridge_models, bridge_models_names, ~{
   iter <- iter + 1
   create_group_df_index(.x, .y,
                         survey_type = "age2")
 })

 # Set up sensitivity model groups for plotting -------------------------------
 # Biomass  -------------------------------------------------------------------
 d_obj_sens_biomass <- map2(sens_models, sens_models_names, ~{
   create_group_df_biomass(.x, .y)
 })
 d_obj_sens_rel_biomass <- map2(sens_models, sens_models_names, ~{
   create_group_df_biomass(.x, .y, rel = TRUE)
 })
 d_obj_sens_recr <- map2(sens_models, sens_models_names, ~{
   create_group_df_recr(.x, .y)
 })
 d_obj_sens_recdev <- map2(sens_models, sens_models_names, ~{
   create_group_df_recr(.x, .y, devs = TRUE)
 })
 # extra mcmc required for these
 d_obj_sens_age1_index_grp2 <- map2(sens_models[2], sens_models_names[2], ~{
   create_group_df_index(.x, .y, "age1")
 })
 d_obj_sens_age1_index_grp3 <- map2(sens_models[3], sens_models_names[3], ~{
   create_group_df_index(.x, .y, "age1")
 })
 d_obj_sens_age1_index_grp4 <- map2(sens_models[4], sens_models_names[4], ~{
   create_group_df_index(.x, .y, "age1")
 })
 d_obj_sens_age2_index_grp2 <- map2(sens_models[2], sens_models_names[2], ~{
   create_group_df_index(.x, .y, "age2")
 })
 d_obj_sens_age2_index_grp3 <- map2(sens_models[3], sens_models_names[3], ~{
   create_group_df_index(.x, .y, "age2")
 })
 d_obj_sens_age2_index_grp4 <- map2(sens_models[4], sens_models_names[4], ~{
   create_group_df_index(.x, .y, "age2")
 })

 # Values used in management presentation
last_yr_catch_fore <- base_model$catch_levels[[ct_actual_ind]][[1]][1]
ct_col <- paste0("ForeCatch_", forecast_yrs[1])
ct_col_sym <- sym(ct_col)
decl_col <- paste0("SSB_", forecast_yrs[2], "<SSB_", forecast_yrs[1])
decl_col_sym <- sym(decl_col)
below40_col <- paste0("Bratio_", forecast_yrs[2], "<0.40")
below40_col_sym <- sym(below40_col)
prob_decl_yr1_zero_catch <- base_model$risks[[1]] |>
  as_tibble() |>
  filter(!!ct_col_sym < 1) |>
  pull(!!decl_col_sym) |>
  f()
prob_decl_yr1_other_catch <- base_model$risks[[1]] |>
  as_tibble() |>
  slice(2) |>
  pull(!!decl_col_sym) |>
  f()
prob_below_b40_yr1_last_yr_catch <- base_model$risks[[1]] |>
  as_tibble() |>
  filter(!!ct_col_sym == last_yr_catch_fore) |>
  pull(!!below40_col) |>
  f()

ct_col <- paste0("ForeCatch_", forecast_yrs[2])
ct_col_sym <- sym(ct_col)
decl_col <- paste0("SSB_", forecast_yrs[3], "<SSB_", forecast_yrs[2])
decl_col_sym <- sym(decl_col)
below40_col <- paste0("Bratio_", forecast_yrs[3], "<0.40")
below40_col_sym <- sym(below40_col)
prob_decl_yr2_zero_catch <- base_model$risks[[2]] |>
  as_tibble() |>
  filter(!!ct_col_sym < 1) |>
  pull(!!decl_col_sym) |>
  f()
prob_decl_yr2_other_catch <- base_model$risks[[2]] |>
  as_tibble() |>
  slice(2) |>
  pull(!!decl_col_sym) |>
  f()
prob_below_b40_yr2_last_yr_catch <- base_model$risks[[2]] |>
  as_tibble() |>
  filter(!!ct_col_sym == last_yr_catch_fore) |>
  pull(!!below40_col) |>
  f()

