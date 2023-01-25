# Press CTRL-SHIFT-O in Rstudio for outline view

rootd_data <- here::here("data")

message("Loading all data tables (csv files) from `", rootd_data, "`")

# Assessment history and changes ----
assessment_history_file <- "assessment-history.csv"
assessment_changes_file <- "assessment-changes.csv"
assessment_history_disp_file <- "assessment-history-SSBdispersion.csv"
assessment.history <- read.csv(file.path(rootd_data, assessment_history_file))
assessment.changes <- read.csv(file.path(rootd_data, assessment_changes_file))
assessment.history.disp <- read.csv(file.path(rootd_data, assessment_history_disp_file))

# Maturity and weight-at-age ----
ovary_samples_file <- "ovary-samples.csv"
maturity_ogives_file <- "maturity-table.csv"
maturity_samples_file <- "hake-maturity-data.csv"
weight_age_extrapolation_mask_file <- "wtatage_all_samplesize.csv"
ovary.samples <- read_csv(file.path(rootd_data, ovary_samples_file))
maturity.ogives <- read.csv(file.path(rootd_data, maturity_ogives_file))
maturity.samples <- read.csv(file.path(rootd_data, maturity_samples_file))
weight_age_extrapolation_mask <- read.csv(file.path(rootd_data, weight_age_extrapolation_mask_file))

# Catch and TAC ----
catch_targets_file <- "catch-targets-biomass.csv"
catch_data_file <- "landings-tac-history.csv"
further_tac_file <- "further-tac-details.csv"
ct <- load_catches(file.path(rootd_data, catch_data_file))
catch.targets <- read_csv(file.path(rootd_data, catch_targets_file))
further.tac <- further.tac.details(file.path(rootd_data, further_tac_file))

# * Canadian catch ----
can_ft_catch_by_month_file <- "can-ft-catch-by-month.csv"
can_ss_catch_by_month_file <- "can-ss-catch-by-month.csv"
can_jv_catch_by_month_file <- "can-jv-catch-by-month.csv"
can.ft.catch.by.month <- read.csv(file.path(rootd_data, can_ft_catch_by_month_file))
can.shore.catch.by.month <- read.csv(file.path(rootd_data, can_ss_catch_by_month_file))
can.jv.catch.by.month <- read.csv(file.path(rootd_data, can_jv_catch_by_month_file))
# * US catch ----
us_shore_catch_by_month_file <- "us-shore-catch-by-month.csv"
us_cp_catch_by_month_file <- "us-cp-catch-by-month.csv"
us_ms_catch_by_month_file <- "us-ms-catch-by-month.csv"
us_ti_catch_by_month_file <- "us-ti-catch-by-month.csv"
us_research_catch_by_month_file <- "us-research-catch-by-month.csv"
us.shore.catch.by.month <- read.csv(file.path(rootd_data, us_shore_catch_by_month_file))
us.cp.catch.by.month <- read.csv(file.path(rootd_data, us_cp_catch_by_month_file))
us.ms.catch.by.month <- read.csv(file.path(rootd_data, us_ms_catch_by_month_file))
us.ti.catch.by.month <- read.csv(file.path(rootd_data, us_ti_catch_by_month_file))
us.research.catch.by.month <- read.csv(file.path(rootd_data, us_research_catch_by_month_file))

# Sampling data ----
sampling_history_file <- "fishery-sampling-history.csv"
sampling.history <- load.sampling.history(file.path(rootd_data, sampling_history_file))
# * Canada sampling ----
can_age_file <- "can-age-data.csv"
can_ss_num_fish_file <- "can-ss-num-fish-aged.csv"
can_ft_num_fish_file <- "can-ft-num-fish-aged.csv"
can_jv_num_fish_file <- "can-jv-num-fish-aged.csv"
can.ages <- load.can.age.data(file.path(rootd_data, can_age_file))
can.ss.num.fish <- read.csv(file.path(rootd_data, can_ss_num_fish_file))
can.ft.num.fish <- read.csv(file.path(rootd_data, can_ft_num_fish_file))
can.jv.num.fish <- read.csv(file.path(rootd_data, can_jv_num_fish_file))
can.shore.age <- can.ages[[1]]
can.ft.age <- can.ages[[2]]
# * US sampling ----
us_shore_age_data_file <- "us-shore-age-data.csv"
us_cp_age_data_file <- "us-cp-age-data.csv"
us_ms_age_data_file <- "us-ms-age-data.csv"
us_atsea_bottom_depth_file <- "depth-us-atsea-bottom.csv"
us_atsea_fishing_depth_file <- "depth-us-atsea-fishing.csv"
us.shore.age <- load.us.age.data(file.path(rootd_data, us_shore_age_data_file))
us.cp.age <- load.us.age.data(file.path(rootd_data, us_cp_age_data_file))
us.ms.age <- load.us.age.data(file.path(rootd_data, us_ms_age_data_file))

# Survey data ----
survey_history_file <- "survey-history.csv"
survey_by_country_file <- "survey-by-country.csv"
kriging_parameters_file <- "kriging-parameters.csv"
kriging.pars <- read.csv(file.path(rootd_data, kriging_parameters_file), comment.char = "#")
survey.history <- load.survey.history(file.path(rootd_data, survey_history_file))
survey.by.country <- load.survey.by.country(file.path(rootd_data, survey_by_country_file))

# Depth data filenames ----
# * Canada depths ----
can_ft_bottom_depth_file <- "depth-can-ft-bottom.csv"
can_ss_bottom_depth_file <- "depth-can-ss-bottom.csv"
can_ft_gear_depth_file <- "depth-can-ft-gear.csv"
can_ss_gear_depth_file <- "depth-can-ss-gear.csv"
can.ft.bottom.depth <- read.csv(file.path(rootd_data, can_ft_bottom_depth_file))
can.ss.bottom.depth <- read.csv(file.path(rootd_data, can_ss_bottom_depth_file))
can.ft.gear.depth <- read.csv(file.path(rootd_data, can_ft_gear_depth_file))
can.ss.gear.depth <- read.csv(file.path(rootd_data, can_ss_gear_depth_file))
# * US depths ----
us_atsea_bottom_depth_file <- "depth-us-atsea-bottom.csv"
us_atsea_fishing_depth_file <- "depth-us-atsea-fishing.csv"
us.atsea.fishing.depth <- read.csv(file.path(rootd_data, us_atsea_fishing_depth_file))
us.atsea.bottom.depth <- read.csv(file.path(rootd_data, us_atsea_bottom_depth_file))

# At-age output data tables ----
out_est_naa_file <- "estimated-numbers-at-age.csv"
out_est_eaa_file <- "estimated-exploitation-at-age.csv"
out_est_caa_file <- "estimated-catch-at-age.csv"
out_est_caa_bio_file <- "estimated-catch-at-age-biomass.csv"
out_est_baa_file <- "estimated-biomass-at-age.csv"

message("All data tables have been loaded")
