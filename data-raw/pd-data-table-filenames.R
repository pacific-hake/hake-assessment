# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

create_data_hake("data_tables_path", "data-tables")

# Assessment history ----
create_data_hake("assess_history_fn", "assessment-history.csv")
create_data_hake("assess_history_probs_fn", "assessment-history-probs.csv")
create_data_hake("assess_changes_fn", "assessment-changes.csv")

# Maturity and weight-at-age ----
create_data_hake("ovary_samples_fn", "ovary-samples.csv")
create_data_hake("maturity_samples_fn", "maturity-samples.csv")
create_data_hake("maturity_estimates_fn", "maturity-ogives.csv")
create_data_hake("weight_age_sample_sizes_fn","wtatage-all-samplesize.csv")
create_data_hake("weight_age_estimates_fn", "weight-at-age-ogives.csv")

# Natural mortality
create_data_hake("natural_mortality_estimates_fn", "M2.csv")
# Age proportions ----
create_data_hake("can_ft_age_props_fn", "can-ft-age-proportions.csv")
create_data_hake("can_ss_age_props_fn", "can-ss-age-proportions.csv")
create_data_hake("can_jv_age_props_fn", "can-jv-age-proportions.csv")
create_data_hake("can_raw_ft_age_counts_fn", "can-raw-ft-age-counts.csv")
create_data_hake("can_raw_ss_age_counts_fn", "can-raw-ss-age-counts.csv")
create_data_hake("can_raw_jv_age_counts_fn", "can-raw-jv-age-counts.csv")
create_data_hake("us_cp_age_props_fn", "us-cp-age-proportions.csv")
create_data_hake("us_ms_age_props_fn", "us-ms-age-proportions.csv")
create_data_hake("us_sb_age_props_fn", "us-sb-age-proportions.csv")

# Catch data ----
create_data_hake("can_ft_catch_by_month_fn", "can-ft-catch-by-month.csv")
create_data_hake("can_ss_catch_by_month_fn", "can-ss-catch-by-month.csv")
create_data_hake("can_jv_catch_by_month_fn", "can-jv-catch-by-month.csv")
create_data_hake("can_catch_by_year_fn", "can-catch-by-year.csv")
create_data_hake("us_ss_catch_by_month_fn", "us-shore-catch-by-month.csv")
create_data_hake("us_cp_catch_by_month_fn", "us-cp-catch-by-month.csv")
create_data_hake("us_ms_catch_by_month_fn", "us-ms-catch-by-month.csv")
create_data_hake("us_ti_ct_by_month_fn", "us-ti-catch-by-month.csv")
create_data_hake("us_research_catch_by_month_fn", "us-research-catch-by-month.csv")
create_data_hake("landings_tac_fn", "landings-tac-history.csv")
create_data_hake("further_tac_details_fn", "further-tac-details.csv")
create_data_hake("catch_targets_biomass_fn", "catch-targets-biomass.csv")

# Weight-at-age
create_data_hake("can_waa_fn", "can-weight-at-age.csv")
create_data_hake("us_waa_fn", "us-weight-at-age.csv")
create_data_hake("survey_waa_fn", "survey-weight-at-age.csv")

# Survey data ----
create_data_hake("kriging_pars_fn", "kriging-parameters.csv")
create_data_hake("survey_history_fn", "survey-history.csv")
create_data_hake("survey_by_country_fn", "survey-by-country.csv")

# Depth data ----
create_data_hake("can_ft_bottom_depth_fn", "depth-can-ft-bottom.csv")
create_data_hake("can_ss_bottom_depth_fn", "depth-can-ss-bottom.csv")
create_data_hake("can_ft_gear_depth_fn", "depth-can-ft-gear.csv")
create_data_hake("can_ss_gear_depth_fn", "depth-can-ss-gear.csv")
create_data_hake("us_atsea_fishing_depth_fn", "depth-us-atsea-fishing.csv")
create_data_hake("us_atsea_bottom_depth_fn", "depth-us-atsea-bottom.csv")

# Overview map data ----
create_data_hake("ports_fn", "map-data/port-locations.csv")
create_data_hake("states_fn", "map-data/state-locations.csv")

# Sample and landing data ----
create_data_hake("can_sample_dr", "/srv/hake/other/samples/canada")
create_data_hake("can_landings_dr", "/srv/hake/other/landings/canada")

create_data_hake("can_sample_data_rds_fn", "sample-data.rds")
create_data_hake("can_depths_rds_fn", "depths-fishing-events.rds")

