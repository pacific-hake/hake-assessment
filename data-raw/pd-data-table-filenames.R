# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

create_data_hake("data_tables_path", "data-tables")

create_data_hake("can_waa_fn", "can-weight-at-age.csv")

create_data_hake("can_ft_age_props_fn", "can-ft-age-proportions.csv")
create_data_hake("can_ss_age_props_fn", "can-ss-age-proportions.csv")
create_data_hake("can_jv_age_props_fn", "can-jv-age-proportions.csv")
create_data_hake("can_raw_ft_age_counts_fn", "can-raw-ft-age-counts.csv")
create_data_hake("can_raw_ss_age_counts_fn", "can-raw-ss-age-counts.csv")
create_data_hake("can_raw_jv_age_counts_fn", "can-raw-jv-age-counts.csv")

create_data_hake("us_cp_age_props_fn", "us-cp-age-proportions.csv")
create_data_hake("us_ms_age_props_fn", "us-ms-age-proportions.csv")
create_data_hake("us_sb_age_props_fn", "us-sb-age-proportions.csv")

create_data_hake("can_ft_catch_by_month_fn", "can-ft-catch-by-month.csv")
create_data_hake("can_ss_catch_by_month_fn", "can-ss-catch-by-month.csv")
create_data_hake("can_jv_catch_by_month_fn", "can-jv-catch-by-month.csv")
create_data_hake("can_catch_by_year_fn", "can-catch-by-year.csv")
create_data_hake("landings_tac_fn", "landings-tac-history.csv")
create_data_hake("further_tac_details_fn", "further-tac-details.csv")

# Canadian sample and landing data ----
create_data_hake("can_sample_dr", "/srv/hake/other/samples/canada")
create_data_hake("can_landings_dr", "/srv/hake/other/landings/canada")

create_data_hake("can_sample_data_rds_fn", "sample-data.rds")
create_data_hake("can_depths_rds_fn", "depths-fishing-events.rds")
