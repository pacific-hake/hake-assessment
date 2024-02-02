# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

# Paths ----
create_data_hake("doc_path", "doc")
create_data_hake("out_csv_path", "out-csv")
create_data_hake("mcmc_path", "mcmc")
create_data_hake("sso_path", "sso")
create_data_hake("ct_levels_path", "catch-levels")
create_data_hake("forecasts_path", "forecasts")
create_data_hake("forecasts_prepend", "forecast-year-")
create_data_hake("retrospectives_path", "retrospectives")
create_data_hake("retrospectives_prepend", "retro-")
create_data_hake("default_hr_path", "default-hr")
create_data_hake("stable_catch_path", "stable-catch")
create_data_hake("spr_100_path", "spr-100")
create_data_hake("ordered_decision_table_paths", c(spr_100_path,
                                                   default_hr_path,
                                                   stable_catch_path))

create_data_hake("sql_path", "sql")

# File names ----
create_data_hake("forecast_descriptions_fn", "forecast-descriptions.csv")
create_data_hake("caption_adjustments_fn", "caption-adjustments.csv")
create_data_hake("object_placement_fn", "object-placement.csv")

create_data_hake("canada_depth_sql_fn", "canada-depths.sql")
