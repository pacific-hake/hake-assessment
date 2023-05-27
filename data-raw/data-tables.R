# Source this file to see the changes
load_dir <- here("data-tables")
# Assessment history ----
assess_history_df <-
  read_csv(file.path(load_dir, "assessment-history.csv"),
           col_types = cols(),
           show_col_types = FALSE)
assess_history_probs_df <-
  read_csv(file.path(load_dir, "assessment-history-probs.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
assess_history_disp_df <-
  read_csv(file.path(load_dir, "assessment-history-dispersion.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
assess_changes_df <-
  read_csv(file.path(load_dir, "assessment-changes.csv"),
           col_types = cols(),
           show_col_types = FALSE)
usethis::use_data(assess_history_df, overwrite = TRUE)
usethis::use_data(assess_history_probs_df, overwrite = TRUE)
usethis::use_data(assess_changes_df, overwrite = TRUE)
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
  read_csv(file.path(load_dir, "maturity-samples.csv"),
           guess_max = Inf,
           show_col_types = FALSE)
weight_age_sample_sizes_df <-
  read_csv(file.path(load_dir, "wtatage_all_samplesize.csv"),
           col_types = cols(),
           show_col_types = FALSE)
usethis::use_data(ovary_samples_df, overwrite = TRUE)
usethis::use_data(maturity_ogives_df, overwrite = TRUE)
usethis::use_data(maturity_samples_df, overwrite = TRUE)
usethis::use_data(weight_age_sample_sizes_df, overwrite = TRUE)

# Catch and TAC ----
ct <-
  read_csv(file.path(load_dir, "landings-tac-history.csv"),
           col_types = cols(),
           show_col_types = FALSE) |>
  mutate(`U.S. Total` =
           `U.S. Foreign` +
           `U.S. Joint-venture` +
           `U.S. Mothership` +
           `U.S. Catcher-processor` +
           `U.S. Shore-based` +
           `U.S. Research`,
         `Canada Total` =
           `Canada Foreign` +
           `Canada Joint-venture` +
           `Canada Shoreside` +
           `Canada Freezer-trawler`,
         Total = `U.S. Total` + `Canada Total`,
         us_prop = `U.S. Total` / Total * 100,
         can_prop = `Canada Total` / Total * 100,
         us_attain = `U.S. Total` / `U.S. TAC` * 100,
         can_attain = `Canada Total` / `Canada TAC` * 100,
         tot_attain = `Total` / `Total TAC` * 100)

catch_targets_df <-
  read_csv(file.path(load_dir, "catch-targets-biomass.csv"),
           col_types = cols(),
           show_col_types = FALSE)
further_tac_df <-
  read_csv(file.path(load_dir, "further-tac-details.csv"),
           col_types = cols(),
           comment = "#",
           show_col_types = FALSE)
usethis::use_data(ct, overwrite = TRUE)
usethis::use_data(catch_targets_df, overwrite = TRUE)
usethis::use_data(further_tac_df, overwrite = TRUE)
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
usethis::use_data(can_ft_catch_by_month_df, overwrite = TRUE)
usethis::use_data(can_ss_catch_by_month_df, overwrite = TRUE)
usethis::use_data(can_jv_catch_by_month_df, overwrite = TRUE)
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
usethis::use_data(us_ss_catch_by_month_df, overwrite = TRUE)
usethis::use_data(us_cp_catch_by_month_df, overwrite = TRUE)
usethis::use_data(us_ms_catch_by_month_df, overwrite = TRUE)
usethis::use_data(us_ti_ct_by_month_df, overwrite = TRUE)
usethis::use_data(us_research_catch_by_month_df, overwrite = TRUE)

# Sampling data ----
sampling_history_df <-
  read_csv(file.path(load_dir, "fishery-sampling-history.csv"),
           col_types = cols(),
           show_col_types = FALSE)
usethis::use_data(sampling_history_df, overwrite = TRUE)
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
usethis::use_data(can_ages_lst, overwrite = TRUE)
usethis::use_data(can_ft_num_fish, overwrite = TRUE)
usethis::use_data(can_ss_num_fish, overwrite = TRUE)
usethis::use_data(can_jv_num_fish, overwrite = TRUE)
usethis::use_data(can_ss_age_df, overwrite = TRUE)
usethis::use_data(can_ft_age_df, overwrite = TRUE)
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
usethis::use_data(us_ss_age_df, overwrite = TRUE)
usethis::use_data(us_cp_age_df, overwrite = TRUE)
usethis::use_data(us_ms_age_df, overwrite = TRUE)

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
usethis::use_data(kriging_pars_df, overwrite = TRUE)
usethis::use_data(survey_history_df, overwrite = TRUE)
usethis::use_data(survey_by_country_df, overwrite = TRUE)

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
usethis::use_data(can_ft_bottom_depth_df, overwrite = TRUE)
usethis::use_data(can_ss_bottom_depth_df, overwrite = TRUE)
usethis::use_data(can_ft_gear_depth_df, overwrite = TRUE)
usethis::use_data(can_ss_gear_depth_df, overwrite = TRUE)
# * US depths ----
us_atsea_fishing_depth_df <-
  read_csv(file.path(load_dir, "depth-us-atsea-fishing.csv"),
           col_types = cols(),
           show_col_types = FALSE)
us_atsea_bottom_depth_df <-
  read_csv(file.path(load_dir, "depth-us-atsea-bottom.csv"),
           col_types = cols(),
           show_col_types = FALSE)
usethis::use_data(us_atsea_fishing_depth_df, overwrite = TRUE)
usethis::use_data(us_atsea_bottom_depth_df, overwrite = TRUE)

# Overview map data ----
ports_df <-
  read_csv(file.path(load_dir, "map-data", "port-locations.csv"),
           col_types = cols(),
           show_col_types = FALSE)
states_df <-
  read_csv(file.path(load_dir, "map-data", "state-locations.csv"),
           col_types = cols(),
           show_col_types = FALSE)
usethis::use_data(ports_df , overwrite = TRUE)
usethis::use_data(states_df, overwrite = TRUE)

