# Source this file to apply changes. Need load_all(). Don't forget to add documentation for
# any new package data to file R/data.R

load_dir <- here("data-tables")
# Assessment history ----
create_data_hake("assess_history_df",
                 read_csv(file.path(load_dir,
                                    "assessment-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("assess_history_probs_df",
                 read_csv(file.path(load_dir,
                                    "assessment-history-probs.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

create_data_hake("assess_changes_df",
                 read_csv(file.path(load_dir,
                                    "assessment-changes.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Maturity and weight-at-age ----
create_data_hake("ovary_samples_df",
                 read_csv(file.path(load_dir,
                                    "ovary-samples.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("maturity_samples_df",
                 read_csv(file.path(load_dir,
                                    "maturity-samples.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))

# estimates_for_assessment.rds from Eric Ward
# utils::write.csv(
#   x = readRDS(fs::path("", "srv", "hake", "other", "tv", "estimates_for_assessment.rds")) |>
#     dplyr::ungroup() |>
#     dplyr::bind_rows(
#       readRDS(fs::path("~", "Downloads", "spline.rds")) |>
#         dplyr::filter(age > 0, age < 21) |>
#         dplyr::rename(p_mature = Maturity) |>
#         dplyr::mutate(year = 2007, model = "Spline"),
#       data.frame(
#         p_mature = maturity_at_age,
#         year = 2007,
#         age = 0:(length(maturity_at_age) - 1),
#         model = "Spline w/ CAN"
#       )
#     ) |>
#     dplyr::arrange(model, year, age),
#   file = fs::path(load_dir, "maturity-ogives.csv"),
#   row.names = FALSE
# )
create_data_hake("maturity_estimates_df",
                 read_csv(file.path(load_dir,
                                    "maturity-ogives.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))

create_data_hake("weight_at_age_estimates_df",
                 read_csv(file.path(load_dir,
                                    "weight-at-age-ogives.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))
create_data_hake("weight_age_sample_sizes_df",
                 read_csv(file.path(load_dir,
                                    "wtatage-all-samplesize.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Natural mortality
create_data_hake("m_at_age_df",
                 read_csv(file.path(load_dir,"M2.csv"))
)

# Catch and TAC ----
create_data_hake("ct",
                 read_csv(file.path(load_dir,
                                    "landings-tac-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE) |>
                   mutate(`U.S. Total` =
                            `U.S. Foreign` +
                            `U.S. Joint-venture` +
                            `U.S. Mothership` +
                            `U.S. Catcher-processor` +
                            `U.S. Shoreside` +
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
                          tot_attain = `Total` / `Total TAC` * 100))

create_data_hake("catch_targets_df",
                 read_csv(file.path(load_dir,
                                    "catch-targets-biomass.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("further_tac_df",
                 read_csv(file.path(load_dir,
                                    "further-tac-details.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

create_data_hake("can_ft_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "can-ft-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_ss_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "can-ss-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_jv_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "can-jv-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# * US catch ----
create_data_hake("us_ss_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "us-shore-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_cp_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "us-cp-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_ms_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "us-ms-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_ti_ct_by_month_df",
                 read_csv(file.path(load_dir,
                                    "us-ti-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_research_catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "us-research-catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Sampling history data ----
create_data_hake("sampling_history_df",
                 read_csv(file.path(load_dir,
                                    "fishery-sampling-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# * Canada age proportions ----
create_data_hake("can_ft_age_df",
                 read_csv(file.path(load_dir,
                                             can_ft_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_ss_age_df",
                 read_csv(file.path(load_dir,
                                             can_ss_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))
#load_can_age_data
create_data_hake("can_jv_age_df",
                 read_csv(file.path(load_dir,
                                    can_jv_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

# * US age proportions ----
create_data_hake("us_cp_age_df",
                 read_csv(file.path(load_dir,
                                    us_cp_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_ms_age_df",
                 read_csv(file.path(load_dir,
                                    us_ms_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_sb_age_df",
                 read_csv(file.path(load_dir,
                                    us_sb_age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

# Survey data ----
create_data_hake("kriging_pars_df",
                 read_csv(file.path(load_dir, "kriging-parameters.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

create_data_hake("survey_history_df",
                 read_csv(file.path(load_dir,
                                    "survey-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("survey_by_country_df",
                 read_csv(file.path(load_dir,
                                    "survey-by-country.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

# Depth data ----
# * Canada depths ----
create_data_hake("can_ft_bottom_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-can-ft-bottom.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_ss_bottom_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-can-ss-bottom.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_ft_gear_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-can-ft-gear.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("can_ss_gear_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-can-ss-gear.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# * US depths ----
create_data_hake("us_atsea_fishing_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-us-atsea-fishing.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("us_atsea_bottom_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-us-atsea-bottom.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Overview map data ----
create_data_hake("ports_df",
  read_csv(file.path(load_dir,
                     "map-data",
                     "port-locations.csv"),
           col_types = cols(),
           comment = "#",

           show_col_types = FALSE))

create_data_hake("states_df",
                 read_csv(file.path(load_dir,
                                    "map-data",
                                    "state-locations.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

# Weight-at-age data
weight_at_age_df <- dplyr::bind_rows(
  utils::read.csv(
    fs::path(load_dir, can_waa_fn)
  ) |>
    dplyr::mutate(data_type = "fishery", country = "canada"),
  utils::read.csv(
    fs::path(load_dir, "us-weight-at-age.csv")
  ) |>
    dplyr::mutate(
      data_type = "fishery",
      country = "usa",
      # Set the month of acoustic poland survey data to 8 based on literature
      Month = ifelse(Source == "Acoustic Poland", 8, Month)
    ),
  utils::read.csv(
    fs::path(load_dir, "survey-weight-at-age.csv")
  ) |>
    dplyr::mutate(
      data_type = "survey",
      country = ifelse(Source == "Canada Acoustic", "canada", "usa")
    ) |>
    dplyr::filter(
      Weight_kg > 0.001
    )
) |>
  dplyr::select(-dplyr::matches("length")) |>
  dplyr::mutate(
    Sex = tidyr::replace_na(Sex, "U")
  ) |>
  dplyr::rename(weight = Weight_kg, age = Age_yrs) |>
  dplyr::rename_with(.fn = tolower)
create_data_hake("weight_at_age_df", weight_at_age_df)
