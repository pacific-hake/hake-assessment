.onLoad <- function(libname, pkgname){
  # Options which will save your sanity
  # max.print - Stop tibbles from showing 1e3 and 1e6
  # warnPartialMatchDollar - Don't allow partial matches using the
  #   dollar operator
  #`dplyr.summarise.inform - Stop messages like the following:
  #  `summarize()` has grouped output by 'Fleet'. You can override using
  #   the `.groups` argument.
  .old_options <<- options(max.print = 999999,
                           warnPartialMatchDollar = TRUE,
                           dplyr.summarise.inform = FALSE)
}

.onUnload <- function(libpath){
  withr::defer(options(.old_options))
}

utils::globalVariables(
  c("%>%", ".", ".x", "1", "50%", "50\\%", "5\\%", "95\\%", ":=",
  "ABC_buffer", "Age", "Autocorrelation",
  "Beg/Mid", "Bin", "Bio_Pattern", "Bio_all", "Bio_smry", "Biomass estimate", "BirthSeas",
  "CAN_FreezeTrawl", "CAN_JV", "CAN_Shoreside", "CAN_forgn", "CANtotal", "Calc_Q", "Catch (t)",
  "Catch or F", "Cohort", "Depletion", "Era", "Exp", "Exp_lower", "Exp_med", "Exp_upper",
  "Factor", "Fleet", "Iter", "Iteration", "Kind", "Label", "Lag", "Lower", "CI", "Median", "N",
  "Nsamp_adj", "Obs", "Obs_med", "Parm_StDev", "Pearson", "Pearson_lower", "Pearson_med",
  "Pearson_upper", "Probability", "Proportion", "Recr_2010",
  "Seas", "Sex", "StdDev", "TAC", "TACCAN", "TACUSA", "TOTAL", "Type",
  "US_JV", "US_foreign", "US_shore", "USresearch", "Upper", "CI", "Ustotal", "V1", "V2", "V3", "Value",
  "XX", "Year", "Yr", "a", "abline", "acf", "across", "aes", "after_stat", "age", "align_plots", "all_of",
  "alt_fig_text", "assess_yr",
  "atSea_US_CP", "atSea_US_MS", "axis", "b", "barplot", "base_model", "base_model_name",
  "big_ticks", "box", "bxp", "can_catch", "can_foreign_xx", "can_freeze_xx",
  "can_jv_xx", "can_shore_xx", "can_tac", "catch", "ct_default_policy_ind",
  "ct_levels_num", "ct_levels", "ct_levels_catch_tol",
  "ct_levels_max_iter", "ct_levels_path", "ct_levels_spr_tol", "col2rgb",
  "cols", "comma", "control_file_name", "cor",
  "data_file_name", "default_hr_path", "density", "derposts_fn", "detectCores",
  "dev_maxyr", "dev_minyr", "devlower", "devmed", "devupper", "dlnorm", "dlower", "dmed", "dnorm",
  "draw_plot", "dupper",
  "expansion", "forecast_fn", "probs_forecast",
  "forecast_yrs", "forecasts_path",
  "hauls.with.samples", "index", "index.025", "index.975",
  "index.med", "iter", "knit", "label_value", "labs", "last_data_yr",
  "last_yr_base_model_dir_name", "lat", "let", "lon",
  "lower", "Lower CI", "med",
  "menu", "model", "model_list", "models", "n", "name",
  "ne_states", "north_south", "now", "obs", "obs_2", "obs_cat:_1", "out",
  "out_est_baa_file", "out_est_caa_bio_file", "out_est_caa_file",
  "out_est_eaa_file", "out_est_naa_file", "pairs", "parameter",
  "posts_fn", "prop", "qlnorm", "quant", "rainbow",
  "retrospective_yrs",
  "retrospectives_path", "rlower", "rmean", "rmed", "rootd_data",
  "rremove", "rsum", "rupper", "se_log", "se_log_2", "seas",
  "sens_model_names_1", "sens_models_1", "show_ss_output",
  "show_ss_output", "slower", "small_ticks", "smed", "spr_100_path",
  "ss_executable", "stable_catch_path",
  "start_yr", "starter_fn",
  "supper", "system_", "text_file", "theme_half_open",
  "toc", "tot_catch", "tot_tac", "upper", "Upper CI", "us_catch", "us_cp_xx",
  "us_foreign_xx", "us_jv_xx", "us_ms_xx", "us_research_xx", "us_shore_xx", "us_tac",
  "value", "variable", "weight_at_age_fn", "maturity_samples_df", "assess_changes_df",
  "assess_history_disp_df", "sumprod", "group", "y_end",
  "end_yr", "red", "xend", "yend", "Default HCR TAC", "Cumulative catch (kt)",
  "survey_history_df", "type", "pos", "endyrvec"))
