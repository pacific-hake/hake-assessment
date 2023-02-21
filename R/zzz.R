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
                           dplyr.summarise.inform = FALSE,
                           xtable.comment = FALSE)
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
  "Factor", "Fleet", "FltSvy", "Iter", "Iteration", "Kind", "Label", "Lag", "Lower", "CI", "Median", "N",
  "Nsamp_adj", "Obs", "Obs_med", "Parm_StDev", "Pearson", "Pearson_lower", "Pearson_med",
  "Pearson_upper", "Probability", "Proportion", "Recr_2010", "SS_output", "SS_readctl",
  "SS_readdat", "SS_readforecast", "SS_readstarter", "SS_writectl",
  "SS_writeforecast", "SS_writestarter", "SSgetMCMC", "SSplotComparisons",
  "SSplotData", "SSsummarize", "Seas", "Sex", "StdDev", "TAC", "TACCAN", "TACUSA", "TOTAL", "Type",
  "US_JV", "US_foreign", "US_shore", "USresearch", "Upper", "CI", "Ustotal", "V1", "V2", "V3", "Value",
  "XX", "Year", "Yr", "a", "abline", "acf", "across", "aes", "after_stat", "age", "align_plots", "all_of",
  "alt_fig_text", "annotation_custom", "arrange", "arrow", "arrows", "assess_yr",
  "atSea_US_CP", "atSea_US_MS", "axis", "b", "barplot", "base_model", "base_model_name",
  "big_ticks", "bind_cols", "box", "bxp", "can_catch", "can_foreign_xx", "can_freeze_xx",
  "can_jv_xx", "can_shore_xx", "can_tac", "catch", "catch.default.policy.ind",
  "catch.levels.num", "catch_levels", "catch_levels_catch_tol",
  "catch_levels_max_iter", "catch_levels_path", "catch_levels_spr_tol", "col2rgb",
  "cols", "comma", "contains", "control_file_name", "coord_cartesian", "coord_sf", "cor",
  "data_file_name", "default_hr_path", "density", "derposts_file_name", "detectCores",
  "dev_maxyr", "dev_minyr", "devlower", "devmed", "devupper", "dlnorm", "dlower", "dmed", "dnorm",
  "draw_plot", "dupper", "element_text", "enframe", "everything", "expand_limits",
  "expansion", "facet_wrap", "first", "forecast_file_name", "forecast_probs",
  "forecast_yrs", "forecasts_path", "funs", "furrr_options", "future_imap", "future_map",
  "geom_bar", "geom_col", "geom_density", "geom_errorbar", "geom_histogram",
  "geom_hline", "geom_label", "geom_line", "geom_linerange", "geom_path", "geom_point",
  "geom_pointrange", "geom_rect", "geom_ribbon", "geom_segment", "geom_sf", "geom_text",
  "geom_text_repel", "geom_tile", "geom_vline", "get_model_executable", "get_os",
  "ggdensity", "ggdraw", "gghistogram", "ggplot", "ggplotGrob", "gray", "grid", "grid.draw",
  "grid.newpage", "group", "group_by", "group_map", "group_nest", "guide_legend", "guides",
  "hauls.with.samples", "head", "hist", "if_else", "imap", "index", "index.025", "index.975",
  "index.med", "iter", "knit", "label_value", "labs", "last_data_yr",
  "last_yr_base_model_dir_name", "lat", "layout", "left_join", "legend", "let", "lines", "lon",
  "lower", "Lower CI", "map_df", "map_dfc", "map_dfr", "matches", "matplot", "maturity.samples", "med",
  "median", "menu", "model", "model_list", "models", "mtext", "mutate_all", "n", "na.omit", "name",
  "ne_states", "north_south", "now", "object.size", "obs", "obs_2", "obs_cat:_1", "one_of", "out",
  "out_est_baa_file", "out_est_caa_bio_file", "out_est_caa_file",
  "out_est_eaa_file", "out_est_naa_file", "pairs", "par", "parameter", "pivot_longer",
  "pivot_wider", "plan", "pmap", "points", "polygon", "pos", "position_dodge",
  "posts_file_name", "prop", "qlnorm", "quant", "quantile", "rainbow", "rbeta", "read.csv",
  "read.table", "read_table2", "rect", "red", "rescale", "retrospective_yrs",
  "retrospectives_path", "rgb", "rlnorm", "rlower", "rmean", "rmed", "rnorm", "rootd_data",
  "rootd_models", "row_number", "rremove", "rsum", "runif", "rupper", "sample_admb",
  "sample_inits", "scale_alpha", "scale_color_manual", "scale_colour_gradientn",
  "scale_fill_gradientn", "scale_fill_manual", "scale_linetype_manual",
  "scale_shape_manual", "scale_size_continuous", "scale_x_continuous",
  "scale_x_discrete", "scale_y_continuous", "se_log", "se_log_2", "seas", "segments",
  "sens_model_names_1", "sens_models_1", "setNames", "show_ss_output",
  "show_ss_output", "slice", "slower", "small_ticks", "smed", "spr_100_path",
  "ss_executable", "st_as_sf", "st_coordinates", "st_crs<-", "stable_catch_path",
  "start_yr", "starter_file_name", "starts_with", "str_split", "strwidth", "summarise",
  "summarize", "summarize_all", "summarize_at", "sumprod", "supper", "survey.history",
  "symbols", "system_", "tail", "text", "text_file", "theme_half_open", "tibble", "tic", "title",
  "toc", "tot_catch", "tot_tac", "type", "ungroup", "unnest", "upper", "Upper CI", "us_catch", "us_cp_xx",
  "us_foreign_xx", "us_jv_xx", "us_ms_xx", "us_research_xx", "us_shore_xx", "us_tac",
  "value", "variable", "weight_at_age_file_name", "write.csv", "write.table",
  "write_csv", "xend", "xlab", "xtable", "y_end", "yend", "ylab", "ylim"))
