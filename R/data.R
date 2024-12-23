# pd-canada-areas-and-vessel-ids.R ----
#  A vector of the major areas codes used for Canadian data for offshore hake
#
#' @format A character vector
"can_major_hake_areas"
#  A data frame containing the Canadian freezer trawler names and ID numbers
#  for the catch and biological databases
#
#' @format A data frame
#' \describe{
#'   \item{vessel}{Name of the vessels that are freezer trawlers}
#'   \item{fos_id}{The FOS database ID number for the vessel}
#'   \item{gfbio_id}{The GFBIO database ID number for the vessel}
#' }
"freezer_trawlers"

# pd-messages.R
#  A warning message about how parallelism cannot be done due to inability
#  to use the OS forking mechanism
#
#' @format A character string
"parallelism_warning"

# pd-regular-expressions.R ----
#' Regular expression for the "early initial" (Early_InitAge) recruitment
#' deviation parameters in SS3 MCMC output
#'
#' @format A character string
"regex_recdev_early"
#' Regular expression for the "early", "main", and "late" recruitment
#' deviation parameters (Early_RecrDev, etc.)in SS3 MCMC output
#'
#' @format A character string
"regex_recdev_all"
#' Regular expression for the "early", "main", and "late" recruitment
#' deviation parameters (Early_RecrDev, etc.)in SS3 MCMC output. Used
#' in load_mcmc_vals.R
#'
#' @format A character string
"regex_eml_recdevs"
#' Regular expression for the "main" recruitment deviation
#' parameters in SS3 MCMC output. Used in load_mcmc_vals.R
#'
#' @format A character string
"regex_main_recdevs"
#' Regular expression for the Report files in the extra MCMC runs.
#' Used in load_extra_mcmc.R
#'
#' @format A character string
"regex_extra_mcmc_report"
#' Regular expression for the CompReport files in the extra MCMC runs.
#' Used in load_extra_mcmc.R
#'
#' @format A character string
"regex_extra_mcmc_compreport"

# pd-paths-and-filenames.R ----
#' Directory name for the main assessment document.
#'
#' @format A character string
"doc_path"
#' Directory name for outputting the CSV versions of at-age tables in the
#' document
#'
#' @format A character string
"out_csv_path"
#' Directory name for the mcmc output for models
#'
#' @format A character string
"mcmc_path"
#' Directory name for the mcmc output when MCMC_output_detail in SS3 is 2
#' which means one report file and CompReport file for each posterior
#'
#' @format A character string
"sso_path"
#' Directory name for storing the catch levels calculations.
#'
#' @format A character string
"ct_levels_path"
#' Directory name for storing the forecasting.
#'
#' @format A character string
"forecasts_path"
#' Name to be prepended to year for forecast directory names.
#'
#' @format A character string
"forecasts_prepend"
#' Directory name for storing the retrospectives.
#'
#' @format A character string
"retropectives_path"
#' Name to be prepended to year for retrospective directory names.
#'
#' @format A character string
"retrospectives_prepend"
#' Directory name for storing the default harvest rule catch catch levels
#'
#' @format A character string
"default_hr_path"
#' Directory name for storing the stable catch catch levels
#'
#' @format A character string
"stable_catch_path"
#' Directory name for storing the SPR 100 catch levels
#'
#' @format A character string
"spr_100_path"
#' An ordered vector of the paths used in the decision table for those catch
#' streams with a calculated value. Other catch streams have a numbered path
#'
#' @format A character vector
"ordered_decision_table_paths"
#' Directory name for any SQL code files
#'
#' @format A character string
"sql_path"
#' Name of file containing the forecasting descriptions and settings
#'
#' @format A character string
"forecast_descriptions_fn"
#' Name of file containing the caption adjustments for figures and tables
#'
#' @format A character string
"caption_adjustments_fn"
#' Name of file containing the object placement settings for figures and
#' tables (LaTeX placement values such as H, !b, tbh, etc.)
#'
#' @format A character string
"object_placement_fn"
#' Name of file containing the SQL code to extract Canadian fishery gear
#' depths and bottom depths
#'
#' @format A character string
"canada_depth_sql_fn"

# pd-plot-settings.R ----
#' The point shape to use for the medians in the time series plots
#'
#' @format A numerical value representing the point shape (R standard point
#' types)
"ts_pointshape"
#' The point size to use for the medians in the time series plots
#'
#' @format A numerical value
"ts_pointsize"
#' The point stroke (boldness) to use for the points representing the median
#' points in the time series plots
#'
#' @format A numerical value
"ts_pointstroke"
#' The line width to use for the line connecting the median points in the
#' time series plots
#'
#' @format A numerical value
"ts_linewidth"
#' The gap between the lines connecting the median points in the time
#' series plots
#'
#' @format A numerical value
"ts_linegap"
#' The line color to use for the line connecting the median points in the
#' time series plots, when there is only one model plotted
#'
#' @format A numerical value or R color string
"ts_single_model_linecolor"
#' The line width to use for the line connecting the median points in the
#' time series plots where there is only one model plotted
#'
#' @format A numerical value
"ts_single_model_linewidth"
#' The line type to use for the line connecting the median points in the
#' time series plots where there is only one model plotted
#'
#' @format A numerical value
"ts_single_model_linetype"
#' The point color to use for the median points in the time series plots,
#' when there is only one model plotted
#'
#' @format A numerical value or R color string
"ts_single_model_pointcolor"
#' The point shape to use for the points representing the median points in the
#' time series plots, when there is only one model plotted
#'
#' @format A numerical value
"ts_single_model_pointshape"
#' The point size to use for the points representing the median points in the
#' time series plots, when there is only one model plotted
#'
#' @format A numerical value
"ts_single_model_pointsize"
#' The point stroke (boldness) to use for the points representing the median
#' points in the time series plots, when there is only one model plotted
#'
#' @format A numerical value
"ts_single_model_pointstroke"
#' The color to use to fill time series CI ribbons for plots where there
#' is only one model plotted
#'
#' @format A character string or numerical value representing an R color
"ts_single_model_ribbon_fill"
#' The line type to use for the borders on the CI ribbons for plots where
#' there is only one model plotted
#'
#' @format A character string or numerical value representing a line type
"ts_single_model_ribbon_linetype"

#' The transparency to use by default to fill time series CI ribbons in the
#' plots in the document
#'
#' @format A numerical value between 0 and 1
"ts_ribbon_alpha"
#' The line type for the edges of the CI ribbon for time series CI ribbons
#'
#' @format A numerical value or character string representing line type for
#' [ggplot2::ggplot()]
"ts_ribbon_linetype"
#' The color to use by default for all fills in the plots in the document
#'
#' @format A character string or numerical value representing an official
#' color in R
"main_fill"
#' The transparency to use by default for all fills in the plots in the
#' document
#'
#' @format A numerical value between 0 and 1
"main_alpha"
#' The default color to use for diagonal cohort lines on age bubble plots
#'
#' @format A character string or numerical value representing an R color
"age_diag_linecolor"
#' The default line width to use for diagonal cohort lines on age bubble plots
#'
#' @format A numerical value
"age_diag_linewidth"
#' The default line type to use for diagonal cohort lines on age bubble plots
#'
#' @format A character string or numerical value representing an R line type
"age_diag_linetype"
#' The default fill color to use for bubbles in age bubble plots
#'
#' @format A character string or numerical value representing an R color
"age_fillcolor"
#' The size font to use by default for all axis titles
#'
#' @format A numerical value
"axis_title_font_size"
#' The size font to use by default for all axis tick labels
#'
#' @format A numerical value
"axis_tick_font_size"
#' The color to use by default for all axis titles
#'
#' @format A numerical value or character string representing an official
#' color in R
"axis_label_color"
#' The length of the minor tick marks in plots in cm
#'
#' @format A numerical value
"minor_tick_length"
#' The color of the horizontal line representing B0
#'
#' @format A character string or numerical value representing an R color
"refpt_bo_linecolor"
#' The color of the horizontal line representing the upper stock reference
#'
#' @format A character string or numerical value representing an R color
"refpt_usr_linecolor"
#' The color of the horizontal line representing the limit reference point
#'
#' @format A character string or numerical value representing an R color
"refpt_lrp_linecolor"
#' The default line width to use for the horizontal line representing B0
#'
#' @format A numerical value
"refpt_bo_linewidth"
#' The default line width to use for the horizontal line representing the
#' upper stock reference
#'
#' @format A numerical value
"refpt_usr_linewidth"
#' The default line width to use for the horizontal line representing the
#' limit reference point
#'
#' @format A numerical value
"refpt_lrp_linewidth"
#' The default line type to use for the horizontal line representing B0
#'
#' @format A character string or numerical value representing an R line type
"refpt_bo_linetype"
#' The default line type to use for the horizontal line representing the
#' upper stock reference
#'
#' @format A character string or numerical value representing an R line type
"refpt_usr_linetype"
#' The default line type to use for the horizontal line representing the
#' limit reference point
#'
#' @format A character string or numerical value representing an R line type
"refpt_lrp_linetype"

# pd-key-posteriors.R ----
#' A list of key posteriors for this assessment
#'
#' @format A list of regular expressions used to find the key posterior names
#' in MCMC output from SS3
"key_posteriors"
#' A list of key posterior names for this assessment
#'
#' @format A list of names for key posteriors. These are the names that
#' will be visible n the assessment document
"key_posteriors_titles"
#' A character string containing the file name for the key posteriors CSV file
#'
#' @format A character string
"key_posteriors_fn"
#' A character string containing the file name for the nuisance posteriors
#' CSV file
#' @format A character string
"nuisance_posteriors_fn"

# pd-ss-filenames.R ----
#' A character string containing the name of the SS3 executable
#' (without extension)
#'
#' @format A character string
"ss_executable"
#' A character string containing the name of the variable flag for showing
#' SS3 output messages or not
#'
#' @format A character string
"show_ss_output"
#' A character string containing the name of the covariance output file in SS3
#'
#' @format A character string
"covar_fn"
#' A character string containing the name of the file SS3 creates internally
#' as a copy of the [data_fn] input file
#'
#' @format A character string
"data_ssnew_fn"
#' A character string containing the name of the secondary file SS3 creates
#' internally as a copy of the [data_fn] input file
#'
#' @format A character string
"data_new_ssnew_fn"

#' A character string containing the file name for the SS3 starter file
#'
#' @format A character string
"starter_fn"
#' A character string containing the file name for the SS3 control file
#'
#' @format A character string
"control_fn"
#' A character string containing the file name for the SS3 data file
#'
#' @format A character string
"data_fn"
#' A character string containing the file name for the SS3 par file
#' (parameter output)
#'
#' @format A character string
"par_fn"
#' A character string containing the file name for the SS3 psv file
#' (binary ADMB output needed for the mceval step)
#'
#' @format A character string
"psv_fn"
#' A character string containing the file name for the SS3 forecast file
#'
#' @format A character string
"forecast_fn"
#' A character string containing the file name for the SS3 weight-at-age file
#'
#' @format A character string
"weight_at_age_fn"
#' A character string containing the file name for the SS3 posteriors
#' output file
#'
#' @format A character string
"posts_fn"
#' A character string containing the file name for the SS3 derived posteriors
#' output file
#'
#' @format A character string
"derposts_fn"
#' A character string containing the file name for the SS3 Report file
#' (MLE output)
#'
#' @format A character string
"report_fn"
#' A character string containing the file name for the SS3 Comp Report file
#' (MLE output)
#'
#' @format A character string
"comp_report_fn"
#' A character string containing the file name for the model output logfile
#'
#' @format A character string
"model_output_log_fn"
#' A vector of character strings containing the file names needed for inputs
#' for SS3
#'
#' @format A character string
"ss_input_files"


# pd-document-settings.R ----
#' Custom catch levels calculations. The tolerance in the SPR away from
#' 1 for the calculation of catch for SPR = 1
#'
#' @format A single decimal number
"ct_levels_spr_tol"
#' The tolerance in tonnes. The iterations will stop if the difference
#' between the projected biomass between the first and second years is
#' less than this
#'
#' @format A single decimal number
"ct_levels_catch_tol"
#' The maximum number of iterations to do. If this is reached, then no
#' catch value could be found within the tolerances above
#'
#' @format A single decimal number
"ct_levels_max_iter"
#'A vector of the probabilities to use for uncertainty calculations in the
#'assessment document
#'
#' @format A vector of three values, for the lower credible interval limit,
#' the median, and the upper credible interval limit
"probs"
#'A vector of the probabilities to use for uncertainty calculations in the
#'forecasting part of the assessment document
#'
#' @format A vector of five values, for the lower credible interval limit,
#' a lower-middle value, the median, a middle-upper value, and the upper
#' credible interval limit
"probs_forecast"
#' The years previous to the end of the time series to strip off for
#' retrospective analysis
#'
#' @format A vector of numbers
"retrospective_yrs"
#' The years previous to the end of the time series to include in plotting.
#' This value must be a subset of `retrospective_yrs`
#'
#' @format A vector of numbers
"plot_retro_yrs"
#' A toggle for showing the output from the SS model runs on the console
#'
#' @format A logical value
"plot_retro_yrs"
#' The year in which the recruitment deviations start in the model
#'
#' @format A single numerical value, a year
"recruit_dev_start_yr"
#' The year in which unfished equilibrium is set
#'
#' @format A single numerical value, a year
"unfished_eq_yr"
#' The starting year for the model estimates
#'
#' @format A single numerical value, a year
"start_yr"
#' The starting year for the age composition data in the model
#'
#' @format A single numerical value, a year
"start_yr_age_comps"
#' The starting year for the acoustic survey in the model
#'
#' @format A single numerical value, a year
"survey_start_yr"
#' The last year for the acoustic survey in the model
#'
#' @format A single numerical value, a year
"survey_end_yr"
#' A vector of years in which the acoustic survey took place and there
#' are data in the model for
#'
#' @format A single numerical value, a year
"surv_yrs"
#' A vector of years when larger-than-normal recruitment took place
#'
#' @format A vector of numerical years
"large_cohorts"
#' A vector of years when larger-than-normal recruitment took place, used
#' only for plots showing age bubbles, for drawing the diagonal cohort lines
#'
#' @format A vector of numerical years
"age_bubble_cohorts"

# pd-filesystem-settings.R ----
#' The Linux file permissions value to use for fs::file_chmod()
#'
#' @format A character string containing 3 octal digits (0-7). e.g. "777"
#' for full permissions for everybody
#'
"output_permissions"

# pd-tac-catch-proportions.R ----
#' The proportion of the U.S. TAC that the Mothership sector gets each year
#'
#' @format A numerical value less than one
"us_ms_prop_tac"
#' The proportion of the U.S. TAC that the Catcher-processor sector gets
#' each year
#'
#' @format A numerical value less than one
"us_cp_prop_tac"
#' The proportion of the U.S. TAC that the Shore-based sector gets each year
#'
#' @format A numerical value less than one
"us_ss_prop_tac"

# pd-data-table-filenames.R ----

#' The filename for the Canadian weight-at-age CSV file
#'
#' @format A character string
"can_waa_fn"
#' The directory name for the directory containing the data tables as
#' csv files
#'
#' @format A character string
"data_tables_path"
#' The filename for the Freezer trawler age proportions by year data file
#'
#' @format A character string
"can_ft_age_props_fn"
#' The filename for the Shoreside age proportions by year data file
#'
#' @format A character string
"can_ss_age_props_fn"
#' The filename for the Joint venture age proportions by year data file
#'
#' @format A character string
"can_jv_age_props_fn"
#' The filename for the Freezer trawler raw age counts by year data file
#'
#' @format A character string
"can_raw_ft_age_counts_fn"
#' The filename for the Shoreside raw age counts by year data file
#'
#' @format A character string
"can_raw_ss_age_counts_fn"
#' The filename for the Joint venture raw age counts by year data file
#'
#' @format A character string
"can_raw_jv_age_counts_fn"
#' The filename for the Catcher-processor fleet age proportions by year
#' data file
#'
#' @format A character string
"us_cp_age_props_fn"
#' The filename for the U.S. Mothership fleet age proportions by year
#' data file
#'
#' @format A character string
"us_ms_age_props_fn"
#' The filename for the U.S. Shore-based fleet  age proportions by year
#' data file
#'
#' @format A character string
"us_sb_age_props_fn"
#' The filename for the Freezer trawler catch by year and month data file
#'
#' @format A character string
"can_ft_catch_by_month_fn"
#' The filename for the Shoreside catch by year and month data file
#'
#' @format A character string
"can_ss_catch_by_month_fn"
#' The filename for the Joint venture catch by year and month data file
#'
#' @format A character string
"can_jv_catch_by_month_fn"
#' The filename for the Canadian catch by year
#'
#' @format A character string
"can_catch_by_year_fn"
#' The filename for the landings and TAC history for the US and Canada
#'
#' @format A character string
"landings_tac_fn"
#' The filename for the file containing further TAC details such as U.S.
#' tribal allocations and Canadian carryover and Joint venture TAC
#'
#' @format A character string
"further_tac_details_fn"
#' The filename for the file containing coastwide catch targets (TACs),
#' realized catch and biomass estimates
#'
#' @format A character string
"catch_targets_biomass_fn"
#' The filename for the RDS file containing hake sample data as extracted
#' using [gfdata::get_commercial_samples()]
#'
#' @format A character string
"can_sample_data_rds_fn"
# pd-data-tables.R ----
#' A data frame containing information on estimated biomass trajectories for
#' historical assessments from 1991 to present
#'
#' @format A data frame with 57 rows and 70 variables:
#' \describe{
#'   \item{yr}{Assessment year}
#'   \item{fishery_independent_cpue}{Toggle for if the index used was fishery-independent}
#'   \item{author}{Assessment authors}
#'   \item{model}{Type of model}
#'   \item{value}{Biomass estimate specifics}
#'   \item{model_type}{Type of model, eg. weight-at-age or growth}
#'   \item{1960}{Biomass value for the year. There are additional columns, one for each year}
#' }
"assess_history_df"
#' A data frame containing information on the maturity estimates by year and
#' model type. The data are coded as proportion mature by age for a given year
#' and model.
#'
#' @format A data frame with 824 rows and 4 variables:
#' \describe{
#'   \item{age}{Age of fish}
#'   \item{year}{Year sample was taken}
#'   \item{p_mature}{Proportion mature from 0 to 1}
#'   \item{model}{Type of model (name of model)}
#' }
"maturity_estimates_df"

#' A data frame containing information on the maturity estimates by year and
#' model type. The data are coded as proportion mature by age for a given year
#' and model.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{year}{Year sample was taken}
#'   \item{age}{Age of fish}
#'   \item{p_weight}{Predicted weight of fish in kg, I think}
#' }
"weight_at_age_estimates_df"

#' A data frame containing information on probability of biomass decline
#' or of being below B40% for hindcasts of the last 10 years
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{Year}{Year of probabilities}
#'   \item{P_decline}{Probability of decline from previous year}
#'   \item{P_below_B40}{Probability the biomass is below B40%}
#' }
"assess_history_probs_df"
#' A data frame containing information on changes in the assessment from
#' year to year
#'
#' @format A data frame with 16 rows and 8 variables:
#' \describe{
#'   \item{Year}{Year of assessment}
#'   \item{Framework}{Modeling framework used}
#'   \item{Survey}{Was there a new year of survey data}
#'   \item{Comp_Method}{Age-composition weighting method}
#'   \item{Comp_Fishery}{Parameter estimate/value for the fishery}
#'   \item{Comp_Survey}{Parameter estimate/value for the survey}
#'   \item{MCMC}{Number of MCMC samples used for all results}
#'   \item{Change}{Description of tchanges made in thee assessment year}
#' }
"assess_changes_df"
#' A data frame containing information on ovary sampling
#'
#' @format A data frame with 12 rows and 8 variables:
#' \describe{
#'   \item{Year}{Year of samples}
#'   \item{NWFSC Trawl Survey}{Number of ovary samples}
#'   \item{CAN Acoustic Survey/Research (Summer)}{Number of ovary samples}
#'   \item{U.S. Acoustic Survey/Research (Summer)}{Number of ovary samples}
#'   \item{U.S. Acoustic Survey/Research (Winter)}{Number of ovary samples}
#'   \item{U.S. At-Sea Hake Observer Program (Spring)}{Number of ovary samples}
#'   \item{U.S. At-Sea Hake Observer Program (Fall)}{Number of ovary samples}
#'   \item{OR Dept. Fish & Wildlife}{Number of ovary samples}
#' }
"ovary_samples_df"
#' A data frame containing information on ovary sampling
#'
#' @format A data frame with 2636 rows and 33 variables:
#' \describe{
#'   \item{sampling_platform}{Group that performed the sampling}
#'   \item{yr}{Year of sample}
#'   \item{primary_reader}{Initials of the primary sample-taker}
#'   \item{haul_date}{Date of the haul (tow) that the sample was from}
#'   \item{month}{Month of the sample}
#'   \item{rounded_latitude}{Latitude of sample, rounded to degree}
#'   \item{n_or_s_of_34_44}{Sample was North or South of 34.44 degrees North}
#'   \item{season}{Season of sample}
#'   \item{visual_maturity_code}{Maturity code for sample taken by visual means only}
#'   \item{weight_kg}{Weight of fish}
#'   \item{length_cm}{Length in centimeters of fish}
#'   \item{age}{Age of fish or NA if not aged}
#'   \item{certainty}{Certain or uncertain of maturity assignment}
#'   \item{o_1}{Oocyte markers}
#'   \item{o_2}{Oocyte markers}
#'   \item{o_4_1}{Oocyte markers}
#'   \item{o_4_2}{Oocyte markers}
#'   \item{o_5}{Oocyte markers}
#'   \item{o_6}{Oocyte markers}
#'   \item{o_7}{Oocyte markers}
#'   \item{o_8}{Oocyte markers}
#'   \item{o_9}{Oocyte markers}
#'   \item{highest_matstage}{Highest maturity stage assigned}
#'   \item{biological_maturity}{0 for not mature and 1 for mature}
#'   \item{prop_atresia}{Proportion for atresia (closedness)}
#'   \item{functional_maturity}{0 for not mature and 1 for mature}
#'   \item{spent}{Toggle for spent ovary}
#'   \item{batch_spent}{0 for not spent, 1 for spent}
#'   \item{spawning}{Toggle for spawning fish or non-spawning fish}
#'   \item{pofs_present}{0 for not present, 1 for present}
#'   \item{histology_notes}{Notes taken during histilogical analysis}
#'   \item{data_notes}{Notes taken during sample analysis}
#' }
"maturity_samples_df"
#' A data frame containing information on weight-at-age
#'
#' @format A data frame with 49 rows and 22 variables:
#' \describe{
#'   \item{Yr}{Year of samples}
#'   \item{seas}{Season code (SS3)}
#'   \item{gender}{Gender code (SS3)}
#'   \item{GP}{Unknown}
#'   \item{bseas}{Unknown}
#'   \item{Fleet}{Fishery fleet (SS3)}
#'   \item{a0}{Number of age 0 fish sampled}
#'   \item{a1}{Number of age 1 fish sampled}
#'   \item{a2}{Number of age 2 fish sampled}
#'   \item{a3}{Number of age 3 fish sampled}
#'   \item{a4}{Number of age 4 fish sampled}
#'   \item{a5}{Number of age 5 fish sampled}
#'   \item{a6}{Number of age 6 fish sampled}
#'   \item{a7}{Number of age 7 fish sampled}
#'   \item{a8}{Number of age 8 fish sampled}
#'   \item{a9}{Number of age 9 fish sampled}
#'   \item{a10}{Number of age 10 fish sampled}
#'   \item{a11}{Number of age 11 fish sampled}
#'   \item{a12}{Number of age 12 fish sampled}
#'   \item{a13}{Number of age 13 fish sampled}
#'   \item{a14}{Number of age 14 fish sampled}
#'   \item{a15}{Number of age 15 fish sampled}
#' }
"weight_age_sample_sizes_df"
#' A data frame containing estimates of maturity at age
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{age}{Age (years) of fish}
#'   \item{year}{A four digit integer representing the year}
#'   \item{p_mature}{The probability a fish is mature}
#'   \item{model}{A string defining which model the probability came from}
#' }
"maturity_estimates_df"
#' A data frame containing information on catch by year and fishery (fleet)
#'
#' @format A data frame with 57 rows and 22 variables:
#' \describe{
#'   \item{Year}{Year of catch}
#'   \item{U.S. Foreign}{Catch in tonnes}
#'   \item{U.S. Joint-venture}{Catch in tonnes}
#'   \item{U.S. Mothership}{Catch in tonnes}
#'   \item{U.S. Catcher-processor}{Catch in tonnes}
#'   \item{U.S. Shore-based}{Catch in tonnes}
#'   \item{U.S. Research}{Catch in tonnes}
#'   \item{Canada Foreign}{Catch in tonnes}
#'   \item{Canada Joint-venture}{Catch in tonnes}
#'   \item{Canada Shoreside}{Catch in tonnes}
#'   \item{Canada Freezer-trawler}{Catch in tonnes}
#'   \item{U.S. TAC}{Total Allowable catch in tonnes}
#'   \item{Canada TAC}{Total Allowable catch in tonnes}
#'   \item{Total TAC}{Total Allowable catch in tonnes}
#'   \item{U.S. Total}{Total catch in tonnes for the U.S. each year}
#'   \item{Canada Total}{Total catch in tonnes for Canada each year}
#'   \item{us_prop}{U.S. proprtion of catch taken each year}
#'   \item{can_prop}{Canadian proprtion of catch taken each year}
#'   \item{us_attain}{U.S. proportion of U.S. TAC caught each year}
#'   \item{can_attain}{Canadian proportion of Canadian TAC caught each year}
#'   \item{tot_attain}{Proportion of total TAC caught each year}
#' }
"ct"
#' A data frame containing information on the actual catch, biomass estimate
#' and Total Allowable Catch by year
#'
#' @format A data frame with 21 rows and 6 variables:
#' \describe{
#'   \item{Year}{Year of catch/TAC}
#'   \item{Realized catch}{Actual catch take for the year in tonnes}
#'   \item{TAC}{Total allowable catch for the year}
#'   \item{Biomass estimate}{The biomass estimate for the year from the current assessment}
#'   \item{Depletion}{The depletion estimate for the year from the current assessment}
#'   \item{Default HCR TAC}{The default harvest control rule TAC (F40%)}
#' }
"catch_targets_df"
#' A data frame containing information on extra TAC information
#'
#' @format A data frame with 9 rows and 12 variables:
#' \describe{
#'   \item{Year}{Year of catch/TAC}
#'   \item{us_tribal_quota}{U.S. tribal quota in tonnes}
#'   \item{us_research_quota}{U.S. research quota in tonnes}
#'   \item{us_nontribal_quota}{U.S. non-tribal quota in tonnes}
#'   \item{us_tribal_quota_reallocated}{U.S. tribal quota reallocated in tonnes}
#'   \item{us_tribal_reallocate_dates}{U.S. tribal quota reallocation dates}
#'   \item{us_tribal_max_landed}{U.S. tribal maximum landed catch in tonnes}
#'   \item{can_carried_over}{Canadian carry-over value in tonnes}
#'   \item{us_shore_reallocated}{U.S. shore-based reallocation in tonnes}
#'   \item{us_cp_reallocated}{U.S. catcher-processor reallocation in tonnes}
#'   \item{us_ms_reallocated}{U.S. mothership reallocation in tonnes}
#'   \item{can_jv_tac}{Canadian joint-venture allocation/reallocation in tonnes}
#' }
"further_tac_df"
#' A data frame containing catch information on Canadian Freezer trawlers
#' by month and year
#'
#' @format A data frame with 16 rows and 13 variables:
#' \describe{
#'   \item{Year}{Year of samples}
#'   \item{1}{January catch in tonnes}
#'   \item{2}{February catch in tonnes}
#'   \item{3}{March catch in tonnes}
#'   \item{4}{April catch in tonnes}
#'   \item{5}{May catch in tonnes}
#'   \item{6}{June catch in tonnes}
#'   \item{7}{July catch in tonnes}
#'   \item{8}{August catch in tonnes}
#'   \item{9}{September catch in tonnes}
#'   \item{10}{October catch in tonnes}
#'   \item{11}{November catch in tonnes}
#'   \item{12}{December catch in tonnes}
#' }
"can_ft_catch_by_month_df"
#' A data frame containing catch information on Canadian Shoreside vessels
#' by month and year
#'
#' @format A data frame with 16 rows and 13 variables:
#' \describe{
#'   \item{Year}{Year of samples}
#'   \item{1}{January catch in tonnes}
#'   \item{2}{February catch in tonnes}
#'   \item{3}{March catch in tonnes}
#'   \item{4}{April catch in tonnes}
#'   \item{5}{May catch in tonnes}
#'   \item{6}{June catch in tonnes}
#'   \item{7}{July catch in tonnes}
#'   \item{8}{August catch in tonnes}
#'   \item{9}{September catch in tonnes}
#'   \item{10}{October catch in tonnes}
#'   \item{11}{November catch in tonnes}
#'   \item{12}{December catch in tonnes}
#' }
"can_ss_catch_by_month_df"
#' A data frame containing catch information on Canadian Joint-venture
#' vessels by month# and year
#'
#' @format A data frame with 6 rows and 13 variables:
#' \describe{
#'   \item{Year}{Year of samples}
#'   \item{1}{January catch in tonnes}
#'   \item{2}{February catch in tonnes}
#'   \item{3}{March catch in tonnes}
#'   \item{4}{April catch in tonnes}
#'   \item{5}{May catch in tonnes}
#'   \item{6}{June catch in tonnes}
#'   \item{7}{July catch in tonnes}
#'   \item{8}{August catch in tonnes}
#'   \item{9}{September catch in tonnes}
#'   \item{10}{October catch in tonnes}
#'   \item{11}{November catch in tonnes}
#'   \item{12}{December catch in tonnes}
#' }
"can_jv_catch_by_month_df"
#' A data frame containing catch information on U.S. shore-based vessels
#' by month and year
#'
#' @format A data frame with 484 rows and 3 variables:
#' \describe{
#'   \item{month}{Month of catch}
#'   \item{year}{Year of catch}
#'   \item{catch}{Catch in tonnes}
#' }
"us_ss_catch_by_month_df"
#' A data frame containing catch information on U.S. catcher-processor vessels
#' by month and year
#'
#' @format A data frame with 484 rows and 3 variables:
#' \describe{
#'   \item{month}{Month of catch}
#'   \item{year}{Year of catch}
#'   \item{catch}{Catch in tonnes}
#' }
"us_cp_catch_by_month_df"
#' A data frame containing catch information on U.S. ti vessels
#' by month and year
#'
#' @format A data frame with 85 rows and 3 variables:
#' \describe{
#'   \item{month}{Month of catch}
#'   \item{year}{Year of catch}
#'   \item{catch}{Catch in tonnes}
#' }
"us_ms_catch_by_month_df"
#' A data frame containing catch information on U.S. tribal vessels
#' by month and year
#'
#' @format A data frame with 88 rows and 3 variables:
#' \describe{2
#'   \item{month}{Month of catch}
#'   \item{year}{Year of catch}
#'   \item{catch}{Catch in tonnes}
#' }
"us_ti_ct_by_month_df"
#' A data frame containing catch information on U.S. research vessels
#' by month and year
#'
#' @format A data frame with 16 rows and 3 variables:
#' \describe{2
#'   \item{month}{Month of catch}
#'   \item{year}{Year of catch}
#'   \item{catch}{Catch in tonnes}
#' }
"us_research_catch_by_month_df"
#' A data frame containing information on amount of sampling done by year
#'
#' @format A data frame with 48 rows and 11 variables:
#' \describe{
#'   \item{Year}{Year of samples}
#'   \item{U.S. Foreign (hauls)}{Number of samples}
#'   \item{U.S. Joint-venture (hauls)}{Number of samples}
#'   \item{U.S. Mothership (hauls)}{Number of samples}
#'   \item{U.S. Combined Mothership Catcher-processor (hauls)}{Number of samples}
#'   \item{U.S. Catcher-processor (hauls)}{Number of samples}
#'   \item{U.S. Shore-based (trips)}{Number of samples}
#'   \item{Canada Foreign (hauls)}{Number of samples}
#'   \item{Canada Joint-venture (hauls)}{Number of samples}
#'   \item{Canada Shoreside (trips)}{Number of samples}
#'   \item{Canada Freezer trawlers (hauls)}{Number of samples}
#' }
"sampling_history_df"
#' A data frame containing information about the number of fish sampled for
#' age by year for the Freezer trawlers in Canada
#'
#' @format A data frame with 17 rows and 2 variables:
#' \describe{
#'   \item{year}{Year of sample}
#'   \item{num_fish}{The number of fish sampled for each year}
#' }
"can_ft_num_fish"
#' A data frame containing information about the number of fish sampled for
#' age by year for the Shoreside vessels in Canada
#'
#' @format A data frame with 28 rows and 2 variables:
#' \describe{
#'   \item{year}{Year of sample}
#'   \item{num_fish}{The number of fish sampled for each year}
#' }
"can_ss_num_fish"
#' A data frame containing information about the number of fish sampled for
#' age by year for the Joint-venture vessels in Canada
#'
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{year}{Year of sample}
#'   \item{num_fish}{The number of fish sampled for each year}
#' }
"can_jv_num_fish"
#' A data frame containing information about the proportions  of fish
#' by age and year for the Shoreside vessels in Canada
#'
#' @format A data frame with 28 rows and 15 variables:
#' \describe{
#'   \item{1}{Proportion of age 1 for the given year (rowname)}
#'   \item{2}{Proportion of age 2 for the given year (rowname)}
#'   \item{3}{Proportion of age 3 for the given year (rowname)}
#'   \item{4}{Proportion of age 4 for the given year (rowname)}
#'   \item{5}{Proportion of age 5 for the given year (rowname)}
#'   \item{6}{Proportion of age 6 for the given year (rowname)}
#'   \item{7}{Proportion of age 7 for the given year (rowname)}
#'   \item{8}{Proportion of age 8 for the given year (rowname)}
#'   \item{9}{Proportion of age 9 for the given year (rowname)}
#'   \item{10}{Proportion of age 10 for the given year (rowname)}
#'   \item{11}{Proportion of age 11 for the given year (rowname)}
#'   \item{12}{Proportion of age 12 for the given year (rowname)}
#'   \item{13}{Proportion of age 13 for the given year (rowname)}
#'   \item{14}{Proportion of age 14 for the given year (rowname)}
#'   \item{15}{Proportion of age 15 for the given year (rowname)}
#' }
"can_ss_age_df"
#' A data frame containing information about the proportions  of fish
#' by age and year for the Freezer trawlers in Canada
#'
#' @format A data frame with 17 rows and 15 variables:
#' \describe{
#'   \item{1}{Proportion of age 1 for the given year (rowname)}
#'   \item{2}{Proportion of age 2 for the given year (rowname)}
#'   \item{3}{Proportion of age 3 for the given year (rowname)}
#'   \item{4}{Proportion of age 4 for the given year (rowname)}
#'   \item{5}{Proportion of age 5 for the given year (rowname)}
#'   \item{6}{Proportion of age 6 for the given year (rowname)}
#'   \item{7}{Proportion of age 7 for the given year (rowname)}
#'   \item{8}{Proportion of age 8 for the given year (rowname)}
#'   \item{9}{Proportion of age 9 for the given year (rowname)}
#'   \item{10}{Proportion of age 10 for the given year (rowname)}
#'   \item{11}{Proportion of age 11 for the given year (rowname)}
#'   \item{12}{Proportion of age 12 for the given year (rowname)}
#'   \item{13}{Proportion of age 13 for the given year (rowname)}
#'   \item{14}{Proportion of age 14 for the given year (rowname)}
#'   \item{15}{Proportion of age 15 for the given year (rowname)}
#' }
"can_ft_age_df"
#' A data frame containing information about the proportions  of fish
#' by age and year for the Shore-based vessels in the U.S.
#'
#' @format A data frame with 15 rows and 18 variables:
#' \describe{
#'   \item{year}{Year of age proportions}
#'   \item{n.fish}{Number of fish sampled for each year}
#'   \item{n.trips}{Number of trips samples came from for each year}
#'   \item{1}{Proportion of age 1 for the given year (rowname)}
#'   \item{2}{Proportion of age 2 for the given year (rowname)}
#'   \item{3}{Proportion of age 3 for the given year (rowname)}
#'   \item{4}{Proportion of age 4 for the given year (rowname)}
#'   \item{5}{Proportion of age 5 for the given year (rowname)}
#'   \item{6}{Proportion of age 6 for the given year (rowname)}
#'   \item{7}{Proportion of age 7 for the given year (rowname)}
#'   \item{8}{Proportion of age 8 for the given year (rowname)}
#'   \item{9}{Proportion of age 9 for the given year (rowname)}
#'   \item{10}{Proportion of age 10 for the given year (rowname)}
#'   \item{11}{Proportion of age 11 for the given year (rowname)}
#'   \item{12}{Proportion of age 12 for the given year (rowname)}
#'   \item{13}{Proportion of age 13 for the given year (rowname)}
#'   \item{14}{Proportion of age 14 for the given year (rowname)}
#'   \item{15}{Proportion of age 15 for the given year (rowname)}
#' }
"us_ss_age_df"
#' A data frame containing information about the proportions  of fish
#' by age and year for the catcher-processor vessels in the U.S.
#'
#' @format A data frame with 15 rows and 18 variables:
#' \describe{
#'   \item{year}{Year of age proportions}
#'   \item{n.fish}{Number of fish sampled for each year}
#'   \item{n.hauls}{Number of hauls samples came from for each year}
#'   \item{1}{Proportion of age 1 for the given year (rowname)}
#'   \item{2}{Proportion of age 2 for the given year (rowname)}
#'   \item{3}{Proportion of age 3 for the given year (rowname)}
#'   \item{4}{Proportion of age 4 for the given year (rowname)}
#'   \item{5}{Proportion of age 5 for the given year (rowname)}
#'   \item{6}{Proportion of age 6 for the given year (rowname)}
#'   \item{7}{Proportion of age 7 for the given year (rowname)}
#'   \item{8}{Proportion of age 8 for the given year (rowname)}
#'   \item{9}{Proportion of age 9 for the given year (rowname)}
#'   \item{10}{Proportion of age 10 for the given year (rowname)}
#'   \item{11}{Proportion of age 11 for the given year (rowname)}
#'   \item{12}{Proportion of age 12 for the given year (rowname)}
#'   \item{13}{Proportion of age 13 for the given year (rowname)}
#'   \item{14}{Proportion of age 14 for the given year (rowname)}
#'   \item{15}{Proportion of age 15 for the given year (rowname)}
#' }
"us_cp_age_df"
#' A data frame containing information about the proportions  of fish
#' by age and year for the mothership vessels in the U.S.
#'
#' @format A data frame with 15 rows and 18 variables:
#' \describe{
#'   \item{year}{Year of age proportions}
#'   \item{n.fish}{Number of fish sampled for each year}
#'   \item{n.hauls}{Number of hauls samples came from for each year}
#'   \item{1}{Proportion of age 1 for the given year (rowname)}
#'   \item{2}{Proportion of age 2 for the given year (rowname)}
#'   \item{3}{Proportion of age 3 for the given year (rowname)}
#'   \item{4}{Proportion of age 4 for the given year (rowname)}
#'   \item{5}{Proportion of age 5 for the given year (rowname)}
#'   \item{6}{Proportion of age 6 for the given year (rowname)}
#'   \item{7}{Proportion of age 7 for the given year (rowname)}
#'   \item{8}{Proportion of age 8 for the given year (rowname)}
#'   \item{9}{Proportion of age 9 for the given year (rowname)}
#'   \item{10}{Proportion of age 10 for the given year (rowname)}
#'   \item{11}{Proportion of age 11 for the given year (rowname)}
#'   \item{12}{Proportion of age 12 for the given year (rowname)}
#'   \item{13}{Proportion of age 13 for the given year (rowname)}
#'   \item{14}{Proportion of age 14 for the given year (rowname)}
#'   \item{15}{Proportion of age 15 for the given year (rowname)}
#' }
"us_ms_age_df"
#' A data frame containing information about the Kriging extrapolation
#' performed on the survey data
#'
#' @format A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{Year}{Year of data}
#'   \item{SearchRadius}{The radius used in the kriging extrapolation}
#'   \item{kmin}{Kringing parameter}
#'   \item{kmax}{Kringing parameter}
#' }
"kriging_pars_df"
#' A data frame containing information about the history of all acoustic
#' surveys done
#'
#' @format A data frame with 14 rows and 5 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{start.date}{Start date of survey}
#'   \item{end.date}{End date of survey}
#'   \item{vessels}{A list of the vessels involved in the survey}
#'   \item{hauls.with.samples}{The number of hauls with samples taken}
#' }
"survey_history_df"
#' A data frame containing information about the depths of the bottom
#' for catch by year for Canadian Freezer trawlers
#'
#' @format A data frame with 16 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"can_ft_bottom_depth_df"
#' A data frame containing information about the depths of the bottom
#' for catch by year for Canadian Shoreside vessels
#'
#' @format A data frame with 16 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"can_ss_bottom_depth_df"
#' A data frame containing information about the depths of the fishing gear
#' for catch by year for Canadian Freezer trawlers
#'
#' @format A data frame with 16 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"can_ft_gear_depth_df"
#' A data frame containing information about the depths of the fishing gear
#' for catch by year for Canadian Shoreside vessels
#'
#' @format A data frame with 16 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"can_ss_gear_depth_df"
#' A data frame containing information about the depths of the fishing gear
#' for catch by year for U.S. at-sea vessels
#'
#' @format A data frame with 15 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"us_atsea_fishing_depth_df"
#' A data frame containing information about the depths of the bottom
#' for catch by year for U.S. at-sea vessels
#'
#' @format A data frame with 15 rows and 6 variables:
#' \describe{
#'   \item{year}{Year of survey}
#'   \item{lower95}{The lower 95% percentile (2.5th)}
#'   \item{lowerhinge}{The lower hinge value (bottom IQR)}
#'   \item{median}{The median depth value}
#'   \item{upperhinge}{The upper hinge value (upper IQR)}
#'   \item{upper95}{The upper 95% percentile (97.5th)}
#' }
"us_atsea_bottom_depth_df"
#' A data frame containing information about the ports used in making the
#' overview map at the beginning of the figures section
#'
#' @format A data frame with 28 rows and 6 variables:
#' \describe{
#'   \item{pos}{Position (side) to place label on}
#'   \item{lat}{Latitude to place label}
#'   \item{lon}{Longitude to place label}
#'   \item{north_south}{Grouping variable for shiftin base vertical line for placement}
#'   \item{name}{Name of the port}
#' }
"ports_df"
#' A data frame containing information about the states and provinces used
#' in making the overview map at the beginning of the figures section
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{lat}{Latitude to place label}
#'   \item{lon}{Longitude to place label}
#'   \item{name}{Name of the port}
#' }
"states_df"
#' A data frame of empirical weight at age for all fisheries and all surveys
#'
#' @format A data frame with many rows and eight columns:
#' \describe{
#'   \item{source}{The fishery or survey the data came from}
#'   \item{weight}{The fish weight ()}
#'   \item{sex}{The sex of the fish, i.e., F, M, or U}
#'   \item{age}{The age in years of the fish}
#'   \item{month}{The month the fish was collected}
#'   \item{year}{The year the fish was collected}
#'   \item{data_type}{If the fish was collected on a fishing or survey vessel}
#'   \item{country}{The country the fish was collected in}
#' }
#' 
#' Proportion mature at each age
#'
#' A vector of maturity values from the maturity ogive. The length of the vector
#' is the same as the number of ages in the model, not the number of ages in the
#' data.
#'
#' @details
#' The data are stored in a data object that can be updated/built by running
#' the script in `data-raw/maturity_at_age.R`.
#'
#' @format ## `maturity_at_age`
#' A vector with 20 observations, one for each modelled age starting with age
#' zero.
#' @source Melissa Head
"maturity_at_age"

#' Quota information for U.S. sectors of Pacific Hake fishery
#' @format ## `who`
#' A data frame with 5 rows and many columns:
#' \describe{
#'   \item{Fleet}{Names of sectors that receive allocation}
#'   \item{2008,...}{Year-specific allocations}
#'   ...
#' }
"quotas"
