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
"key_posteriors_file"
#' A character string containing the file name for the nuisance posteriors
#' CSV file
#'
#' @format A character string
"nuisance_posteriors_file"
#' A character string containing the name of the SS3 executable
#' (without extension)
#'
#' @format A character string
"ss_executable"
#' A character string containing the file name for the SS3 starter file
#'
#' @format A character string
"starter_fn"
#' A character string containing the file name for the SS3 par file
#' (parameter output)
#'
#' @format A character string
"par_fn"
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
#' @format A vector of three values, for the lower credible interval limit,
#' a lower-middle value, the median, a middle-upper value, and the upper
#' credible interval limit
"forecast_probs"
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
