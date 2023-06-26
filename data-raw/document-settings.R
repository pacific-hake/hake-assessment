# General variables
ct_levels_spr_tol <- 0.001
ct_levels_catch_tol <- 100
ct_levels_max_iter <- 20
probs <- c(0.025, 0.5, 0.975)
probs_forecast <- c(0.05, 0.25, 0.5, 0.75, 0.95)
retrospective_yrs <- 1:10
plot_retro_yrs <- 1:5
show_ss_output <- FALSE
large_cohorts <- c(2010, 2014, 2016, 2020)
age_bubble_cohorts <- c(1980, 1984, 1999, 2010, 2014, 2016, 2020)

# Data start and endpoint variables ----
recruit_dev_start_yr <- 1946
unfished_eq_yr <- 1964
start_yr <- 1966
start_yr_age_comps <- 1975
survey_start_yr <- 1995
survey_end_yr <- 2021
surv_yrs <- c(1995, 1998, 2001, 2003, 2005, 2007,
              2009, 2011, 2012, 2013, 2015, 2017,
              2019, 2021)

# Source this file to see the changes
usethis::use_data(ct_levels_spr_tol, overwrite = TRUE)
usethis::use_data(ct_levels_catch_tol, overwrite = TRUE)
usethis::use_data(ct_levels_max_iter, overwrite = TRUE)
usethis::use_data(probs, overwrite = TRUE)
usethis::use_data(probs_forecast, overwrite = TRUE)
usethis::use_data(retrospective_yrs, overwrite = TRUE)
usethis::use_data(plot_retro_yrs, overwrite = TRUE)
usethis::use_data(show_ss_output, overwrite = TRUE)
usethis::use_data(recruit_dev_start_yr, overwrite = TRUE)
usethis::use_data(unfished_eq_yr, overwrite = TRUE)
usethis::use_data(start_yr, overwrite = TRUE)
usethis::use_data(start_yr_age_comps, overwrite = TRUE)
usethis::use_data(survey_start_yr, overwrite = TRUE)
usethis::use_data(survey_end_yr, overwrite = TRUE)
usethis::use_data(surv_yrs, overwrite = TRUE)
usethis::use_data(large_cohorts, overwrite = TRUE)
usethis::use_data(age_bubble_cohorts, overwrite = TRUE)
