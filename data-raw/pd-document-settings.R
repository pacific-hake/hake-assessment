# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

# General variables
create_data_hake("ct_levels_spr_tol", 0.001)
create_data_hake("ct_levels_catch_tol", 0.5)
create_data_hake("ct_levels_max_iter", 30)
create_data_hake("probs", c(0.025, 0.5, 0.975))
create_data_hake("probs_forecast", c(0.05, 0.25, 0.5, 0.75, 0.95))
create_data_hake("retrospective_yrs", 1:10)
create_data_hake("plot_retro_yrs", 1:5)
create_data_hake("large_cohorts", c(2010, 2014, 2016, 2021))
create_data_hake("age_bubble_cohorts",
                 c(1980, 1984, 1999, 2010, 2014, 2016, 2021))

# Data start and endpoint variables ----
create_data_hake("recruit_dev_start_yr", 1946)
create_data_hake("unfished_eq_yr", 1964)
create_data_hake("start_yr", 1966)
create_data_hake("start_yr_age_comps", 1975)
create_data_hake("survey_start_yr", 1995)
create_data_hake("survey_end_yr", 2023)
create_data_hake("surv_yrs", c(1995, 1998, 2001, 2003, 2005, 2007, 2009,
                               2011, 2012, 2013, 2015, 2017, 2019, 2021, 2023))
