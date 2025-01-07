# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R
library(usethis)

create_data_hake("ss_executable", "ss3")
create_data_hake("show_ss_output", FALSE)

create_data_hake("covar_fn", "covar.sso")
create_data_hake("data_ssnew_fn", "data.ss_new")
create_data_hake("data_new_ssnew_fn", "data_echo.ss_new")


create_data_hake("starter_fn", "starter.ss")
create_data_hake("control_fn", "hake_control.ss")
create_data_hake("data_fn", "hake_data.ss")
create_data_hake("par_fn", "ss3.par")
create_data_hake("psv_fn", "ss3.psv")
create_data_hake("forecast_fn", "forecast.ss")
create_data_hake("weight_at_age_fn", "wtatage.ss")
create_data_hake("posts_fn", "posteriors.sso")
create_data_hake("derposts_fn", "derived_posteriors.sso")
create_data_hake("report_fn", "Report.sso")
create_data_hake("comp_report_fn", "CompReport.sso")
create_data_hake("model_output_log_fn", "model_output.log")

create_data_hake("ss_input_files", c(starter_fn,
                                     control_fn,
                                     data_fn,
                                     forecast_fn,
                                     weight_at_age_fn))
