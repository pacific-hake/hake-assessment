# Stock Synthesis file names needed for the hake package
ss_executable <- "ss3"
starter_fn <- "starter.ss"
par_fn <- "ss.par"
forecast_fn <- "forecast.ss"
weight_at_age_fn <- "wtatage.ss"
posts_fn <- "posteriors.sso"
derposts_fn <- "derived_posteriors.sso"
report_fn <- "Report.sso"
comp_report_fn <- "CompReport.sso"

# Source this file to see the changes
usethis::use_data(ss_executable, overwrite = TRUE)
usethis::use_data(starter_fn, overwrite = TRUE)
usethis::use_data(par_fn, overwrite = TRUE)
usethis::use_data(forecast_fn, overwrite = TRUE)
usethis::use_data(weight_at_age_fn, overwrite = TRUE)
usethis::use_data(posts_fn, overwrite = TRUE)
usethis::use_data(derposts_fn, overwrite = TRUE)
usethis::use_data(report_fn, overwrite = TRUE)
usethis::use_data(comp_report_fn, overwrite = TRUE)
