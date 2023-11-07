# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

# Regular expressions used in the assessment ----
create_data_hake("regex_recdev_early", "^Early_InitAge_([0-9]{1,2})$")
create_data_hake("regex_recdev_all",
                 "^(Early_RecrDev_|Main_RecrDev_|Late_RecrDev_)([0-9]{4})$")
create_data_hake("regex_eml_recdevs", "^[EML].+_RecrDev")
create_data_hake("regex_main_recdevs", "^Main_RecrDev")

create_data_hake("regex_extra_mcmc_report", "Report_mce_[0-9]+\\.sso$")
create_data_hake("regex_extra_mcmc_compreport", "CompReport_mce_[0-9]+\\.sso$")
# create_data_hake("", "")
