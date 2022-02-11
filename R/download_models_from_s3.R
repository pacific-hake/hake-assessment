# Download models that have been modified after finding a problem in SS and fixing it (where the biomass for the final year is incorrect)
# Feb 11, 2022

s3_quick_download(file.path("models",
  c("2022.01.01_newSSexe/2022.01.01_newSSexe.rds",
    "2022.01.03_newcatchage/2022.01.03_newcatchage.rds",
    "2022.01.05_updatesurvey/2022.01.05_updatesurvey.rds",
    "2022.01.06_newsurvey/2022.01.06_newsurvey.rds",
    "2022.01.07_newwtatage/2022.01.07_newwtatage.rds",
    "2022.01.09_age1index/2022.01.09_age1index.rds",
    "2022.01.10_base_v2/2022.01.10_base_v2.rds",
    "2022.01.15_h_prior_mean_low/2022.01.15_h_prior_mean_low.rds",
    "2022.01.16_h_fix_high/2022.01.16_h_fix_high.rds",
    "2022.01.17_sigmR_fix_low/2022.01.17_sigmR_fix_low.rds",
    "2022.01.18_sigmR_fix_high/2022.01.18_sigmR_fix_high.rdws",
    "2022.01.20_M_0.2SD/2022.01.20_M_0.2SD.rds",
    "2022.01.21_M_0.3SD/2022.01.21_M_0.3SD.rds",
    "2022.01.23_age1Survey/2022.01.23_age1Survey.rds",
    "2022.01.24_compWeight_HarmonicMean/2022.01.24_compWeight_HarmonicMean.rds",
    "2022.01.27_tvSelect_phi_extralow/2022.01.27_tvSelect_phi_extralow.rds",
    "2022.01.28_tvSelect_phi_low/2022.01.28_tvSelect_phi_low.rds",
    "2022.01.29_tvSelect_phi_high/2022.01.29_tvSelect_phi_high.rds",
    "2022.01.43_maxSel_Age5/2022.01.43_maxSel_Age5.rds",
    "2022.01.44_maxSel_Age7/2022.01.44_maxSel_Age7.rds",
    "2022.01.45_maxSel_Age8/2022.01.45_maxSel_Age8.rds",
    "2022.01.100_zerosumcontraint/2022.01.100_zerosumcontraint.rds")))

make_small_rds()
