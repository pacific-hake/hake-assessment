# Key posteriors used in the assessment ----
key_posteriors <- list("NatM",
                       "SR_LN",
                       "SR_BH_steep",
                       "Q_extraSD_Acoustic_Survey",
                       "ln\\(DM_theta\\)_Age_P1",
                       "ln\\(DM_theta\\)_Age_P2")

key_posteriors_titles <- list("Natural mortality",
                              "ln(R[0])",
                              "Steepness",
                              "Survey extra SD",
                              "Dirichlet-multinomial fishery",
                              "Dirichlet-multinomial survey")

key_posteriors_file <- "keyposteriors.csv"
nuisance_posteriors_file <- "nuisanceposteriors.csv"

# Source this file to see the changes
usethis::use_data(key_posteriors, overwrite = TRUE)
usethis::use_data(key_posteriors_titles, overwrite = TRUE)
usethis::use_data(key_posteriors_file, overwrite = TRUE)
usethis::use_data(nuisance_posteriors_file, overwrite = TRUE)
