# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

# Key posteriors used in the assessment ----
create_data_hake("key_posteriors",
                 list("NatM",
                      "SR_LN",
                      "SR_BH_steep",
                      "Q_extraSD_Acoustic_Survey",
                      "Q_extraSD_Age1_Survey",
                      "ln\\(DM_theta\\)_Age_P1",
                      "ln\\(DM_theta\\)_Age_P2"))

create_data_hake("key_posteriors_titles",
                 list("Natural mortality",
                      "ln(R[0])",
                      "Steepness",
                      "Survey extra SD",
                      "Age 1 extra SD",
                      "Dirichlet-multinomial fishery",
                      "Dirichlet-multinomial survey"))

create_data_hake("key_posteriors_fn",
                 "keyposteriors.csv")

create_data_hake("nuisance_posteriors_fn",
                 "nuisanceposteriors.csv")
