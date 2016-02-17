################################################################################
## Key values that pertain to the base model
################################################################################

## IMPORTANT - If any of these do not match up with what the models are set up
##  for, the build will fail. The only exception is that end.yr must actually
##  be the end year of the model + 1.

## recruitment deviations start year
recruit.dev.start.yr <- 1946
## Unfished equilibrium year.
unfished.eq.yr  <- 1964
## Start year for the models
start.yr        <- 1966
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr          <- 2016
## First year in the survey timeseries
survey.start.yr <- 1998
## Last year in the survey timeseries
survey.end.yr   <- 2015
## The last year an assessment was done
last.assess.yr  <- end.yr - 1
## current assessment year
assess.yr       <- end.yr
## Final year of data (This is what is the end year is in the model data files)
last.data.yr    <- end.yr - 1

## These will be used to generate the keyposteriors.csv file,
##  the remaining ones will be put into nuisanceposteriors.csv.
key.posteriors <- c("NatM_p_1_Fem_GP_1",
                    "SR_LN",
                    "SR_BH_steep",
                    "Q_extraSD_2_Acoustic_Survey")
key.posteriors.file <- "keyposteriors.csv"
nuisance.posteriors.file <- "nuisanceposteriors.csv"
