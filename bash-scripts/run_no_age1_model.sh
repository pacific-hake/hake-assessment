#!/bin/bash

$NNO_AGE1_MODEL=2022.01.23_age1Survey
# Run base model without the age-1 index MCMC using adnuts on AWS
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/$NO_AGE1_MODEL', adapt_delta = 0.95)" \
> /dev/null 2>&1; echo "No age-1 model MCMC complete")

# Change a filename so that it works with hake-assessment code.
# R4ss breaks if this is not done.
mv ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/data_echo.ss_new \
~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/data.ss_new

# Run the base models catch-level calculations
# Environment variable $NO_AGE1_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$NO_AGE1_MODEL', run_catch_levels = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "No age-1 model catch level calculations complete")

# Delete all files except the forecast.ss files
find ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/catch-levels -type f \
 ! -name 'forecast.ss' -delete

# Run the base models forecasts
# Environment variable $NO_AGE1_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$NO_AGE1_MODEL', run_catch_levels = FALSE, run_forecasts = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "No age-1 model forecasts complete")

# Delete the unnecessary files. Some are huge (eg. echoinput.sso can 3GB)
find ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/forecasts -type f \
 ! \( -name 'posteriors.sso' -o -name 'derived_posteriors.sso' -o -name 'forecast.ss' \) -delete

# Copy all the output for the base model to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL \
~/hake-assessment/hakestore/$MODELS_DIR/$NO_AGE1_MODEL
echo "Copied No age-1 model output to S3 storage"
