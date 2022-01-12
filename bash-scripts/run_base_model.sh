#!/bin/bash

# Run base model MCMC using adnuts on AWS
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/$BASE_MODEL', adapt_delta = 0.95)" \
> /dev/null 2>&1; echo "Base model MCMC complete")

# Change a filename so that it works with hake-assessment code.
# R4ss breaks if this is not done.
mv ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/data_echo.ss_new \
~/hake-assessment/$MODELS_DIR/$BASE_MODEL/data.ss_new

# Run the base models catch-level calculations
# Environment variable $BASE_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_catch_levels = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model catch level calculations complete")

# Run the base models forecasts
# Environment variable $BASE_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_catch_levels = FALSE, run_forecasts = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model forecasts complete")

# Delete the unnecessary files. Some are huge (eg. echoinput.sso can 3GB)
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'admodel.*' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'Cumreport.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'echoinput.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'fmin.log' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'Forecast-report.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'mcmc' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'runnumber.ss' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'ParmTrace.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'posterior_vectors.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'posterior_obj_func.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'rebuild.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'sims' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'ss.*' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'unbounded.csv' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'warning.sso' -delete
find ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/forecasts -type f -name 'wtatage.ss_new' -delete

# Copy all the output for the base model to the persistent S3 drive 'hakestore'
DATE_STR="_jan04_2022"
cp -R ~/hake-assessment/$MODELS_DIR/$BASE_MODEL \
~/hake-assessment/hakestore/$BASE_MODEL$DATE_STR
echo "Copied Base model output to S3 storage"
