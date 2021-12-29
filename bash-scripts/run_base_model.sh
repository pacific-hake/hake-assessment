#!/bin/bash
#
# Run base model MCMC using adnuts on AWS
# Environment variable $MODEL_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODEL_DIR/$BASE_MODEL', adapt_delta = 0.9)" \
> adnuts.log 2>&1)

# Change a filename so that it works with hake-assessment code. r4ss breaks if this is not done
mv ~/hake-assessment/$MODEL_DIR/$BASE_MODEL/data_echo.ss_new ~/hake-assessment/$MODEL_DIR/$BASE_MODEL/data.ss_new

# Run the base models forecasting and retrospectives, and build the RDS file
# Environment variable $BASE_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_catch_levels = TRUE, run_forecasts = TRUE)" \
> adnuts.log 2>&1)

# Copy all the output for the base model to the persistent S3 drive 'hakestore'
DATE_STR="_dec29"
cp -R ~/hake-assessment/$MODEL_DIR/$BASE_MODEL ~/hake-assessment/hakestore/$BASE_MODEL$DATE_STR
