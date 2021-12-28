#!/bin/bash
#
# Run base model MCMC using adnuts
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODEL_DIR/2021.00.04_base_v1', adapt_delta = 0.9)" \
> adnuts.log 2>&1)

# Run the base models forecasting and retrospectives, and build the RDS file
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2021.00.04_base_v1', run_catch_levels = TRUE, run_forecasts = TRUE, run_retrospectives = TRUE)" \
> adnuts.log 2>&1)

# Change a filename so that it works with hake-assessment code. r4ss breaks if this is not done
mv ~/hake-assessment/models/2021.00.04_base_v1/data_echo.ss_new ~/hake-assessment/models/2021.00.04_base_v1/data.ss_new

# Copy all the output for the base model to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/models/2021.00.04_base_v1 ~/hake-assessment/hakestore/2021.00.04_base_v1_dec28
