#!/bin/bash

# Run base model MCMC using adnuts on AWS
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
LY_BASE_MODEL=2021.00.04_base_2022_code
BASE_MODEL_12_CPUS=2022.01.10_base_12_cpus

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/$LY_BASE_MODEL', adapt_delta = 0.95, extra_mcmc = TRUE, n_cores = 16)" \
> /dev/null 2>&1; echo "$LY_BASE_MODEL MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/$BASE_MODEL_12_CPUS', adapt_delta = 0.95, extra_mcmc = TRUE, n_cores = 12)" \
> /dev/null 2>&1; echo "$BASE_MODEL_12_CPUS MCMC complete")

# Change a filename so that it works with hake-assessment code.
# R4ss breaks if this is not done.
mv ~/hake-assessment/$MODELS_DIR/$LY_BASE_MODEL/data_echo.ss_new \
~/hake-assessment/$MODELS_DIR/$LY_BASE_MODEL/data.ss_new

mv ~/hake-assessment/$MODELS_DIR/$BASE_MODEL_12_CPUS/data_echo.ss_new \
~/hake-assessment/$MODELS_DIR/$BASE_MODEL_12_CPUS/data.ss_new

# Copy all the output for the base model to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/$LY_BASE_MODEL \
~/hake-assessment/hakestore/$MODELS_DIR/$LY_BASE_MODEL &

cp -R ~/hake-assessment/$MODELS_DIR/$BASE_MODEL_12_CPUS \
~/hake-assessment/hakestore/$MODELS_DIR/$BASE_MODEL_12_CPUS

echo "Copied $LY_BASE_MODEL output to S3 storage"
echo "Copied $BASE_MODEL_12_CPUS output to S3 storage"
