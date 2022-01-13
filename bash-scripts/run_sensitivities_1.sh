#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 96 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.15_h_prior_mean_low', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.15_h_prior_mean_low MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.16_h_fix_high', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.16_h_fix_high MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.17_sigmR_fix_low', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.17_sigmR_fix_low MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.18_sigmR_fix_high', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.18_sigmR_fix_high MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.20_M_0.2SD', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.20_M_0.2SD MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.21_M_0.3SD', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.21_M_0.3SD MCMC complete")

# Copy all the output to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/2022.01.15_h_prior_mean_low \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.15_h_prior_mean_low
echo "Copied 2022.01.15_h_prior_mean_low model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.16_h_fix_high \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.16_h_fix_high
echo "Copied 2022.01.16_h_fix_high model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.17_sigmR_fix_low \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.17_sigmR_fix_low
echo "Copied 2022.01.17_sigmR_fix_low model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.18_sigmR_fix_high \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.18_sigmR_fix_high
echo "Copied 2022.01.18_sigmR_fix_high model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.20_M_0.2SD \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.20_M_0.2SD
echo "Copied 2022.01.20_M_0.2SD model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.21_M_0.3SD \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.21_M_0.3SD
echo "Copied 2022.01.21_M_0.3SD model output to S3 storage"
