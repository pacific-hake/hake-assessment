#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 80 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.22_M_hamel_prior', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.22_M_hamel_prior MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.24_compWeight_HarmonicMean', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.24_compWeight_HarmonicMean MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.27_tvSelect_phi_extralow', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.27_tvSelect_phi_extralow MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.28_tvSelect_phi_low', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.28_tvSelect_phi_low MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.29_tvSelect_phi_high', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.29_tvSelect_phi_high MCMC complete")

# Copy all the output to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/2022.01.22_M_hamel_prior \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.22_M_hamel_prior
echo "Copied 2022.01.22_M_hamel_prior model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.23_age1Survey \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.23_age1Survey
echo "Copied 2022.01.23_age1Survey model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.24_compWeight_HarmonicMean \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.24_compWeight_HarmonicMean
echo "Copied 2022.01.24_compWeight_HarmonicMean model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.27_tvSelect_phi_extralow \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.27_tvSelect_phi_extralow
echo "Copied 2022.01.27_tvSelect_phi_extralow model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.28_tvSelect_phi_low \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.28_tvSelect_phi_low
echo "Copied 2022.01.28_tvSelect_phi_low model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.29_tvSelect_phi_high \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.29_tvSelect_phi_high
echo "Copied 2022.01.29_tvSelect_phi_high model output to S3 storage"
