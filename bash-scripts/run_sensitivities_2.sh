#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 80 CPUs for these 5
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.22_M_hamel_prior 2022.01.24_compWeight_HarmonicMean 2022.01.27_tvSelect_phi_extralow \
        2022.01.28_tvSelect_phi_low 2022.01.29_tvSelect_phi_high)

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running $model in a subshell"; \
  echo; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  run_adnuts('$MODELS_DIR/$model', adapt_delta = 0.95, n_cores = 16);
  build_rds('$model');" > /dev/null 2>&1; \
  echo; \
  echo "$model MCMC complete"; \
  cp -R ~/hake-assessment/$MODELS_DIR/$model ~/hake-assessment/hakestore/$MODELS_DIR/$model; \
  echo; \
  echo "Copied $model model output to S3 storage") &
done
