#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 64 CPUs for these 4
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.43_maxSel_Age5 2022.01.44_maxSel_Age7 2022.01.45_maxSel_Age8 \
        2022.01.100_zerosumcontraint)

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
