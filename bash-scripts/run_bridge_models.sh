#!/bin/bash

# Run bridging models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 96 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.01_newSSexe 2022.01.03_newcatchage 2022.01.05_updatesurvey \
        2022.01.06_newsurvey 2022.01.07_newwtatage 2022.01.09_age1index)

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
