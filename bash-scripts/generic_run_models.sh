#!/bin/bash

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running $model in parallel in a subshell"; \
  echo; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  run_adnuts('$MODELS_DIR/$model', \
             extra_mcmc = $extra_mcmc, \
             n_cores = $n_cores, \
             adapt_delta = $adapt_delta, \
             n_final = $n_final, \
             warmup_final = $warmup_final);
  build_rds('$model');" > /dev/null 2>&1; \
  echo; \
  echo "$model MCMC complete"; \
  rm -rf ~/hake-assessment/hakestore/$MODELS_DIR/$model; \
  cp -R ~/hake-assessment/$MODELS_DIR/$model ~/hake-assessment/hakestore/$MODELS_DIR; \
  echo; \
  echo "Copied $model model output to S3 storage") &
done
