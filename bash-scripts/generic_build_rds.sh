#!/bin/bash

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Building RDS for $model in a subshell"; \
  echo; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  build_rds('../models/$model')" > /dev/null 2>&1; \
  echo; \
  echo "$model RDS complete") &
done
