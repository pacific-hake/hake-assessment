#!/bin/bash
# Create bridge model RDS files in parallel
#
# Note that if you are working in Windows and running this script in WSL2, the
# Linux version of R is run, so if there are any changes to gfiscamutils,
# you must have pushed them to Github and then entered R in Linux and run:
# remotes::install_github("pbs-assess/gfiscamutils")
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' create_rds_files_bridge.sh

 models=(01-base-2015 \
         02-bridge-update-data-to-2014 \
         03-bridge-update-data-to-2021 \
         04-bridge-add-wchg \
         05-bridge-switch-to-dm-likelihood \
         06-bridge-switch-to-2-fleet-model \
         07-bridge-add-discard-cpue \
         08-bridge-switch-to-split-sex \
         09-bridge-switch-fishing-year-to-feb-21-feb-20 \
         10-bridge-remove-wchg \
         11-bridge-fix-natural-mortalities)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models
model_group=01-bridge-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Creating RDS file for $model in a subshell"; \
  Rscript -e "library(tidyverse);library(gfiscamutils);create_rds_file('$models_path/$model_group/$model')"
  echo "$model RDS file created") &

done
