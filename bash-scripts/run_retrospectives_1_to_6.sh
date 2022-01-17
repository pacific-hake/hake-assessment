#!/bin/bash

# Run model retrospectives 1 through 6. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $BASE_MODEL is set in aws/install_hake.sh

# Required the MLE run in the root part of the model directory have all SS files present.
# To do that, copy from S3 hakestore:
# cp hakestore/models/$BASE_MODEL/* models/$BASE_MODEL

BASE_MODEL=2022.01.10_base
years=(1 2 3 4 5 6)

for year in ${years[@]}; do
  (trap 'kill 0' SIGINT; \
  Rscript -e "setwd(here::here()); \
  source('R/all.R'); \
  run_retrospectives('$MODELS_DIR/$BASE_MODEL', retrospective_yrs = $year, n_cores = 16)" \
  > /dev/null 2>&1; \
  year_string=`printf "%02d" $year`; \
  cp -R ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/retrospectives/retro-$year_string \
    ~/hake-assessment/hakestore/$MODELS_DIR/$BASE_MODEL/retrospectives; \
  echo; \
  echo "Copied retrospective -$year to S3 storage, retrospective completed") &
done
