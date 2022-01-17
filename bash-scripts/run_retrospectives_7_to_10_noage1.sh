#!/bin/bash

# Run model retrospectives 7 through 10. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $NO_AGE1_MODEL is set in aws/install_hake.sh

# Required the MLE run in the root part of the model directory have all SS files present.
# To do that, copy from S3 hakestore:
# cp hakestore/models/$NO_AGE1_MODEL/* models/$NO_AGE1_MODEL

NO_AGE1_MODEL=2022.01.23_age1Survey
years=(7 8 9 10)

for year in ${years[@]}; do
  (trap 'kill 0' SIGINT; \
  Rscript -e "setwd(here::here()); \
  source('R/all.R'); \
  run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = $year, n_cores = 16)" \
  > /dev/null 2>&1; \
  year_string=`printf "%02d" $year`; \
  cp -R ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives/retro-$year_string \
    ~/hake-assessment/hakestore/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives; \
  echo; \
  echo "Copied retrospective -$year to S3 storage, retrospective completed") &
done
