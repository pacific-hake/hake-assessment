#!/bin/bash

for year in ${years[@]}; do
  (trap 'kill 0' SIGINT; \
  Rscript -e "setwd(here::here()); \
  source('R/all.R'); \
  run_retrospectives('$MODELS_DIR/$MODEL', retrospective_yrs = $year, n_cores = $n_cores)" > /dev/null 2>&1; \
  year_string=`printf "%02d" $year`; \
  rm -rf ~/hake-assessment/hakestore/$MODELS_DIR/$MODEL/retrospectives/retro-$year_string; \
  cp -R ~/hake-assessment/$MODELS_DIR/$MODEL/retrospectives/retro-$year_string \
    ~/hake-assessment/hakestore/$MODELS_DIR/$MODEL/retrospectives; \
  echo; \
  echo "Copied retrospective -$year to S3 storage, retrospective completed") &
done
