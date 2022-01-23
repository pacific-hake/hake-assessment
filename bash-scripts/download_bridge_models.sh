#!/bin/bash

models=(2022.01.01_newSSexe 2022.01.03_newcatchage 2022.01.05_updatesurvey \
        2022.01.06_newsurvey 2022.01.07_newwtatage 2022.01.09_age1index)

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Downloading from S3 $model in a subshell"; \
  echo; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  s3_download('models/$model')"; \
  echo; \
  echo "Download of $model RDS complete") &
done

