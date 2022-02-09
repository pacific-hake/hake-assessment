#!/bin/bash

models=(2022.01.01_newSSexe \
        2022.01.03_newcatchage \
        2022.01.05_updatesurvey \
        2022.01.06_newsurvey \
        2022.01.07_newwtatage \
        2022.01.09_age1index \
        2022.01.15_h_prior_mean_low \
        2022.01.16_h_fix_high \
        2022.01.17_sigmR_fix_low \
        2022.01.18_sigmR_fix_high \
        2022.01.20_M_0.2SD \
        2022.01.21_M_0.3SD \
        2022.01.22_M_hamel_prior \
        2022.01.24_compWeight_HarmonicMean \
        2022.01.27_tvSelect_phi_extralow \
        2022.01.28_tvSelect_phi_low \
        2022.01.29_tvSelect_phi_high \
        2022.01.43_maxSel_Age5 \
        2022.01.44_maxSel_Age7 \
        2022.01.45_maxSel_Age8 \
        2022.01.100_zerosumcontraint))

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running mceval for $model in parallel in a subshell"; \
  cp /home/cgrandin/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/* /home/cgrandin/hake-assessment/$MODELS_DIR/$model/mcmc; \
  cd /home/cgrandin/hake-assessment/$MODELS_DIR/$model/mcmc; \
  /usr/bin/ss/ss -mceval; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  build_rds('$model');" > /dev/null 2>&1; \
  echo; \
  echo "$model MCeval complete"; \
  rm -rf ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/sso; \
  cp -r ~/hake-assessment/$MODELS_DIR/$model/mcmc/sso ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc; \
  cp ~/hake-assessment/$MODELS_DIR/$model/$model.rds ~/hake-assessment/hakestore/$MODELS_DIR/$model; \
  echo; \
  echo "Copied $model model output to S3 storage") &

done

