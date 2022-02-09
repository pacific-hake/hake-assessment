#!/bin/bash

models=(2022.01.10_base_v2/retrospectives/retro-01 \
        2022.01.10_base_v2/retrospectives/retro-02 \
        2022.01.10_base_v2/retrospectives/retro-03 \
        2022.01.10_base_v2/retrospectives/retro-04 \
        2022.01.10_base_v2/retrospectives/retro-05 \
        2022.01.10_base_v2/retrospectives/retro-06 \
        2022.01.10_base_v2/retrospectives/retro-07 \
        2022.01.10_base_v2/retrospectives/retro-09 \
        2022.01.10_base_v2/retrospectives/retro-10 \
        2022.01.23_age1Survey/retrospectives/retro-01 \
        2022.01.23_age1Survey/retrospectives/retro-02 \
        2022.01.23_age1Survey/retrospectives/retro-03 \
        2022.01.23_age1Survey/retrospectives/retro-04 \
        2022.01.23_age1Survey/retrospectives/retro-05 \
        2022.01.23_age1Survey/retrospectives/retro-06 \
        2022.01.23_age1Survey/retrospectives/retro-07 \
        2022.01.23_age1Survey/retrospectives/retro-08 \
        2022.01.23_age1Survey/retrospectives/retro-09 \
        2022.01.23_age1Survey/retrospectives/retro-10)

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running mceval for $model in parallel in a subshell"; \
  echo; \
  cd /home/cgrandin/hake-assessment/hakestore/models/$model/mcmc; \
  /usr/bin/ss/ss -mceval; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  build_rds('$model', '/home/cgrandin/hake-assessment/models');" > /dev/null 2>&1; \
  echo; \
  echo "$model MCeval complete") &
done
