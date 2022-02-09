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

mkdir /home/cgrandin/hake-assessment/$MODELS_DIR/2022.01.10_base_v2/retrospectives; \
mkdir /home/cgrandin/hake-assessment/$MODELS_DIR/2022.01.23_age1Survey/retrospectives; \

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running mceval for $model in parallel in a subshell"; \
  echo; \
  mkdir /home/cgrandin/hake-assessment/$MODELS_DIR/$model; \
  mkdir /home/cgrandin/hake-assessment/$MODELS_DIR/$model/mcmc; \
  cp /home/cgrandin/hake-assessment/hakestore/$MODELS_DIR/$model/* /home/cgrandin/hake-assessment/$MODELS_DIR/$model; \
  cp /home/cgrandin/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/* /home/cgrandin/hake-assessment/$MODELS_DIR/$model/mcmc; \
  cd /home/cgrandin/hake-assessment/$MODELS_DIR/$model; \
  /usr/bin/ss/ss -mceval; \
  echo; \
  echo "$model MCeval complete"; \
  rm -rf ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc; \
  cp -r ~/hake-assessment/$MODELS_DIR/$model/mcmc ~/hake-assessment/hakestore/$MODELS_DIR/$model; \
  echo; \
  echo "Copied $model model output to S3 storage") &

done
