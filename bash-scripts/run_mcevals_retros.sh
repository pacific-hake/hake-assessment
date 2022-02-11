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

mkdir ~/hake-assessment/$MODELS_DIR/2022.01.10_base_v2/retrospectives; \
mkdir ~/hake-assessment/$MODELS_DIR/2022.01.23_age1Survey/retrospectives; \

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running mceval for $model in parallel in a subshell"; \
  echo; \
  mkdir ~/hake-assessment/$MODELS_DIR/$model; \
  mkdir ~/hake-assessment/$MODELS_DIR/$model/mcmc; \
  cp ~/hake-assessment/hakestore/$MODELS_DIR/$model/* ~/hake-assessment/$MODELS_DIR/$model; \
  cp ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/* ~/hake-assessment/$MODELS_DIR/$model/mcmc; \
  cd ~/hake-assessment/$MODELS_DIR/$model/mcmc; \
  /usr/bin/ss/ss -mceval; \
  echo; \
  echo "$model MCeval complete"; \
  cp ~/hake-assessment/$MODELS_DIR/$model/mcmc/posteriors.sso ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/posteriors.sso; \
  cp ~/hake-assessment/$MODELS_DIR/$model/mcmc/derived_posteriors.sso ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/derived_posteriors.sso; \
  cp ~/hake-assessment/$MODELS_DIR/$model/mcmc/posterior_vectors.sso ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/posterior_vectors.sso; \
  cp ~/hake-assessment/$MODELS_DIR/$model/mcmc/posterior_obj_func.sso ~/hake-assessment/hakestore/$MODELS_DIR/$model/mcmc/posterior_obj_func.sso; \
  echo; \
  echo "Copied $model model output to S3 storage") &

done
