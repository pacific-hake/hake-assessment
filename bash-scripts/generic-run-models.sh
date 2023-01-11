#!/bin/bash

# Run ADNUTS MCMC using Rscript from the Bash command line in parallel subshells

# This script must be called from another script where the following variables have been set
[[ -z $project_path ]] && { echo "Variable 'project_path' has not been set, bailing out." ; exit 1; }
[[ -z $type_path ]] && { echo "Variable 'type_path' has not been set, bailing out." ; exit 1; }
[[ -z $models ]] &&{ echo "Variable 'models' has not been set, bailing out." ; exit 1; }
[[ -z $extra_mcmc ]] &&{ echo "Variable 'extra_mcmc' has not been set, bailing out." ; exit 1; }
[[ -z $adapt_delta ]] &&{ echo "Variable 'adapt_delta' has not been set, bailing out." ; exit 1; }

models_path="models"
year_path=2023
version_path="01-version"

num_chains=16
num_samples=8000
num_warmup_samples=250

models_path=$project_path/$models_path/$year_path/$version_path/$type_path
[ -d "$models_path" ] && echo "Directory $models_path exists, starting run loop." || \
  { echo "Error: Directory $models_path does not exist, bailing out." ; exit 1; }

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  model_path=$models_path/$model; \
  [ -d "$model_path" ] && echo "Directory $model_path exists, running the model in a subshell." || \
    { echo "Error: Directory $model_path does not exist, bailing out for this model." ; exit 1; };
  echo; \
  Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  run_adnuts_timed(path = '$model_path', \
                   extra_mcmc = $extra_mcmc, \
                   num_chains = $num_chains, \
                   adapt_delta = $adapt_delta, \
                   num_samples = $num_samples, \
                   num_warmup_samples = $num_warmup_samples); \
  build_rds('$model');" > /dev/null 2>&1; \
  echo; \
  echo "Run complete for model $model"; \
  echo; \
done
