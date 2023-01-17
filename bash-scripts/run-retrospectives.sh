#!/bin/bash

# Must comment out two rows of this at a time to run on hake-precision server
# Each takes 16 CPUs
years=(1 2 3 4)
       5 6 7 8)
       9 10)
repo_path=`Rscript -e "cat(here::here())"`
# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#models+path=$repo_path/models
models_path="/srv/hake/models"
year_path=2023
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

num_chains=16
num_samples=8000
num_warmup_samples=250
adapt_delta=0.95
run_extra_mcmc=FALSE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not exist, bailing out." ; exit 1; }

for year in ${years[@]}; do
  (trap 'kill 0' SIGINT; \
  echo; \
  Rscript -e " \
  setwd('$repo_path'); source('R/all.R'); \
  run_retrospectives('$model_path', \
                     retrospective_yrs = $year, \
                     num_chains = $num_chains, \
                     num_samples = $num_samples, \
                     num_warmup_samples = $num_warmup_samples, \
                     adapt_delta = $adapt_delta, \
                     run_extra_mcmc = $run_extra_mcmc)" > /dev/null 2>&1; \
  echo "Retrospective -$year completed") &
done
