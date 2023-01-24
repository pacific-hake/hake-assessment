#!/bin/bash

# Run base model forecasts only

# Start timer

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
run_extra_mcmc=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not exist, bailing out." ; exit 1; }

# Run the base models forecasts
(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); source('R/all.R'); \
build_rds('$model_path', run_catch_levels = FALSE, run_forecasts = TRUE, build_file = FALSE)" \
  > /dev/null 2>&1; echo "Base model forecasts complete")

