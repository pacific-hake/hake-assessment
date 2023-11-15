#!/bin/bash

# Run base model forecasts only

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

repo_path=`Rscript -e "cat(here::here())"`
models_path="models"
# *Never* change `year_path` manually - See `get-assess-year.sh` call above
year_path=$assess_year
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

num_chains=16
num_samples=8000
num_warmup_samples=250
adapt_delta=0.95
run_extra_mcmc=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name

[[ ! -d $model_path ]] && { echo "Error: Directory $model_path \
does not exist, bailing out." ; exit 1; }

# Run the base models forecasts
(trap 'kill 0' SIGINT; \
  echo; \
  Rscript -e " \
    setwd('$repo_path'); \
    devtools::load_all(); \
    run_forecasts(model_path = $model_path)" \
  > /dev/null 2>&1; \
  printf "\nBase model forecasts complete\n" \
)
