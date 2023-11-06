#!/bin/bash

# Run a single model MCMC using ADNUTS
# To fix the permissions for the hake group:
# chmod -R u+rwx,g+rwx,o= 01-base-models

# Start timer
SECONDS=0

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
type_path="05-test-models"
model_name="01-test-ss3"

num_chains=16
num_samples=8000
num_warmup_samples=250
adapt_delta=0.95
run_extra_mcmc=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not exist, bailing out." ; exit 1; }

(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); devtools::load_all(); \
run_adnuts_timed('$model_path', adapt_delta = $adapt_delta, run_extra_mcmc = $run_extra_mcmc, \
                 num_chains = $num_chains, num_samples = $num_samples, \
                 num_warmup_samples = $num_warmup_samples)" \
  > /dev/null 2>&1; echo "Base model MCMC complete")

