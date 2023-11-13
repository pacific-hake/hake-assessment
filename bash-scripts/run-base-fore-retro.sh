#!/bin/bash

# Run base model catch-levels, forecasts, and retrospectives

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

repo_path=`Rscript -e "cat(here::here())"`
models_path="/srv/hake/models"
# *Never* change `year_path` manually - See `get-assess-year.sh` call above
year_path=$assess_year
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

# retrospectives settings
num_chains=16
num_samples=8000
num_warmup_samples=250
adapt_delta=0.95

# Forecasts are run with -mceval only so other settings don't apply
run_forecasts=TRUE
run_retrospectives=TRUE
# Retrospective extra-mcmc setting
run_extra_mcmc=TRUE

# catch-levels settings
run_catch_levels=TRUE
run_default_hr=TRUE
run_spr_100=TRUE
run_stable_catch=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not \
exist, bailing out." ; exit 1; }

# Run all
Rscript -e " \
setwd('$repo_path'); \
devtools::load_all(); \
run_forecasts_retrospectives(model_path = '$model_path', \
                             run_catch_levels = $run_catch_levels, \
                             run_forecasts = $run_forecasts, \
                             run_retrospectives = $run_retrospectives, \
                             num_chains = $num_chains, \
                             num_samples = $num_samples, \
                             num_warmup_samples = $num_warmup_samples, \
                             adapt_delta = $adapt_delta, \
                             run_extra_mcmc = $run_extra_mcmc, \
                             run_default_hr = $run_default_hr, \
                             run_spr_100 = $run_spr_100, \
                             run_stable_catch = $run_stable_catch)" \
> /dev/null 2>&1; \
printf "\nForecasts amd retrospectives completed\n"
