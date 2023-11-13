#!/bin/bash

# Must comment out two rows of this at a time to run on hake-precision server
# Each takes 16 CPUs, so 64 CPUs for 4 of them, or run all ten on 5 CPUs each
#years=(1 2 3 4)
#years=(4 5 6 7)
years=(7 8 9 10)

repo_path=`Rscript -e "cat(here::here())"`
# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

models_path="/srv/hake/models"
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
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not \
exist, bailing out." ; exit 1; }

for year in ${years[@]}; do
  (trap 'kill 0' SIGINT; \
  echo; \
  Rscript -e " \
  setwd('$repo_path'); devtools::load_all(); \
  run_retrospectives(model_path = '$model_path', \
                     yrs = $year, \
                     num_chains = $num_chains, \
                     num_samples = $num_samples, \
                     num_warmup_samples = $num_warmup_samples, \
                     adapt_delta = $adapt_delta, \
                     run_extra_mcmc = $run_extra_mcmc)" > /dev/null 2>&1; \
  echo "Retrospective -$year completed") &
done
