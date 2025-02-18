#!/bin/bash

# To check if the models were run successfully run this command:
#
# find . -wholename '*/mcmc/sso/posteriors.sso' | sort
#
# There must be a posteriors.sso file in the sso directory for the
# model to have been completed. This is a recursive command,
# so to see if all bridge models (for example) finished,
# enter the 02-bridge-models directory and issue the command
#
# The path structure is as follows
# /srv/hake/models/2023/01-version/01-base-models/01-base/
#  ^   ^    ^      ^    ^          ^              ^
#  |   |    |      |    |          |              |
#  |   |    |      |    |          |              $model_name
#  |   |    |      |    |          $type_path
#  |   |    |      |    $version_path
#  |   |    |      $year
#  |   |    $models_path
#  \  /
#   ||
#   $project_path

# Run ADNUTS MCMC using Rscript from the Bash command line in
# parallel subshells

# This script must be called from another script where the following
# variables have been set
[[ -z $models ]] && { printf "\nVariable 'models' has not been set, bailing \
out.\n" ; exit 1; }
[[ -z $type_path ]] && { printf "\nVariable 'type_path' has not been set, \
bailing out.\n" ; exit 1; }
[[ -z $run_extra_mcmc ]] && { printf "\nVariable 'run_extra_mcmc' has not been \
set, bailing out.\n" ; exit 1; }
[[ -z $adapt_delta ]] && { printf "\nVariable 'adapt_delta' has not been set, \
bailing out.\n" ; exit 1; }

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
repo_path=`Rscript -e "cat(here::here())"`
models_path="models"
# *Never* change `year_path` manually - See `get-assess-year.sh` call above
year_path=$assess_year
version_path="02-version"

ss_exe="ss3"
num_chains=8
num_samples=8000
num_warmup_samples=250

models_path=$project_path/$models_path/$year_path/$version_path/$type_path

[ -d "$models_path" ] && printf "\nDirectory $models_path exists, starting \
run loop.\n" || { printf "\nError: Directory $models_path does not exist sucks to be you, \
bailing out.\n" ; exit 1; }

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  model_path=$models_path/$model; \
  [ -d "$model_path" ] && printf "\nDirectory $model_path exists, running the \
  model.\n" || { printf "\nError: Directory $model_path does not \
  exist, bailing out for this model.\n" ; exit 1; };
  printf "\nRunning model in directory\n$model\n"; \
  Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  run_adnuts_timed(path = '$model_path', \
                   run_extra_mcmc = $run_extra_mcmc, \
                   num_chains = $num_chains, \
                   adapt_delta = $adapt_delta, \
                   num_samples = $num_samples, \
                   num_warmup_samples = $num_warmup_samples, \
                   fn_exe = '$ss_exe')"; \
  > /dev/null 2>&1; \
  printf "\nFinished running model in directory\n$model\n"; \
  ) &
done
