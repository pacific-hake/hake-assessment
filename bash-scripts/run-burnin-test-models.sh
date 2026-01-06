#!/bin/bash

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

models=( \
#03-burnin-1 \
#04-burnin-50 \
#05-burnin-100 \
#06-burnin-150 \
#07-burnin-200 \
08-burnin-250 \
09-burnin-300 \
10-burnin-350 \
11-burnin-400 \
12-burnin-450 \
13-burnin-500 \
)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

# The following seems to introduce a leading newline when tried in 2026
repo_path=`Rscript -e "cat(here::here())"`
# Remove leading newline
repo_path=`echo $repo_path | tr -d '\n'`

version_path="02-version"
type_path="05-test-models"
models_path="models"
ss_exe="ss3_2024"

year_path=2024
run_extra_mcmc=FALSE
adapt_delta=0.95
num_chains=8
num_samples=8000

models_path=$project_path/$models_path/$year_path/$version_path/$type_path

[ -d "$models_path" ] && printf "\nDirectory $models_path exists, starting \
run loop.\n" || { printf "\nError: Directory $models_path does not exist, \
bailing out.\n" ; exit 1; }

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  model_path=$models_path/$model; \
  [ -d "$model_path" ] && printf "\nDirectory $model_path exists, running the \
  model.\n" || { printf "\nError: Directory $model_path does not \
  exist, bailing out for this model.\n" ; exit 1; };
  printf "\nRunning model in directory\n$model\n"; \
  num_warmup_samples=`echo "$model" | grep -Po '\d+$'`; \
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
