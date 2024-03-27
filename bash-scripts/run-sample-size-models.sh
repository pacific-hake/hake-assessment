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
01-80-delta-2-cores \
02-80-delta-4-cores \
03-80-delta-14-cores \
04-85-delta-2-cores \
05-85-delta-4-cores \
06-85-delta-14-cores \
07-90-delta-2-cores \
08-90-delta-4-cores \
09-90-delta-14-cores \
10-95-delta-2-cores \
11-95-delta-4-cores \
12-95-delta-14-cores \
)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
repo_path=`Rscript -e "cat(here::here())"`

version_path="02-version"
type_path="05-test-models/17-2500-samples-group"
models_path="models"
ss_exe="ss3_2024"

year_path=2024
run_extra_mcmc=FALSE
#adapt_delta=0.8
#num_chains=8
num_samples=`echo "$type_path" | grep -Po '\d+(?=-samples)'`

models_path=$project_path/$models_path/$year_path/$version_path/$type_path

[ -d "$models_path" ] && printf "\nDirectory $models_path exists, starting \
run loop.\n" || { printf "\nError: Directory $models_path does not exist, \
bailing out.\n" ; exit 1; }

for model in ${models[@]}; do
(
  trap 'kill 0' SIGINT
  model_path=$models_path/$model
  [ -d "$model_path" ] && printf "\nDirectory $model_path exists, running the \
  model.\n" || { printf "\nError: Directory $model_path does not \
  exist, bailing out for this model.\n" ; exit 1; }

  model_num_str=`echo $model | grep -Po '^\d+(?=-\d+)'`
  model_num=${model_num_str#0}
  if [[ ${model_num} -gt 6 ]]; then
    printf "Exiting $model because the model number is greater than 6\n"
    exit 0
  fi

  adapt_delta_num=`echo "$model" | grep -Po '\d+(?=-delta)'`
  adapt_delta=`echo ".$adapt_delta_num"`
  num_chains=`echo "$model" | grep -Po '\d+(?=-cores)'`
  printf "\nRunning model in directory $model with:\n"
  printf "model number string = $model_num_str\n"
  printf "model integer = $model_num\n"
  printf "adapt_delta_num = $adapt_delta_num\n"
  printf "adapt_delta = $adapt_delta\n"
  printf "num chains (cores) = $num_chains\n"

  Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  run_adnuts_timed(path = '$model_path', \
                   run_extra_mcmc = $run_extra_mcmc, \
                   num_chains = $num_chains, \
                   adapt_delta = $adapt_delta, \
                   num_samples = $num_samples, \
                   num_warmup_samples = 50, \
                   fn_exe = '$ss_exe')"; \
  > /dev/null 2>&1; \
  printf "\nFinished running model in directory\n$model\n"
  ) &
done
