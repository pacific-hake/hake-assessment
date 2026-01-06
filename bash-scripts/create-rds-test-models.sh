#!/bin/bash

# Each of these directories are located in the 05-test-models directory
dirs=( \
15-500-samples-group \
16-1000-samples-group \
17-2500-samples-group \
18-5000-samples-group \
19-10000-samples-group \
)
# All of these are directories are located in every directories above ($dirs)
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
#project  path=`Rscript -e "cat(dirname(here::here()))"`
type_path="05-test-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only for speed when testing
# the function `create_rds_file()` and the functions it calls
first=0

# The following seems to introduce a leading newline when tried in 2026
repo_path=`Rscript -e "cat(here::here())"`
# Remove leading newline
repo_path=`echo $repo_path | tr -d '\n'`

project_path="/srv/hake"
models_path="models"
. ./get-assess-year.sh
year_path=$assess_year
version_path="02-version"

# `$models` comes from the calling script
for dir in ${dirs[@]}; do
(trap 'kill 0' SIGINT; \
  models_path=$project_path/$models_path/$year_path/$version_path/$type_path/$dir
  [[ ! -d $models_path ]] && { printf "\nError: Directory $models_path does not \
  exist, bailing out.\n" ; exit 1; }
  printf "\nCreating RDS file in directory\n$type_path\n"; \
  for model in ${models[@]}; do
    (trap 'kill 0' SIGINT; \
    model_path=$models_path/$model; \
    printf "\nCreating RDS file in directory\n$model\n"; \
    Rscript -e " \
    setwd('$repo_path'); \
    suppressPackageStartupMessages(devtools::load_all()); \
    create_rds_file(model_path = '$model_path', \
                    verbose = $verbose, \
                    overwrite = $overwrite, \
                    keep_index_fit_posts = $keep_index_fit_posts, \
                    first = $first)"; \
    printf "\nCreation of RDS file in directory\n$model\nfinished\n"; \
    ) &
  done

  printf "\nCreation of RDS files in subdirectory\n$type_path\nfinished\n"; \
  ) &
done
