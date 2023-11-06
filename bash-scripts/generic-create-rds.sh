#!/bin/bash

# This script must be called from another script where the following variables have been set
[[ -z $project_path ]] && { echo "Variable 'project_path' has not been set, bailing out." ; exit 1; }
[[ -z $type_path ]] && { echo "Variable 'type_path' has not been set, bailing out." ; exit 1; }
[[ -z $models ]] && { echo "Variable 'models' has not been set, bailing out." ; exit 1; }
[[ -z $verbose ]] && { echo "Variable 'verbose' has not been set, bailing out." ; exit 1; }
[[ -z $overwrite ]] && { echo "Variable 'overwrite' has not been set, bailing out." ; exit 1; }
[[ -z $keep_index_fit_posts ]] && { echo "Variable 'keep_index_fit_posts' has not been set, bailing out." ; exit 1; }

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

models_path=$project_path/$models_path/$year_path/$version_path/$type_path
for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  model_path=$models_path/$model; \
  echo "Building RDS for $model in a subshell"; \
  echo; \
  Rscript -e " \
  setwd('$repo_path'); \
  devtools::load_all(); \
  # Use first = 2000 as argument to load first 2000 samples only
  create_rds_file('$model_path', \
                  verbose = $verbose, \
                  overwrite = $overwrite, \
                  keep_index_fit_posts = $keep_index_fit_posts)"; \
  echo; \
  echo "$model RDS complete"; \
  echo "") &
done
