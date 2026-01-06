#!/bin/bash

# This script must be called from another script where the following
# variables have been set
[[ -z $type_path ]] && { printf "\nVariable 'type_path' has not been set, \
bailing out.\n" ; exit 1; }
[[ -z $models ]] && { printf "\nVariable 'models' has not been set, bailing \
out.\n" ; exit 1; }
# These are the arguments to the `create_rds_file()` function call below
[[ -z $verbose ]] && { printf "\nVariable 'verbose' has not been set,\
bailing out.\n" ; exit 1; }
[[ -z $overwrite ]] && { printf "\nVariable 'overwrite' has not been set, \
bailing out.\n" ; exit 1; }
[[ -z $keep_index_fit_posts ]] && { printf "\nVariable 'keep_index_fit_posts' \
has not been set, bailing out.\n" ; exit 1; }
[[ -z $first ]] && { printf "\nVariable 'first' has not been set, bailing \
out.\n" ; exit 1; }

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

# The following seems to introduce a leading newline when tried in 2026
repo_path=`Rscript -e "cat(here::here())"`
# Remove leading newline
repo_path=`echo $repo_path | tr -d '\n'`
`
# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

models_path="models"
# *Never* change `year_path` manually here. Instead, to assign a manual year,
# add it as an argument to the `get-assess-year.sh` call above
year_path=$assess_year
version_path="02-version"

# In the following assignment, only `$type_path ` comes from the calling script
models_path=$project_path/$models_path/$year_path/$version_path/$type_path
[[ ! -d $models_path ]] && { printf "\nError: Directory $models_path does not \
exist, bailing out.\n" ; exit 1; }

# `$models` comes from the calling script
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
