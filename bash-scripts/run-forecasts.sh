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

# Run base model forecasts only

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

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

# The following seems to introduce a leading newline when tried in 2026
repo_path=`Rscript -e "cat(here::here())"`
# Remove leading newline
repo_path=`echo $repo_path | tr -d '\n'`

models_path="/srv/hake/models"
# *Never* change `year_path` manually - See `get-assess-year.sh` call above
year_path=$assess_year
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

model_path=$models_path/$year_path/$version_path/$type_path/$model_name

[[ ! -d $model_path ]] && { echo "Error: Directory $model_path \
does not exist, bailing out." ; exit 1; }

# Run the base models forecasts
(trap 'kill 0' SIGINT; \
  echo; \
  Rscript -e " \
    setwd('$repo_path'); \
    devtools::load_all(); \
    run_forecasts(model_path = '$model_path')" \
> /dev/null 2>&1; \
  printf "\nBase model forecasts complete\n" \
)
