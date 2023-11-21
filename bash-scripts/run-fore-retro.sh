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

# Run base model catch-levels, forecasts, and retrospectives
# Make sure to change values in `run-forecasts.sh` and `run-restrospectives.sh`
# for the year before running

. ./run-forecasts.sh
. ./run-retrospectives.sh 1 2 3 4
. ./run-retrospectives.sh 5 6 7
. ./run-retrospectives.sh 8 9 10

printf "\nForecasts amd retrospectives completed\n"
