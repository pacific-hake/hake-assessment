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

models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high \
        05-m-02-sd \
        06-m-03-sd \
        07-m-hamel-prior \
        08-age-1-survey)

type_path="03-sensitivity-models"

run_extra_mcmc=TRUE
adapt_delta=0.95

. ./generic-run-models.sh
