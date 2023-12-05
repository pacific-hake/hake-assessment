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

# Build RDS files for bridge models

models=(01-updated-ss-exe \
        02-add-new-catch \
        03-add-new-weight-at-age \
        04-add-survey-age-2-plus \
        05-add-survey-age-1 \
        06-add-fishery-ages)

type_path="02-bridging-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only
first=0

. ./generic-create-rds.sh
