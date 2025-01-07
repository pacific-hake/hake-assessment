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

models=( \
        # 00-update-ss3-exe \
        # 01-fix-catches \
        # 02-fix-weight-at-age \
        # # 03-fix-survey-1 \
        # # 04-fix-fishery-comps \
        10-add-catches \
        20-add-weight-at-age \
        # # 30-add-survey-2 \
        # # 31-add-survey-age-comps
        # 40-add-survey-1 \
        50-add-fishery-ages \
        )

type_path="02-bridging-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the posteriors 1 through `first` only
first=0

. ./generic-create-rds.sh
