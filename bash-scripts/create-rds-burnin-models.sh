#!/bin/bash
#
# Build RDS files for the sensitivity models
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
03-burnin-1 \
04-burnin-50 \
05-burnin-100 \
06-burnin-150 \
07-burnin-200 \
#08-burnin-250 \
#09-burnin-300 \
#10-burnin-350 \
#11-burnin-400 \
#12-burnin-450 \
#13-burnin-500 \
)
type_path="05-test-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only
first=0

. ./generic-create-rds.sh
