#!/bin/bash

# Build RDS file for the base model
# The path structure is as follows
# /srv/hake/models/2023/01-version/01-base-models/01-base/
#  ^   ^    ^      ^    ^          ^              ^
#  |   |    |      |    |          |              |
#  |   |    |      |    |          |              $models (in this script)
#  |   |    |      |    |          $type_path (in this script)
#  |   |    |      |    $version_path (in generic-create-rds.sh)
#  |   |    |      $year (in generic-create-rds.sh)
#  |   |    $models_path (in generic-create-rds.sh)
#  \  /
#   ||
#   $project_path (in this script)

models=(01-base)

type_path="01-base-models"
verbose=TRUE
overwrite=TRUE
keep_index_fit_posts=TRUE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only for speed when testing
# the function `create_rds_file()` and the functions it calls
first=0

. ./generic-create-rds.sh
