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

# Build RDS files for some sensitivity models. Do not put more than 6 here
# as the machine will run out of memory

models=(06-base-stochastic-forecast-recruitment)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="05-test-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only for speed when testing
# the function `create_rds_file()` and the functions it calls
first=0

. ./generic-create-rds.sh
