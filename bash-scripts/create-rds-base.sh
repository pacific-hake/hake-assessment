#!/bin/bash

# Build RDS file for the base model
#
models=(01-base)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
# project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="01-base-models"
verbose=TRUE
overwrite=TRUE
keep_index_fit_posts=TRUE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only
first=0

. ./generic-create-rds.sh
