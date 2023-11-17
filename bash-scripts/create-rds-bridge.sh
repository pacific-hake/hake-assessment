#!/bin/bash

# Build RDS files for bridge models

models=(01-updated-ss-exe \
        02-add-new-catch \
        03-add-new-weight-at-age \
        04-add-survey-age-2-plus \
        05-add-survey-age-1 \
        06-add-fishery-ages)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="02-bridging-models"
verbose=FALSE
overwrite=TRUE
keep_index_fit_posts=FALSE
# If `first` is zero or negative, load all the posteriors. If any positive
# number, load the poateriors 1 through `first` only
first=0

. ./generic-create-rds.sh
