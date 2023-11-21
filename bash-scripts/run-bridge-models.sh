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

models=(01-updated-ss-exe \
        02-add-new-catch \
        03-add-new-weight-at-age \
        04-add-survey-age-2-plus \
        05-add-survey-age-1 \
        06-add-fishery-ages)

type_path="02-bridging-models"

# The extra mcmc output is needed for the bridge models because there is
# an index fit comparison plot and the index fits are only in the extra mcmc
# output
run_extra_mcmc=TRUE
adapt_delta=0.95

. ./generic-run-models.sh
