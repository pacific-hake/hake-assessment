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

models=( \
        09-comp-weight-harmonic-mean \
        10-tv-select-phi-extra-low \
        11-tv-select-phi-low \
        12-tv-select-phi-high \
        13-max-sel-age-5 \
        14-max-sel-age-7 \
        15-max-sel-age-8 \
        16-zero-sum-constraint \
        18-tv-maturity-182 \
        19-m-at-age \
        20-m-at-age-fixed \
        )

type_path="03-sensitivity-models"

run_extra_mcmc=TRUE
adapt_delta=0.95

. ./generic-run-models.sh
