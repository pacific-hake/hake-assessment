#!/bin/bash

models=(13-max-sel-age-5 \
        14-max-sel-age-7 \
        15-max-sel-age-8 \
        16-zero-sum-constraint)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=TRUE
adapt_delta=0.95
small=TRUE
verbose=FALSE

. ./generic-run-models.sh
