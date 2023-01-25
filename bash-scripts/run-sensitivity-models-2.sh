#!/bin/bash

models=(05-m-02-sd \
        06-m-03-sd \
        07-m-hamel-prior \
        17-m-hamel-prior-updated)
        #08-age-1-survey)

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
