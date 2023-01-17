#!/bin/bash

models=(#01-updated-ss-exe \
        #02-add-new-catch \
        #03-add-new-weight-at-age)
        04-add-survey-age-2-plus \
        05-add-survey-age-1 \
        06-add-fishery-ages)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="02-bridging-models"

# The extra mcmc output is needed for the bridge models because there is
# an index fit comparison plot and the index fits are only in the extra mcmc
# output
run_extra_mcmc=TRUE
adapt_delta=0.95

. ./generic-run-models.sh
