#!/bin/bash

models=(01-new-ss-exe \
        02-new-catch-age \
        03-update-survey \
        04-age-1-index \
        05-new-wt-at-age)

#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="02-bridging-models"

extra_mcmc=FALSE
adapt_delta=0.95

. ./generic_run_models.sh
