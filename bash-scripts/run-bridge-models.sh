#!/bin/bash

models=(01-updated-ss-exe \
        02-add-new-catch \
        03-add-new-weight-at-age \
        04-add-survey-age-2-plus \
        05-add-survey-age-1 \
        06-add-fishery-ages)

#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="02-bridging-models"

extra_mcmc=FALSE
adapt_delta=0.95

. ./generic_run_models.sh
