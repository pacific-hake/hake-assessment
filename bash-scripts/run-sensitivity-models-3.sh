#!/bin/bash

models=(11-tv-select-phi-high \
        12-max-sel-age-5 \
        13-max-sel-age-7 \
        14-max-sel-age-8)

#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="03-sensitivity-models"

extra_mcmc=FALSE
adapt_delta=0.95

. ./generic_run_models.sh
