#!/bin/bash

models=(06-m-03-sd \
        07-age-1-survey \
        08-comp-weight-harmonic-mean \
        09-tv-select-phi-extra-low \
        10-tv-select-phi-low)

#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="03-sensitivity-models"

extra_mcmc=FALSE
adapt_delta=0.95

. ./generic_run_models.sh
