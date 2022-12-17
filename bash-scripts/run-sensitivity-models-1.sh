#!/bin/bash

models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high \
        05-m-02-sd)

#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="03-sensitivity-models"

extra_mcmc=FALSE
adapt_delta=0.95

. ./generic_run_models.sh
