#!/bin/bash

models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=FALSE
adapt_delta=0.95

. ./generic-run-models.sh
