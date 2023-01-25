#!/bin/bash

models=(09-comp-weight-harmonic-mean \
        10-tv-select-phi-extra-low \
        11-tv-select-phi-low \
        12-tv-select-phi-high)

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
