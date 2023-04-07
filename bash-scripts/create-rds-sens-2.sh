#!/bin/bash

# Build RDS files for some sensitivity models. Do not put more than 6 here
# as the machine will run out of memory

models=(07-m-hamel-prior \
        08-age-1-survey \
        09-comp-weight-harmonic-mean \
        10-tv-select-phi-extra-low \
        11-tv-select-phi-low \
        12-tv-select-phi-high)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="03-sensitivity-models"
verbose=FALSE
overwrite=TRUE

. ./generic-create-rds.sh
