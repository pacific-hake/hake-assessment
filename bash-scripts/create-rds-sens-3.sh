#!/bin/bash

# Build RDS files for some sensitivity models. Do not put more than 6 here
# as the machine will run out of memory

models=(13-max-sel-age-5 \
        14-max-sel-age-7 \
        15-max-sel-age-8 \
        16-zero-sum-constraint \
        17-m-hamel-prior-updated)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="03-sensitivity-models"
verbose=FALSE
overwrite=TRUE

. ./generic-create-rds.sh
