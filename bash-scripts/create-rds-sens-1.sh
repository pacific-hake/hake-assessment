#!/bin/bash

# Build RDS files for some sensitivity models. Do not put more than 6 here
# as the machine will run out of memory

models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high \
        05-m-02-sd \
        06-m-03-sd)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="03-sensitivity-models"
verbose=FALSE
overwrite=TRUE

. ./generic-create-rds.sh
