#!/bin/bash

# Build RDS files for Bridge models
#
models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high \
        05-m-02-sd \
        06-m-03-sd \
        07-m-hamel-prior \
        08-age-1-survey \
        09-comp-weight-harmonic-mean \
        10-tv-select-phi-extra-low \
        11-tv-select-phi-low \
        12-tv-select-phi-high \
        13-max-sel-age-5 \
        14-max-sel-age-7 \
        15-max-sel-age-8 \
        16-zero-sum-constraint \
        17-m-hamel-prior-updated)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="03-sensitivity-models"
 # Create small RDS by only having a subset of the extra MCMC output
small=TRUE
verbose=FALSE

. ./generic-create-rds.sh
