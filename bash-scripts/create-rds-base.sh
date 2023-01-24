#!/bin/bash

# Build RDS file for Brgie models
#
models=(01-base)

# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="01-base-models"
small=FALSE
verbose=TRUE

. ./generic-create-rds.sh
