#!/bin/bash

# Build RDS file for Brgie models
#
models=(01-base-cp)

#project  _path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
type_path="01-base-models"

. ./generic-create-rds.sh
