#!/bin/bash
#
# Create an RDS file for each retrospective model found in the
# `$model_path/retrospectives` directory. The `retrospectives` part
# of the path is appended  in the `create_rds_files_retro()` function

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

repo_path=`Rscript -e "cat(here::here())"`
# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#project  path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"

models_path="models"
# *Never* change `year_path` manually here. Instead, to assign a manual year,
# add it as an argument to the `get-assess-year.sh` call above
year_path=$assess_year
version_path="01-version"
type_path="01-base-models"
model="01-base"

model_path=$project_path/$models_path/$year_path/$version_path/$type_path/$model
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not \
exist, bailing out." ; exit 1; }

(trap 'kill 0' SIGINT; \
  printf "\nCreating RDS files for base model retrospectives\n"; \
  Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  create_rds_files_retro(model_path = '$model_path', \
                         verbose = TRUE, \
                         overwrite = TRUE)"; \
  printf "\nFinished creating RDS file for base model retrospectives\n"; \
)