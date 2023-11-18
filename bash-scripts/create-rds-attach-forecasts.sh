#!/bin/bash
#
# Load and attach forecasts to a RDS file as a list element called `fortecasts`
# and overwrite the original RDS file

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
  printf "\nAttaching foreecasts to base model RDS file\n \
  (in parallel if on Linux/Mac)\n"; \
  Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  create_rds_attach_forecasts(model_path = '$model_path', \
                              verbose = TRUE)"; \
  printf "\nFinished attaching foreecasts to base model RDS file\n"; \
)