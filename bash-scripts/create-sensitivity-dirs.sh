#!/bin/bash

repo_path=`Rscript -e "cat(here::here())"`
version_path="/srv/hake/models/2023/01-version"
sens_path="03-sensitivity-models"

Rscript -e " \
  setwd('$repo_path'); \
  source('R/all.R'); \
  create_sens_dirs(dir_version = '$version_path', \
                   sens_dir_name = '$sens_path')"

chmod -R 777 $version_path/$sens_path
