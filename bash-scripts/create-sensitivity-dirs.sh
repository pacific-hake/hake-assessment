#!/bin/bash

# The path structure is as follows
# /srv/hake/models/2023/01-version/01-base-models/01-base/
#  ^   ^    ^      ^    ^          ^              ^
#  |   |    |      |    |          |              |
#  |   |    |      |    |          |              $model_name
#  |   |    |      |    |          $type_path
#  |   |    |      |    $version_path
#  |   |    |      $year
#  |   |    $models_path
#  \  /
#   ||
#   $project_path

repo_path=`Rscript -e "cat(here::here())"`
version_path="/srv/hake/models/2024/02-version"
sens_path="03-sensitivity-models"

Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  create_sens_dirs(dir_version = '$version_path', \
                   sens_dir_name = '$sens_path')"

chmod -R 755 $version_path/$sens_path
