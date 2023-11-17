#!/bin/bash
#
# Create an RDS file for each retrospective model found in the
# `$model_path/retrospectives` directory. The `retrospectives` part
# of the path is appended  in the `create_rds_files_retro()` function

model_path="/srv/hake/models/2023/01-version/01-base-models/01-base"

(trap 'kill 0' SIGINT; \
  echo; \
  Rscript -e " \
  setwd('$repo_path'); \
  suppressPackageStartupMessages(devtools::load_all()); \
  create_rds_files_retro(model_path = $model_path, \
                         verbose = TRUE, \
                         overwrite = TRUE)"; \
)