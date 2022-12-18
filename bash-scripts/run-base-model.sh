#!/bin/bash

# Run base model MCMC using ADNUTS

#project_path=`Rscript -e "cat(dirname(here::here()))"`
project_path="/srv/hake"
models_path="models"
year_path=2023
version_path="01-version"
type_path="01-base_models"
model_name="01-base"

n_cores=16
n_final=8000
warmup_final=250
adapt_delta=0.95
extra_mcmc=FALSE

model_path=$project_path/$models_path/$year_path/$version_path/$type_path/$model_name
[ -d "$model_path" ] && echo "Directory $model_path exists, starting run loop." || \
  { echo "Error: Directory $model_path does not exist, bailing out." ; exit 1; }

# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$model_path', adapt_delta = $adapt_delta, extra_mcmc = $extra_mcmc, n_cores = n_cores)" \
> /dev/null 2>&1; echo "Base model MCMC complete")

# Run the base models catch-level calculations
# Environment variable $BASE_MODEL is set in R/model-setup.R
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$model_path', run_catch_levels = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model catch level calculations complete")

# Delete all files except the forecast.ss files
find ~/hake-assessment/$model_path/catch-levels -type f \
 ! -name 'forecast.ss' -delete

# Run the base models forecasts
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$model_name', run_catch_levels = FALSE, run_forecasts = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model forecasts complete")

# Delete the unnecessary files. Some are huge (eg. echoinput.sso can 3GB)
find ~/hake-assessment/$model_path/forecasts -type f \
 ! \( -name 'posteriors.sso' -o -name 'derived_posteriors.sso' -o -name 'forecast.ss' \) -delete

