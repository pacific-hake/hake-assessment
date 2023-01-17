#!/bin/bash

# Run base model MCMC using ADNUTS
# To fix the permissions for the hake group:
# chmod -R u+rwx,g+rwx,o= 01-base-models

# Start timer
SECONDS=0

repo_path=`Rscript -e "cat(here::here())"`
# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#models+path=$repo_path/models
models_path="/srv/hake/models"
year_path=2023
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

num_chains=16
num_samples=8000
num_warmup_samples=250
adapt_delta=0.95
run_extra_mcmc=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name
[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not exist, bailing out." ; exit 1; }

(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); source('R/all.R'); \
run_adnuts_timed('$model_path', adapt_delta = $adapt_delta, run_extra_mcmc = $run_extra_mcmc, \
                 num_chains = $num_chains, num_samples = $num_samples, \
                 num_warmup_samples = $num_warmup_samples)" \
> /dev/null 2>&1; echo "Base model MCMC complete")

# Run the base models catch-level calculations
(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); source('R/all.R'); \
build_rds('$model_path', run_catch_levels = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model catch level calculations complete")

# Delete all files except the forecast.ss files
find $model_path/catch-levels -type f \
! -name 'forecast.ss' -delete

# Run the base models forecasts
(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); source('R/all.R'); \
build_rds('$model_path', run_catch_levels = FALSE, run_forecasts = TRUE, build_file = FALSE)" \
> /dev/null 2>&1; echo "Base model forecasts complete")

# Delete the unnecessary files. Some are huge (eg. echoinput.sso can 3GB)
find $model_path/forecasts -type f \
! \( -name 'posteriors.sso' -o -name 'derived_posteriors.sso' -o -name 'forecast.ss' \) -delete

# Build the RDS file
(trap 'kill 0' SIGINT; Rscript -e "setwd('$repo_path'); source('R/all.R'); \
create_rds_file('$model_path')" \
> /dev/null 2>&1; echo "Base model RDS file created")

ELAPSED="Script runtime: $(($SECONDS / 3600)) hrs $((($SECONDS / 60) % 60)) min $(($SECONDS % 60)) sec"
echo $ELAPSED
