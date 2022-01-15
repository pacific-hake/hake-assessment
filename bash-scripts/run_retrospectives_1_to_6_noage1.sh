#!/bin/bash

# Run the base model retrospectives 1 through 6. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $NO_AGE1_MODEL is set in R/model-setup.R

# Required the MLE run in the root part of the model directory have all it files present
# cp hakestore/models/$NO_AGE1_MODEL/* models/$NO_AGE1_MODEL

NO_AGE1_MODEL=2022.01.23_age1Survey
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 1, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 1 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 2, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 2 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 3, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 3 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 4, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 4 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 5, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 5 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 6, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 6 complete")

cp -R ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives/retro-0[1-6] \
~/hake-assessment/hakestore/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives
echo "Copied Base model retrospectives 1-6 to S3 storage"
