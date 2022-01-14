#!/bin/bash

# Run the base model retrospectives 7 through 10. Each one needs 16 CPUs to run at
# optimal speed so they need 64 CPUS
# Environment variable $NO_AGE1_MODEL is set in R/model-setup.R

# Required the MLE run in the root part of the model directory have all it files present
# cp hakestore/models/$NO_AGE1_MODEL/* models/$NO_AGE1_MODEL

NO_AGE1_MODEL=2022.01.23_age1Survey
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 7, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 7 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 8, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 8 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 9, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 9 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_retrospectives('$MODELS_DIR/$NO_AGE1_MODEL', retrospective_yrs = 10, n_cores = 16)" \
> /dev/null 2>&1; echo "Restrospective - 10 complete")

cp -R ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives/retro-0[7-9] \
~/hake-assessment/hakestore/$NO_AGE1_MODEL/retrospectives
cp -R ~/hake-assessment/$MODELS_DIR/$NO_AGE1_MODEL/retrospectives/retro-10 \
~/hake-assessment/hakestore/$NO_AGE1_MODEL/retrospectives
echo "Copied Base model retrospectives 7-10 to S3 storage"
