#!/bin/bash

# Run the base model retrospectives 1 through 6. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $BASE_MODEL is set in R/model-setup.R

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 1, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 1 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 2, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 2 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 3, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 3 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 4, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 4 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 5, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 5 complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 6, build_file = FALSE)" \
> /dev/null 2>&1; echo "Restrospective - 6 complete")

cp -R ~/hake-assessment/$MODELS_DIR/$BASE_MODEL/retrospectives/retro-0[1-6] \
~/hake-assessment/hakestore/$BASE_MODEL/retrospectives
echo "Copied Base model retrospectives 1-6 to S3 storage"
