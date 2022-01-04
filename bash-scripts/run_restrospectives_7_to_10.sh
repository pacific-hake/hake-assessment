#!/bin/bash

# Run the base model retrospectives 7 through 10. Each one needs 16 CPUs to run at
# optimal speed so they need 64 CPUS
# Environment variable $BASE_MODEL is set in R/model-setup.R

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 7, build_file = FALSE)" \
> /dev/null 2>&1)

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 8, build_file = FALSE)" \
> /dev/null 2>&1)

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 9, build_file = FALSE)" \
> /dev/null 2>&1)

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL', run_retrospectives = TRUE, retrospective_yrs = 10, build_file = FALSE)" \
> /dev/null 2>&1)
