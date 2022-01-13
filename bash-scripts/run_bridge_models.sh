#!/bin/bash

# Run bridging models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 96 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.01_newSSexe', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.01_newSSexe MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.03_newcatchage', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.03_newcatchage MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.05_updatesurvey', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.05_updatesurvey MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.06_newsurvey', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.06_newsurvey MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.07_newwtatage', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.07_newwtatage MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.09_age1index', adapt_delta = 0.95, n_cores = 16)" \
> /dev/null 2>&1; echo "2022.01.09_age1index MCMC complete")

# Copy all the output to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/2022.01.01_newSSexe \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.01_newSSexe
echo "Copied 2022.01.01_newSSexe model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.03_newcatchage \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.03_newcatchage
echo "Copied 2022.01.03_newcatchage model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.05_updatesurvey \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.05_updatesurvey
echo "Copied 2022.01.05_updatesurvey model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.07_newwtatage \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.07_newwtatage
echo "Copied 2022.01.07_newwtatage model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.09_age1index \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.09_age1index
echo "Copied 2022.01.09_age1index model output to S3 storage"
