#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 64 CPUs for these 4
# Environment variable $MODELS_DIR is set in aws/install_hake.sh
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.43_maxSel_Age5', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.43_maxSel_Age5 MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.44_maxSel_Age7', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.44_maxSel_Age7 MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.45_maxSel_Age8', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.45_maxSel_Age8 MCMC complete") &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
run_adnuts('$MODELS_DIR/2022.01.100_zerosumcontraint', adapt_delta = 0.95, extra_mcmc = FALSE)" \
> /dev/null 2>&1; echo "2022.01.100_zerosumcontraint MCMC complete")

# Copy all the output to the persistent S3 drive 'hakestore'
cp -R ~/hake-assessment/$MODELS_DIR/2022.01.43_maxSel_Age5 \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.43_maxSel_Age5
echo "Copied 2022.01.43_maxSel_Age5 model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.44_maxSel_Age7 \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.44_maxSel_Age7
echo "Copied 2022.01.44_maxSel_Age7 model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.45_maxSel_Age8 \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.45_maxSel_Age8
echo "Copied 2022.01.45_maxSel_Age8 model output to S3 storage"

cp -R ~/hake-assessment/$MODELS_DIR/2022.01.100_zerosumcontraint \
~/hake-assessment/hakestore/$MODELS_DIR/2022.01.100_zerosumcontraint
echo "Copied 2022.01.100_zerosumcontraint model output to S3 storage"
