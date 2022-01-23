#!/bin/bash

# Run bridging models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 96 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.01_newSSexe \
        2022.01.03_newcatchage \
        2022.01.05_updatesurvey \
        2022.01.06_newsurvey \
        2022.01.07_newwtatage \
        2022.01.09_age1index)

extra_mcmc=TRUE
n_cores=16
adapt_delta=0.95
n_final=8000
warmup_final=250

. ./generic_run_models.sh
