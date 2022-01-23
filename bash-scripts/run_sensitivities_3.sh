#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 64 CPUs for these 4
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.43_maxSel_Age5 \
        2022.01.44_maxSel_Age7 \
        2022.01.45_maxSel_Age8 \
        2022.01.100_zerosumcontraint)

extra_mcmc=TRUE
n_cores=16
adapt_delta=0.95
n_final=8000
warmup_final=250

. ./generic_run_models.sh
