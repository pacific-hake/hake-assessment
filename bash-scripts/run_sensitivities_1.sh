#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 96 CPUs for these 6
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.15_h_prior_mean_low \
        2022.01.16_h_fix_high \
        2022.01.17_sigmR_fix_low \
        2022.01.18_sigmR_fix_high \
        2022.01.20_M_0.2SD \
        2022.01.21_M_0.3SD)

extra_mcmc=TRUE
n_cores=16
adapt_delta=0.95
n_final=8000
warmup_final=250

. ./generic_run_models.sh
