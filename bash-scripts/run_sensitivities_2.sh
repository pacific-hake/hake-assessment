#!/bin/bash

# Run sensitivity models MCMCs using adnuts on AWS. Each needs 16 CPUs, so need 80 CPUs for these 5
# Environment variable $MODELS_DIR is set in aws/install_hake.sh

models=(2022.01.22_M_hamel_prior \
        2022.01.24_compWeight_HarmonicMean \
        2022.01.27_tvSelect_phi_extralow \
        2022.01.28_tvSelect_phi_low \
        2022.01.29_tvSelect_phi_high)

extra_mcmc=TRUE
n_cores=16
adapt_delta=0.95
n_final=8000
warmup_final=250

. ./generic_run_models.sh
