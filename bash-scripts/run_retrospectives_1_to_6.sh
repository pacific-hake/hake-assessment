#!/bin/bash

# Run model retrospectives 1 through 6. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $BASE_MODEL is set in aws/install_hake.sh

# Required the MLE run in the root part of the model directory have all SS files present.
# To do that, copy from S3 hakestore:
# cp hakestore/models/$BASE_MODEL/* models/$BASE_MODEL

MODEL=2022.01.10_base
years=(1 2 3 4 5 6)
n_cores=16

. ./generic_run_retrospectives.sh
