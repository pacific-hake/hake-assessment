#!/bin/bash

# Run the base model retrospectives 7 through 10. Each one needs 16 CPUs to run at
# optimal speed so they need 64 CPUS
# Environment variable $BASE_MODEL is set in aws/install_hake.sh

# Required the MLE run in the root part of the model directory have all it files present
# cp hakestore/models/$BASE_MODEL/* models/$BASE_MODEL

MODEL=2022.01.10_base
years=(7 8 9 10)
n_cores=16

. ./generic_run_retrospectives.sh
