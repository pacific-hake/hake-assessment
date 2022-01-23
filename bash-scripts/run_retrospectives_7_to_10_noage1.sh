#!/bin/bash

# Run model retrospectives 7 through 10. Each one needs 16 CPUs to run at
# optimal speed so they need 96 CPUS
# Environment variable $NO_AGE1_MODEL is set in aws/install_hake.sh

# Required the MLE run in the root part of the model directory have all SS files present.
# To do that, copy from S3 hakestore:
# cp hakestore/models/$NO_AGE1_MODEL/* models/$NO_AGE1_MODEL

MODEL=2022.01.23_age1Survey
years=(7 8 9 10)
n_cores=16

. ./generic_run_retrospectives.sh

