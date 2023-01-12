#!/bin/bash

models=(01-h-prior-mean-low \
        02-h-fix-high \
        03-sigma-r-fix-low \
        04-sigma-r-fix-high)

project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=FALSE
adapt_delta=0.95

. ./generic-run-models.sh
