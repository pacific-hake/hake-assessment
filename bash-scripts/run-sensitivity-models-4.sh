#!/bin/bash

models=(13-max-sel-age-5 \
        14-max-sel-age-7 \
        15-max-sel-age-8 \
        16-zero-sum-constraint)

project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=FALSE
adapt_delta=0.95

. ./generic-run-models.sh
