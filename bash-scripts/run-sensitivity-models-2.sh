#!/bin/bash

models=(05-m-02-sd \
        06-m-03-sd \
        07-m-hamel-prior \
        08-age-1-survey)

project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=FALSE
adapt_delta=0.95

. ./generic-run-models.sh
