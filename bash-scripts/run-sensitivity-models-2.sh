#!/bin/bash

models=(05-m-02-sd \
        06-m-03-sd \
        07-m-hamel-prior \
        17-m-hamel-prior-updated)
        #08-age-1-survey)

type_path="03-sensitivity-models"

run_extra_mcmc=TRUE
adapt_delta=0.95

. ./generic-run-models.sh
