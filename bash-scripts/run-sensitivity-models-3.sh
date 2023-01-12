#!/bin/bash

models=(09-comp-weight-harmonic-mean \
        10-tv-select-phi-extra-low \
        11-tv-select-phi-low \
        12-tv-select-phi-high)

project_path="/srv/hake"

type_path="03-sensitivity-models"

run_extra_mcmc=FALSE
adapt_delta=0.95

. ./generic-run-models.sh
