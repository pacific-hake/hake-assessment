#!/bin/bash

# Copy SS3 input files from the base model folder to a test model folder

base_model_path="/srv/hake/models/2023/01-version/01-base-models/01-base"
test_model_path="/srv/hake/models/2024/01-version/05-test-models/04-time-varying-weight-at-age"

files=(hake_data.ss \
       hake_control.ss \
       starter.ss \
       forecast.ss \
       wtatage.ss)

[[ ! -d $base_model_path ]] && { printf "\nBase model directory does \
not exist, bailing out.\n" ; exit 1; }

[[ ! -d $test_model_path ]] && { printf "\nTest model directory does \
not exist, bailing out.\n" ; exit 1; }

for fn in ${files[@]}; do
  cp "$base_model_path/$fn" "$test_model_path/$fn"
done
