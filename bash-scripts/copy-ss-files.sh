#!/bin/bash

test_path=/srv/hake/models/2024/02-version/05-test-models
base_path=/srv/hake/models/2024/02-version/01-base-models/01-base

# Copy these files into all subdirectories of the
# 15-500-samples-group directory
for d in $test_path/15-500-samples-group/*/; do
  cp $base_path/hake_control.ss "$d"
  cp $base_path/hake_data.ss "$d"
  cp $base_path/wtatage.ss "$d"
  cp $base_path/forecast.ss "$d"
  cp $base_path/starter.ss "$d"
done
