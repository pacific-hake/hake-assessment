#!/bin/bash

models=(03-burnin-1 \
        04-burnin-50 \
        05-burnin-100 \
        06-burnin-150 \
        07-burnin-200 \
        08-burnin-250 \
        09-burnin-300 \
        10-burnin-350 \
        11-burnin-400 \
        12-burnin-450 \
        13-burnin-500)

#num_warmup_samples=(1 50 100 150 200 250 300 350 400 450 500)

for model in ${models[@]}; do
  echo "Model folder name = $model";
  num_warmup_samples=`echo "$model" | grep -Po '\d+$'`
  echo "Burn-in = $num_warmup_samples";
done