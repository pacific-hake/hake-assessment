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

num_warmup_samples=(1 50 100 150 200 250 300 350 400 450 500)

i=0
for model in ${models[@]}; do
  echo "${models[i]}"
  echo "${num_warmup_samples[i]}"
  i=$i+1
done