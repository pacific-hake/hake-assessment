#!/bin/bash

# Run base model catch-levels, forecasts, and retrospectives
# Make sure to change values in `run-forecasts.sh` and `run-restrospectives.sh`
# for the year before running

. ./run-forecasts.sh
. ./run-retrosectives.sh 1 2 3 4
. ./run-retrosectives.sh 5 6 7
. ./run-retrosectives.sh 8 9 10

printf "\nForecasts amd retrospectives completed\n"
