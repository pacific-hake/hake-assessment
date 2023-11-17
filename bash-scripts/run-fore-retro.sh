#!/bin/bash

# Run base model catch-levels, forecasts, and retrospectives
# Make sure to change values in `run-forecasts.sh` and `run-restrospectives.sh`
# for the year before running

. ./run-forecasts.sh
. ./run-retrospectives.sh 1 2 3
. ./run-retrospectives.sh 4 5 6 7
. ./run-retrospectives.sh 8 9 10

printf "\nForecasts amd retrospectives completed\n"
