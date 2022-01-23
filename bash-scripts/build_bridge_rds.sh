#!/bin/bash

# Build RDS file for Brgie models
#
models=(2022.01.01_newSSexe 2022.01.03_newcatchage 2022.01.05_updatesurvey \
        2022.01.06_newsurvey 2022.01.07_newwtatage 2022.01.09_age1index)

. ./generic_build_rds.sh
