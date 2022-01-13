#!/bin/bash

# Build RDS files from model output
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.01_newSSexe')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.03_newcatchage')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.05_updatesurvey')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.06_newsurvey')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.07_newwtatage')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.09_age1index')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.15_h_prior_mean_low')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.16_h_fix_high')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.17_sigmR_fix_low')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.18_sigmR_fix_high')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.20_M_0.2SD')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.21_M_0.3SD')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.22_M_hamel_prior')" > /dev/null 2>&1) &

# This one takes longer because it has forecasts, etc
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.23_age1Survey')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.24_compWeight_HarmonicMean')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.27_tvSelect_phi_extralow')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.28_tvSelect_phi_low')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.29_tvSelect_phi_high')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.43_maxSel_Age5')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.44_maxSel_Age7')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.45_maxSel_Age8')" > /dev/null 2>&1) &

(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('2022.01.100_zerosumcontraint')" > /dev/null 2>&1) &

# The base model will take the longest to build, so wait for it before copying
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); \
build_rds('$BASE_MODEL')" > /dev/null 2>&1)

# Copy the RDS files into S3 storage, preserving directory structure
find ~/hake-assessment/$MODELS_DIR -type f -name '*.rds' -exec cp --parents {} \
~/hake-assessment/hakestore/$MODELS_DIR \;

echo "Copied RDS files to S3 storage"
