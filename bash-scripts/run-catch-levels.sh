#!/bin/bash

Rscript -e " \
  setwd(here::here()); \
  source('R/all.R'); \
  build_rds('2022.01.10_base_v2', run_catch_levels = TRUE, build_file = FALSE)"