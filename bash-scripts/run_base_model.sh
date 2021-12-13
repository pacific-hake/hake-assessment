#!/bin/bash
(trap 'kill 0' SIGINT; Rscript -e "setwd(here::here()); source('R/all.R'); run_adnuts('$MODEL_DIR/2021.00.04_base_v1', adapt_delta = 0.9)" > adnuts.log 2>&1)
