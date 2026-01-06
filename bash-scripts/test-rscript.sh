#!/bin/bash

repo_path=`Rscript -e "cat(here::here())"`
repo_path=`echo $repo_path | tr -d '\n'`

echo "The repo path in BASH is: $repo_path"
echo

Rscript -e " \
  print('The repo HERE path is: $repo_path'); \
  setwd('$repo_path'); \
  print(paste0('The current dir inside rscript is ', getwd())); \
"