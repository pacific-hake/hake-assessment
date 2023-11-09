#!/bin/bash

Rscript -e " \
devtools::load_all(); \
parallelly::supportsMulticore(); \
run_ct_levels(model_path = \
    '/srv/hake/models/2023/01-version/01-base-models/01-base')"
