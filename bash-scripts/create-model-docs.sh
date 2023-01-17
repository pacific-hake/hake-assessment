#!/bin/bash

# Run this to create the model docs and copy the HTML files to the correct
# places on your system. This must be run everytime there are changes to the
# models directory structures, like if a new sensitivity is added. It will
# also update the timestamps for all model directories

repo_path=`Rscript -e "cat(here::here())"`
models_html="models/models.html"

Rscript -e "setwd('$repo_path'); \
            rmarkdown::render('models/models.Rmd'); \
            invisible(file.copy('$models_html', \
            '/srv/hake/$models_html', \
            overwrite = TRUE))"

echo
echo "Recreated model documentation and copied HTML files to /srv/hake/models "
echo "and its subdirectories."
echo
echo "Open file:///srv/hake/models/models.html in a web browser to start"

