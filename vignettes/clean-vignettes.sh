#!/bin/bash
#
# Remove all *.R and *.htmlo files from the current directory.
# These are generated by `rmarkdown::render()` by default, so
# they are the byproduct of creating vignette html pages.
#
rm *.R
rm *.html
