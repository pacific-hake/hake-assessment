#!/bin/bash

# Pull a repository. Used for a starting script for Docker container
# so that you are starting with the most current commit in the repo
# each time you start the container

git pull

# Return control back to the main Dockerfile script. If this is missing,
# then the ENTRYPOINT command that called this script will exit the
# container once this script is finished. Don't remove or the container will
# open and close again instantly when you launch it.
exec "$@"
