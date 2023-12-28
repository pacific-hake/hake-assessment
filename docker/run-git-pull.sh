#!/bin/bash

# Tell git to ignore permission chages as differences. If we don't do this,
# then pulling files with different permissions and changing the
# permissions later will cause git to report every file and directory in
# the repo as modified and they will have to be added again - we do not
# want that!
git config core.fileMode false

# Pull a repository. Used for a starting script for Docker container
# so that you are starting with the most current commit in the repo
# each time you start the container
git pull

# Need to change permission because when the container is launched, the repo
# is completely read-only to group and user (has 744 chmod code). It is
# writable to root only, but the Rstudio server is launched as user `rstudio`
# so it has to be changed.
chmod -R 777 .

# Return control back to the main Dockerfile script. If this is missing,
# then the ENTRYPOINT command that called this script will exit the
# container once this script is finished. Don't remove or the container will
# open and close again instantly when you launch it.
exec "$@"
