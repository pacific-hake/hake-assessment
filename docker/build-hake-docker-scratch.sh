#!/bin/bash

# Build the hake image from scratch. First builds the dependent images,
# which reside in subdirectories.
#
# Sometimes there are errors which are caused by slow connections
# (server side). These mostly seem to happen when installing TexLive. If
# that happens, just run the cahced version of this script -
# `build-hake-docker-cached.sh`

docker build --no-cache -t cgrandin/ubuntu-essentials ubuntu-essentials

docker build --no-cache -t cgrandin/ss-texlive ss-texlive

docker build --no-cache -t cgrandin/hake .