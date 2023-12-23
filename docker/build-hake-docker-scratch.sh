#!/bin/bash

# Build the `ubuntu-essentials` image which resides in the `ubuntu-essentials`
# directory from the docker directory
#
# Sometimes there are errors which are caused by slow connections
# (server side). These mostly seem to happen when installing TexLive. If
# that happens, just run this script again. There doesn't seem to be much
# we can do about it.
docker build --no-cache -t cgrandin/ubuntu-essentials ubuntu-essentials

docker build --no-cache -t cgrandin/hake .