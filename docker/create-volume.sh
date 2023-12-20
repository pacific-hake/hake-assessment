#!/bin/bash

# Creates a volume that is needed to link an outside drive with a drive inside
# a docker container. The drive outside the container to be linked to is
# defined here as the `device` in the call below. By default this is /srv.
# The name of the directory inside the container that this directory will
# be linked to is defined in the `docker run` call. See the
# `run-hake-docker.sh` script.
#
# This script must be run before the `run-hake-docker.sh` script is run.

docker volume create \
--driver local \
--opt type=none \
--opt device=/srv \
--opt o=bind \
hake_models