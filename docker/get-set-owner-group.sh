#!/bin/bash

# Get the owner and group of the hake-assessment directory so that we can
# set that inside a Docker container


facl_out=`getfacl -np /home/$USERNAME/github/pacific-hake/hake-assessment`

owner=`printf "$facl_out" | grep -Po '^# owner: \K[0-9]+$'`
group=`printf "$facl_out" | grep -Po '^# group: \K[0-9]+$'`

echo $owner
echo $group
