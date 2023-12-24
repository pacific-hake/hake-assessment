#!/bin/bash

# Copy the hake.pdf file from within a Docker container out to your
# local machine.
#
# The conatiner ID or cointainer name is passed as an argument to this script
# and the file will be copied to your current directory
# To find the container name and ID, run `docker ps` on the local machine

printf "Attempting to copy hake.pdf from the container to the local machine.\n\n"
if [ $# -eq 0 ]
  then
    echo "You must supply a container ID or name. Run `docker ps` for these"
    exit 1
else
  docker cp $1:/home/rstudio/github/pacific-hake/hake-assessment/doc/hake.pdf hake.pdf
  printf "\n"
fi