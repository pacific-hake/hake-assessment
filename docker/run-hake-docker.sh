#!/bin/bash
#
# Run a hake container, and link the models directory on your machine to
# `/srv` inside the container so that nothing has to be changed in the
# hake assessment repository code.
#
# **You are required to run the `create-volume.sh` script prior to starting
# the container with this script.**
#
# Method 1 - To start the container is a bash shell (for running model
# scripts, etc) but als have RStudio server available run this command:
#
# docker run -it -p 8787:8787 -e PASSWORD=a -v hake_models:/srv cgrandin/hake bash
#
# Once in the container, you can launch RStudio server by running the
# following command:
#
# /init
#
# To return to the bash shell, you must press the exit button on the top right
# of the Rstudio GUI in the browser
#
# Method 2 - To start the container with RStudio server running, but no Bash
# shell available, run this (slightly shorter) command:
#
# docker run -p 8787:8787 -e PASSWORD=a -v hake_models:/srv cgrandin/hake
#
# Both methods: To access the RStudio sever, open a browser and type the
# following in the URL bar:
#
# localhost:8787
#
# This will go to an RStudio sign-in page. Enter the user as "rstudio" and
# the Password as "a". You can change that password if you like by changing
# `PASSWORD` in the `docker run` call below.

# See Vignette `vignettes/run-docker-container.Rmd` for what to do here.
# Example below: `SRV_DIR` in Windows on grandin's machine, is going to be
# linked to `/srv` in the Docker container
#
# SRV_DIR="d:/WORK/A_Species/Hake/srv"
# On the server (default):
if [[ "$HOSTNAME" == "hake-precision" ]]; then
  # Linux hake server
  #SRV_DIR=/srv
  SRV_DIR=/srv
else
  # $USERNAME below is output of echo $USERNAME in your bash shell
  if [[ "$USERNAME" == "grandin" ]]; then
    SRV_DIR="d:/WORK/A_Species/Hake/srv"
  elif [[ "$USERNAME" == "kelli" ]]; then
    SRV_DIR=/srv
  elif [[ "$USERNAME" == "aaron" ]]; then
    SRV_DIR=/srv
  elif [[ "$USERNAME" == "andy" ]]; then
    SRV_DIR=/srv
  else
    printf "You need to set up the linkage between a directory on your\n \
            local machine and the /srv directory inside the Docker container.\n \
            Edit the 'run-hake-docker.sh' file.\n\n"
    exit 1
  fi
fi

if [ ! -d "$SRV_DIR" ]; then
  printf "Directory '$SRV_DIR' does not exist, Edit the 'run-hake-docker.sh'\n \
          file and try again.\n\n"
  exit 1
fi

docker run -it -p 8787:8787 -e PASSWORD=a --mount \
  type=bind,source=$SRV_DIR,target=/srv cgrandin/hake bash
