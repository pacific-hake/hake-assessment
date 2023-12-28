#!/bin/bash

# Run a hake container, and link the models directory on your machine to
# `/srv` inside the container so that nothing has to be changed in the
# hake assessment repository code in order to run it on a different machine.
#
# See the vignette `vignettes/run-docker-container.Rmd` for details

if [[ "$HOSTNAME" == "hake-precision" ]]; then
  # Linux hake server
  #SRV_DIR=/srv
  SRV_DIR=/srv
else
  # $USERNAME below is output of echo $USERNAME in your bash shell on your
  # local machine
  if [[ "$USERNAME" == "grandin" ]]; then
    SRV_DIR="/d/WORK/A_Species/Hake/srv"
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
