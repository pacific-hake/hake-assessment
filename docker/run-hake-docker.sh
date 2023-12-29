#!/bin/bash

# Run a hake container, and link the models directory on your machine to
# `/srv` inside the container so that nothing has to be changed in the
# hake assessment repository code in order to run it on a different machine.
#
# See the vignette `vignettes/run-docker-container.Rmd` for details

if [[ "$HOSTNAME" == "hake-precision" ]]; then
  # Linux hake server
  SRV_DIR="/srv"
  # Assumes everyone has their 'hake-assessment' repository in the same
  # location within their '/home' directory
  REPO_DIR="$HOME/github/pacific-hake/hake-assessment"
else
  # **Machine is assumed to be MS WINDOWS**
  # $USERNAME below is output of echo $USERNAME in your bash shell on your
  # local machine
  if [[ "$USERNAME" == "grandin" ]]; then
    SRV_DIR="/d/WORK/A_Species/Hake/srv"
    REPO_DIR="/d/github/pacific-hake/hake-assessment"
  elif [[ "$USERNAME" == "kelli" ]]; then
    SRV_DIR="/c/srv"
    REPO_DIR="/c/github/pacific-hake/hake-assessment"
  elif [[ "$USERNAME" == "aaron" ]]; then
    SRV_DIR="/c/srv"
    REPO_DIR="/c/github/pacific-hake/hake-assessment"
  elif [[ "$USERNAME" == "andy" ]]; then
    SRV_DIR="/c/srv"
    REPO_DIR="/c/github/pacific-hake/hake-assessment"
  else
    printf "You need to set up the linkages between the directory on your\n \
            local machine and the /srv directory and hake-assessment\n \
            repository directory inside the Docker container.\n \
            Edit the 'run-hake-docker.sh' file.\n\n"
    exit 1
  fi
fi

if [ ! -d "$SRV_DIR" ]; then
  printf "Directory '$SRV_DIR' does not exist on your local machine, Edit\n \
          the 'run-hake-docker.sh' file and try again.\n\n"
  exit 1
fi

if [ ! -d "$REPO_DIR" ]; then
  printf "Directory '$REPO_DIR' does not exist on your local machine, Edit\n \
          the 'run-hake-docker.sh' file and try again.\n\n"
  exit 1
fi

docker run \
  -it \
  --rm \
  -p 8787:8787 \
  -e PASSWORD=a \
  --mount type=bind,source=$SRV_DIR,target=/srv \
  --mount type=bind,source=$REPO_DIR,target=/home/rstudio/hake-assessment \
  cgrandin/hake \
  bash
