# Creates the hake assessment Docker image. Built on the `cgrandin/ss-texlive`
# image (See subdirectory by the same name).
#
# To build, make sure you are in the `docker` directory of a cloned hake
# repository.
#
# Use one of the two build scripts:
# `build-hake-docker-scratch.sh`
# `build-hake-docker-cached.sh`
#
# Every so ofter you will want to clean up the images and containers.
# To do so follow these next few commands:
#
# To look at and then remove all images:
# docker images -a
# docker rmi $(docker images -a)
#
# To look at and then remove all non-running containers:
# docker ps -a
# docker rm $(docker ps -a)
#
# To run the container this build produces (cgrandin/hake), run the
# `run-hake-docker.sh` script and read the comments within it.

FROM cgrandin/ss-texlive
LABEL maintainer = "chris.grandin@gmail.com"

SHELL ["/bin/bash", "-c"]

WORKDIR $HOME

# Install R packages needed for the hake assessment
#
# Need to copy the .Renviron file which contains the GITHUB_PAT token so
# that the GitHub API limit is not reached while adding the GitHub packages.
# This file is not in the repository because it contains a secret key, so you
# need to generate your own token like this:
# usethis::usethis::create_github_token()
# Then edit the Renviron file (which may not exist) in this directory and
# add this line:
#
# GITHUB_PAT=<Generated Token from GitHub site>
COPY Renviron $HOME/.Renviron
COPY install_r_packages.R $HOME
RUN R -e "install.packages('here'); \
          source(here::here('install_r_packages.R'))"

# Delete remnants
RUN rm install_r_packages.R install_texlive.sh; rm -rf texlive_tmp

#.Rprofile contains the code that loads the hake package into the session
# automatically at startup
COPY Rprofile $HOME/.Rprofile
# Add some things to the history file so we don't have to type them out
# each time there is a new container
COPY Rhistory $HOME/.Rhistory

ENV HAKE=$HOME/github/pacific-hake/hake-assessment
RUN mkdir -p $HAKE

WORKDIR $HAKE
