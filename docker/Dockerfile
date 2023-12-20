# Creates the hake assessment Docker image. Starts with ubuntu-essentials
# which is built by building the Dockerfile in the subdirectory of this
# one by that name
#
# To build, make sure you are in the `docker` directory of a cloned hake
# repository (this directory)
#
# The command to build this image is:
# docker build -t cgrandin/hake .
# or, to build completely from scratch:
# docker build --no-cache -t cgrandin/hake .
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

FROM cgrandin/ubuntu-essentials as os
LABEL maintainer = "chris.grandin@gmail.com"

SHELL ["/bin/bash", "-c"]

ENV HOME=/home/rstudio
ENV PATH=$HOME:$PATH
ENV INST=/usr/bin

#RUN useradd -m container
WORKDIR $HOME

# Install ADMB and set corresponding environment variables
ENV ADMB_HOME=$INST/admb/build/admb
ENV ADMB_AD2CSV=$INST/admb/contrib/ad2csv
ENV PATH=$ADMB_AD2CSV:$ADMB_HOME/bin:$PATH
COPY clean_admb $ADMB_HOME
COPY install_admb.sh $INST/install_admb.sh
WORKDIR $INST
# The -d in the following tells the script to download (git clone) the
# source code prior to building it
RUN ./install_admb.sh -d
WORKDIR $HOME

# Install SS3 and set corresponding environment variables
COPY clean_ss3 $INST
COPY install_ss3.sh $INST/install_ss3.sh
ENV SS3_EXE_NAME=ss3_2024
ENV SS3_HOME=$INST/ss3
ENV PATH=$SS3_HOME:$PATH
WORKDIR $INST
RUN ./install_ss3.sh
RUN mv ss3/ss ss3/$SS3_EXE_NAME
WORKDIR $HOME

# Clone the hake repository
WORKDIR $HOME
RUN git clone https://github.com/pacific-hake/hake-assessment hake
RUN chmod -R 777 hake

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

# Install TexLive and set corresponding environment variables
COPY install_texlive.sh $HOME
RUN ./install_texlive.sh
ENV TEXLIVE_HOME=/usr/local/texlive/2023/bin/x86_64-linux
ENV MANPATH=/usr/local/texlive/2023/texmf-dist/doc/man
ENV INFOPATH=/usr/local/texlive/2023/texmf-dist/doc/info
ENV PATH=$TEXLIVE_HOME:$PATH
# Update the TexLive package manager contents
RUN tlmgr update --self

# Delete remnants
RUN rm install_r_packages.R
RUN rm install_texlive.sh
RUN rm -rf texlive_tmp

# Start in the hake directory
WORKDIR $HOME/hake
