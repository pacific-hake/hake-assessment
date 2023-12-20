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

docker run -it -p 8787:8787 -e PASSWORD=a -v hake_models:/srv \
  cgrandin/hake bash
