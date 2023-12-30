#!/bin/bash

# Build the hake image from scratch. First builds the dependent images,
# which reside in subdirectories.

docker build -t cgrandin/ubuntu-essentials ubuntu-essentials

docker build -t cgrandin/ss-texlive ss-texlive

docker build -t cgrandin/hake .