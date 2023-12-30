# Create Docker image that has RStudio and tidyverse built-in,
# then add any necessary OS packages. The resulting image is uised as the
# base for the `cgrandin/ss-texlive` image, which resides in the sibling
# directory `ss-texlive`
#
# To build, make sure you are in the `docker/ubuntu-essentials` directory
# of a cloned hake repository (this directory)
#
# The command to build this image is:
# docker build -t cgrandin/ubuntu-essentials .
# or, to build completely from scratch:
# docker build --no-cache -t cgrandin/ubuntu-essentials .

FROM rocker/tidyverse:latest
RUN apt-get update && apt-get  -y --no-install-recommends install \
    # For VScode
    apt-transport-https \
    build-essential \
    curl \
    flex \
    htop \
    ghostscript \
    git \
    less \
    # For magick R package
    libmagick++-dev \
    # libgeos and libdunits packages are required for sf R package
    libudunits2-dev \
    libgeos++-dev \
    libgeos3.10.2 \
    libgeos-c1v5 \
    libgeos-dev \
    libgeos-doc \
    libgdal-dev \
    libglpk-dev \
    # For the raytracer group of R packages (left in despite
    # raytracer removal as it is fast)
    libglu1 \
    # libproj and libsqllite3 packages are required for sf and marmap
    # R packages
    libproj-dev \
    libsqlite3-dev \
    libxt6

RUN apt upgrade -y
