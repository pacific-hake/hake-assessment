# Docker script for hake assessment
# Used to produce Docker image cgrandin/hake
# https://hub.docker.com/repository/docker/cgrandin/hake

FROM cgrandin/ss as ss
LABEL maintainer = "chris.grandin@gmail.com"

FROM rocker/verse:latest

# Install Linux packages required to run INLA
RUN apt-get update -qq && \
    apt-get -y --no-install-recommends install \
    libxt6 \
    tk8.6-dev \
    libgeos-dev \
    libudunits2-dev \
    libgdal-dev \
    libv8-dev \
    libglpk40 \
    cargo

COPY --from=ss /usr/bin/ss /usr/bin/ss
COPY install_packages.R install_packages.R
COPY add_port.sh add_port.sh
RUN Rscript install_packages.R

ENV SS_HOME=/usr/bin/ss
ENV PATH=$SS_HOME:$PATH

CMD ["bash", "./add_port.sh"]

