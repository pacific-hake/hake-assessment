# Docker script for hake assessment
# Used to produce Docker image cgrandin/hake
# https://hub.docker.com/repository/docker/cgrandin/hake

FROM cgrandin/ss_streamline_ss_proced as ss
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
COPY --from=ss /usr/bin/admb /usr/bin/admb
COPY install_packages.R install_packages.R
COPY add_port.sh add_port.sh
COPY credentials credentials
COPY config config
RUN Rscript install_packages.R

ENV SS_HOME=/usr/bin/ss
ENV PATH=/usr/bin/admb/contrib/ad2csv:/usr/bin/admb/build/admb:$SS_HOME:$PATH

CMD ["bash", "./add_port.sh"]

