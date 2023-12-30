# Create a Docker image that has ADMB, SS3, and TexLive installed on top of
# the `cgrandin/ubuntu-essentials` image (See sibling directory by the same
# name)
#
# To build, make sure you are in the `docker/ss-texlive` directory
# of a cloned hake repository (this directory)
#
# The command to build this image is:
# docker build -t cgrandin/ss-texlive .
# or, to build completely from scratch:
# docker build --no-cache -t cgrandin/ss-texlive .

FROM cgrandin/ubuntu-essentials
LABEL maintainer = "chris.grandin@gmail.com"

SHELL ["/bin/bash", "-c"]

ENV HOME=/home/rstudio
ENV PATH=$HOME:$PATH
ENV INST=/usr/bin

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

# Install TexLive and set corresponding environment variables
COPY install_texlive.sh $HOME
RUN ./install_texlive.sh
ENV TEXLIVE_HOME=/usr/local/texlive/2023/bin/x86_64-linux
ENV MANPATH=/usr/local/texlive/2023/texmf-dist/doc/man
ENV INFOPATH=/usr/local/texlive/2023/texmf-dist/doc/info
ENV PATH=$TEXLIVE_HOME:$PATH

WORKDIR $HOME
