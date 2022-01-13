#!/bin/bash

# Keys -------------------------------------------------------------------------
# *Remove these keys before pushing to GitHub*

# Environment variables --------------------------------------------------------
models_dir=models
base_model_dir=2022.01.10_base

# Users ------------------------------------------------------------------------
# Add user cgrandin
useradd -m cgrandin
passwd -d cgrandin
cd /home/cgrandin
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
echo $ssh_key | tee -a .ssh/authorized_keys
echo | tee -a .ssh/authorized_keys
# Add user aedwards
useradd -m aedwards
passwd -d aedwards
cd /home/aedwards
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
echo $ssh_key | tee -a .ssh/authorized_keys
echo | tee -a .ssh/authorized_keys
# Set groups and ownership for cgrandin
usermod -a -G adm cgrandin
usermod -a -G systemd-journal cgrandin
usermod -a -G wheel cgrandin
chown cgrandin:cgrandin /home/cgrandin/.ssh
chown cgrandin:cgrandin /home/cgrandin/.ssh/authorized_keys
# Set groups and ownership for aedwards
usermod -a -G adm aedwards
usermod -a -G systemd-journal aedwards
usermod -a -G wheel aedwards
chown aedwards:aedwards /home/aedwards/.ssh
chown aedwards:aedwards /home/aedwards/.ssh/authorized_keys
# Add users to sudoers so they can run mkdir
echo "rstudio ALL=NOPASSWD: /usr/bin/mkdir" | tee -a /etc/sudoers
echo "cgrandin ALL=NOPASSWD: /usr/bin/mkdir" | tee -a /etc/sudoers
echo "aedwards ALL=NOPASSWD: /usr/bin/mkdir" | tee -a /etc/sudoers
# To give rstudio FULL permissions use next line
# echo "rstudio ALL=(ALL) NOPASSWD:ALL" | tee -a /etc/sudoers

# S3 FUSE ----------------------------------------------------------------------
# Install S3 Fuse from source so that the hakestore S3 directory and its
# subdirectories can be mounted in the EC2 instance
cd /usr/local
yum update
yum -y install automake fuse fuse-devel gcc-c++ \
    git libcurl-devel libxml2-devel make openssl-devel
git clone https://github.com/s3fs-fuse/s3fs-fuse.git
cd s3fs-fuse
./autogen.sh
./configure --prefix=/usr --with-openssl
make
make install
touch /etc/passwd-s3fs
echo $access_key | tee -a /etc/passwd-s3fs
chmod 640 /etc/passwd-s3fs
# Set Fuse to allow all users access to the newly mounted drives
echo "user_allow_other" | sudo tee -a /etc/fuse.conf

# Hake assessment --------------------------------------------------------------
# Pull the hake repository and make it writable for user ec2-user
cd /home/ec2-user
git clone https://github.com/pacific-hake/hake-assessment
chmod -R 777 hake-assessment
# Copy the assessment repo for user cgrandin
cp -R hake-assessment /home/cgrandin/hake-assessment
chmod -R 777 /home/cgrandin/hake-assessment
# Copy the assessment repo for user aedwards
cp -R hake-assessment /home/aedwards/hake-assessment
chmod -R 777 /home/aedwards/hake-assessment
# Create the new directories to mount the S3 drive on
mkdir -p /home/ec2-user/hake-assessment/hakestore
mkdir -p /home/cgrandin/hake-assessment/hakestore
mkdir -p /home/aedwards/hake-assessment/hakestore
# Mount the S3 drive using S3 FUSE:
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/ec2-user/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/cgrandin/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/aedwards/hake-assessment/hakestore
# Place the S3 mounting code into fstab, and increase permissions so that S3 drive
#  is re-mounted on start of instance after being stopped
echo "s3fs#hakestore /home/ec2-user/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" | tee -a /etc/fstab
echo "s3fs#hakestore /home/cgrandin/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" | tee -a /etc/fstab
echo "s3fs#hakestore /home/aedwards/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" | tee -a /etc/fstab
chmod 777 /etc/fstab

# Docker -----------------------------------------------------------------------
# Install and run docker
amazon-linux-extras enable docker
yum -y install docker
service docker start
# Adds users to the docker group so they don't need to prefix with sudo
usermod -a -G docker ec2-user
usermod -a -G docker cgrandin
usermod -a -G docker aedwards
chmod 666 /var/run/docker.sock
docker pull cgrandin/hake
cd /home/ec2-user
docker run -d -p 8787:8787 -e USER=rstudio -e PASSWORD=a \
       -e MODELS_DIR=$models_dir -e BASE_MODEL=$base_model_dir --name=hake-rstudio --restart always \
       --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/hake
# Run docker container for user cgrandin
cd /home/cgrandin
docker run -d -p 8780:8780 -e USER=cgrandin -e PASSWORD=a \
       -e MODELS_DIR=$models_dir -e BASE_MODEL=$base_model_dir --name=hake-cgrandin --restart always \
       --mount type=bind,source="$(pwd)",target=/home/cgrandin cgrandin/hake
# Run docker container for user aedwards
cd /home/aedwards
docker run -d -p 8781:8781 -e USER=aedwards -e PASSWORD=a \
       -e MODELS_DIR=$models_dir -e BASE_MODEL=$base_model_dir --name=hake-aedwards --restart always \
       --mount type=bind,source="$(pwd)",target=/home/aedwards cgrandin/hake

# These two commands ensure the docker service starts when the machine starts after being stopped
systemctl enable docker.service
systemctl enable containerd.service

# Other programs ---------------------------------------------------------------
# Install htop, a CPU process viewer
yum -y install htop

# SSH Settings -----------------------------------------------------------------
# Set the ssh server up to send timeout checks to the clients to make sure they
#  are still connected
echo "TCPKeepAlive yes" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 600" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 0" | tee -a /etc/ssh/sshd_config

# R Startup Settings -----------------------------------------------------------
# Remove unnecessary R directory
#echo "if(interactive()) unlink('/home/rstudio/R', recursive = TRUE, force = TRUE)" | tee -a /home/ec2-user/.Rprofile
#echo "if(interactive()) unlink('/home/cgrandin/R', recursive = TRUE, force = TRUE)" | tee -a /home/cgrandin/.Rprofile
#echo "if(interactive()) unlink('/home/aedwards/R', recursive = TRUE, force = TRUE)" | tee -a /home/aedwards/.Rprofile
# Start in hake-assessment directory
echo "if(interactive()) setwd('hake-assessment')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) setwd('hake-assessment')" | tee -a /home/cgrandin/.Rprofile
echo "if(interactive()) setwd('hake-assessment')" | tee -a /home/aedwards/.Rprofile
# Set environment variables for R terminal
echo "MODELS_DIR=$models_dir" >> /home/ec2-user/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/ec2-user/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/ec2-user/.Renviron
echo "MODELS_DIR=$models_dir" >> /home/cgrandin/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/cgrandin/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/cgrandin/.Renviron
echo "MODELS_DIR=$models_dir" >> /home/aedwards/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/aedwards/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/aedwards/.Renviron
# Make a copy of all SS input files for all models from S3 in their proper named
#  subdirectories
echo "if(interactive()) source('R/all.R')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) source('R/all.R')" | tee -a /home/cgrandin/.Rprofile
echo "if(interactive()) source('R/all.R')" | tee -a /home/aedwards/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" | tee -a /home/cgrandin/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" | tee -a /home/aedwards/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" | tee -a /home/cgrandin/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" | tee -a /home/aedwards/.Rprofile

