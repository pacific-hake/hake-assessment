#!/bin/bash

# Keys -------------------------------------------------------------------------
# *Remove these keys before pushing to GitHub*

# Get this public key by running ssh-keygen on the private key file:
# ssh-keygen -y -f ~/.ssh/hake.pem
ssh_key=""
# If this access key goes stale or becomes comprimised, create a new key/secret in the
# IAM Management Console, Users/Administrators/Administrator,
# Security credentials tab. Download the CSV and paste the values here with a colon in between.
access_key=""

# Bash environment variables ---------------------------------------------------
models_dir=models
base_model_dir=2022.01.10_base
#docker_container=cgrandin/hake # ADMB commit 1376cb55 "Sept 11, 2021 - Update azure-pipelines.yml for Azure Pipelines"
docker_container=cgrandin/hake_admb_1376cb # ADMB commit for version 12.3
#docker_container=cgrandin/hake_2021
#docker_container=cgrandin/hake_admb_ss

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
# Add user aberger
useradd -m aberger
passwd -d aberger
cd /home/aberger
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
echo $ssh_key | tee -a .ssh/authorized_keys
echo | tee -a .ssh/authorized_keys
# Add user kjohnson
useradd -m kjohnson
passwd -d kjohnson
cd /home/kjohnson
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
echo $ssh_key | tee -a .ssh/authorized_keys
echo | tee -a .ssh/authorized_keys
# Add user koken
useradd -m koken
passwd -d koken
cd /home/koken
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
# Set groups and ownership for aberger
usermod -a -G adm aberger
usermod -a -G systemd-journal aberger
usermod -a -G wheel aberger
chown aberger:aberger /home/aberger/.ssh
chown aberger:aberger /home/aberger/.ssh/authorized_keys
# Set groups and ownership for kjohnson
usermod -a -G adm kjohnson
usermod -a -G systemd-journal kjohnson
usermod -a -G wheel kjohnson
chown kjohnson:kjohnson /home/kjohnson/.ssh
chown kjohnson:kjohnson /home/kjohnson/.ssh/authorized_keys
# Set groups and ownership for koken
usermod -a -G adm koken
usermod -a -G systemd-journal koken
usermod -a -G wheel koken
chown koken:koken /home/koken/.ssh
chown koken:koken /home/koken/.ssh/authorized_keys
# Add users to sudoers so they can run mkdir
echo "rstudio ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
echo "cgrandin ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
echo "aedwards ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
echo "aberger ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
echo "kjohnson ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
echo "koken ALL=NOPASSWD: /usr/bin/mkdir" >> /etc/sudoers
# To give rstudio FULL permissions use next line
# echo "rstudio ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

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
echo $access_key >> /etc/passwd-s3fs
chmod 640 /etc/passwd-s3fs
# Set Fuse to allow all users access to the newly mounted drives
echo "user_allow_other" >> /etc/fuse.conf

# Hake assessment --------------------------------------------------------------
# Pull the hake repository and make it writable for user ec2-user
cd /home/ec2-user
git clone https://github.com/pacific-hake/hake-assessment
chmod -R 777 hake-assessment
# Copy the assessment repo for cgrandin
cp -R hake-assessment /home/cgrandin/hake-assessment
chmod -R 777 /home/cgrandin/hake-assessment
# Copy the assessment repo for aedwards
cp -R hake-assessment /home/aedwards/hake-assessment
chmod -R 777 /home/aedwards/hake-assessment
# Copy the assessment repo for aberger
cp -R hake-assessment /home/aberger/hake-assessment
chmod -R 777 /home/aberger/hake-assessment
# Copy the assessment repo for kjohnson
cp -R hake-assessment /home/kjohnson/hake-assessment
chmod -R 777 /home/kjohnson/hake-assessment
# Copy the assessment repo for koken
cp -R hake-assessment /home/koken/hake-assessment
chmod -R 777 /home/koken/hake-assessment
# Create the new directories to mount the S3 drive on
mkdir -p /home/ec2-user/hake-assessment/hakestore
mkdir -p /home/cgrandin/hake-assessment/hakestore
mkdir -p /home/aedwards/hake-assessment/hakestore
mkdir -p /home/aberger/hake-assessment/hakestore
mkdir -p /home/kjohnson/hake-assessment/hakestore
mkdir -p /home/koken/hake-assessment/hakestore
# Mount the S3 drive using S3 FUSE:
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/ec2-user/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/cgrandin/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/aedwards/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/aberger/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/kjohnson/hake-assessment/hakestore
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/koken/hake-assessment/hakestore
# Place the S3 mounting code into fstab, and increase permissions so that S3 drive
#  is re-mounted on start of instance after being stopped
echo "s3fs#hakestore /home/ec2-user/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
echo "s3fs#hakestore /home/cgrandin/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
echo "s3fs#hakestore /home/aedwards/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
echo "s3fs#hakestore /home/aberger/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
echo "s3fs#hakestore /home/kjohnson/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
echo "s3fs#hakestore /home/koken/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" >> /etc/fstab
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
usermod -a -G docker aberger
usermod -a -G docker kjohnson
usermod -a -G docker koken
chmod 666 /var/run/docker.sock
docker pull $docker_container
cd /home/ec2-user
docker run -d -p 8787:8787 \
       -e USER=rstudio \
       -e PASSWORD=a \
       -e MODELS_DIR=$models_dir \
       -e BASE_MODEL=$base_model_dir \
       --name=hake-rstudio --restart always \
       --mount type=bind,source="$(pwd)",target=/home/rstudio $docker_container
# Run docker container for user cgrandin
cd /home/cgrandin
docker run -d -p 8780:8780 \
       -e USER=cgrandin \
       -e PASSWORD=a \
       -e MODELS_DIR=$models_dir \
       -e BASE_MODEL=$base_model_dir \
       --name=hake-cgrandin --restart always \
       --mount type=bind,source="$(pwd)",target=/home/cgrandin $docker_container
# Run docker container for user aedwards
cd /home/aedwards
docker run -d -p 8781:8781 \
       -e USER=aedwards \
       -e PASSWORD=a \
       -e MODELS_DIR=$models_dir \
       -e BASE_MODEL=$base_model_dir \
       --name=hake-aedwards --restart always \
       --mount type=bind,source="$(pwd)",target=/home/aedwards $docker_container
# Run docker container for user aberger
cd /home/aberger
docker run -d -p 8782:8782 \
       -e USER=aberger \
       -e PASSWORD=a \
       -e MODELS_DIR=$models_dir \
       -e BASE_MODEL=$base_model_dir \
       --name=hake-aberger --restart always \
       --mount type=bind,source="$(pwd)",target=/home/aberger $docker_container
# Run docker container for user kjohnson
cd /home/kjohnson
docker run -d -p 8783:8783 \
       -e USER=kjohnson \
       -e PASSWORD=a \
       -e MODELS_DIR=$models_dir \
       -e BASE_MODEL=$base_model_dir \
       --name=hake-kjohnson --restart always \
       --mount type=bind,source="$(pwd)",target=/home/kjohnson $docker_container
# Run docker container for user koken
cd /home/koken
docker run -d -p 8784:8784 \
           -e USER=koken \
           -e PASSWORD=a \
           -e MODELS_DIR=$models_dir \
	   -e BASE_MODEL=$base_model_dir \
           --name=hake-koken --restart always \
           --mount type=bind,source="$(pwd)",target=/home/koken $docker_container

# These two commands ensure the docker service starts when the machine starts after being stopped
systemctl enable docker.service
systemctl enable containerd.service

# Other programs ---------------------------------------------------------------
# Install htop, a CPU process viewer
yum -y install htop

# SSH Settings -----------------------------------------------------------------
# Set the ssh server up to send timeout checks to the clients to make sure they
#  are still connected
echo "TCPKeepAlive yes" >> /etc/ssh/sshd_config
echo "ClientAliveInterval 600" >> /etc/ssh/sshd_config
echo "ClientAliveInterval 0" >> /etc/ssh/sshd_config

# R Startup and Environment settings -------------------------------------------
# ec2-user
echo "if(interactive()) setwd('hake-assessment')" >> /home/ec2-user/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/ec2-user/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/ec2-user/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/ec2-user/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/ec2-user/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/ec2-user/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/ec2-user/.Renviron
# cgrandin
echo "if(interactive()) setwd('hake-assessment')" >> /home/cgrandin/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/cgrandin/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/cgrandin/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/cgrandin/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/cgrandin/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/cgrandin/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/cgrandin/.Renviron
# aedwards
echo "if(interactive()) setwd('hake-assessment')" >> /home/aedwards/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/aedwards/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/aedwards/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/aedwards/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/aedwards/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/aedwards/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/aedwards/.Renviron
# aberger
echo "if(interactive()) setwd('hake-assessment')" >> /home/aberger/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/aberger/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/aberger/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/aberger/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/aberger/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/aberger/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/aberger/.Renviron
# kjohnson
echo "if(interactive()) setwd('hake-assessment')" >> /home/kjohnson/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/kjohnson/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/kjohnson/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/kjohnson/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/kjohnson/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/kjohnson/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/kjohnson/.Renviron
# koken
echo "if(interactive()) setwd('hake-assessment')" >> /home/koken/.Rprofile
echo "if(interactive()) source('R/all.R')" >> /home/koken/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2022-ss-input-files', 'models')" >> /home/koken/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" >> /home/koken/.Rprofile
echo "MODELS_DIR=$models_dir" >> /home/koken/.Renviron
echo "BASE_MODEL=$base_model_dir" >> /home/koken/.Renviron
echo "PATH=/usr/bin/ss:$PATH" >> /home/koken/.Renviron

