#!/bin/bash
#
# Install S3 Fuse from source so that the hakestore S3 directory and its
# subdirectories can be mounted in the EC2 instance
#
# Import this file into the 'User data' when setting up a new EC2 instance

# Install S3 Fuse from source
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
# If this key goes stale or becomes comprimised, create a new key/secret in the
# IAM Management Console, Users/Administrators/Administrator,
# Security credentials tab. Download the CSV and paste the values here.
# Replace the following line with the line contained in the local file 'key_pair'
# before uploading to AWS. It cannot be pushed to GitHub.
echo "KEY:SECRET" | tee -a /etc/passwd-s3fs
chmod 640 /etc/passwd-s3fs
# Set Fuse to allow all users access to the newly mounted drives
echo "user_allow_other" | sudo tee -a /etc/fuse.conf

# Add users
# useradd -m cgrandin
# passwd -d cgrandin
# git clone https://github.com/pacific-hake/hake-assessment
# chmod -R 777 hake-assessment
# Create the new directory to mount the S3 drive on
# mkdir -p /home/cgrandin/hake-assessment/hakestore

# Set up directory structure like this /home/ec2-user/hake-assessment/hakedrave/models-2021/...
# Pull the hake repository and make it writable for user ec2-user
cd /home/ec2-user
git clone https://github.com/pacific-hake/hake-assessment
chmod -R 777 hake-assessment
# Create the new directory to mount the S3 drive on
mkdir -p /home/ec2-user/hake-assessment/hakestore

# This is how you would mount the S3 drive manually from the command line
#  at /home/ec2-user/hake-assessment/hakestore using S3 fuse:
# s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/ec2-user/hake-assessment/hakestore
# Place the s3fs mounting into fstab
echo "s3fs#hakestore /home/ec2-user/hake-assessment/hakestore fuse _netdev,allow_other,use_cache=/tmp,umask=0000" | tee -a /etc/fstab

# Install htop, a CPU process viewer
yum -y install htop

# Set the ssh server up to send timeout checks to the clients to make sure they are still connected
echo "TCPKeepAlive yes" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 600" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 0" | tee -a /etc/ssh/sshd_config

# Install and run docker
amazon-linux-extras enable docker
yum -y install docker
service docker start
# Adds user ec2-user to the docker group so you don't need to prefix with sudo
usermod -a -G docker ec2-user
chmod 666 /var/run/docker.sock
docker pull cgrandin/hake-noextramcmc
cd /home/ec2-user
docker run -d -p 8787:8787 -e USER=rstudio -e PASSWORD=a \
       -e MODEL_DIR=models --name=hake --restart always \
       --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/hake-noextramcmc
# These two commands ensure the docker service starts when the machine starts after being stopped
systemctl enable docker.service
systemctl enable containerd.service

# Add rstudio user to sudoers so they can run mkdir command
echo "rstudio ALL=NOPASSWD: /usr/bin/mkdir" | tee -a /etc/sudoers
echo "rstudio ALL=NOPASSWD: /usr/bin/rmdir" | tee -a /etc/sudoers
# To give rstudio FULL permissions use next line
# echo "rstudio ALL=(ALL) NOPASSWD:ALL" | tee -a /etc/sudoers

# Start in hake-assessment directory
echo "if(interactive()) setwd('hake-assessment')" | tee -a /home/ec2-user/.Rprofile
# Make a copy of all SS input files for all models from S3 in their proper named subdirectories
echo "if(interactive()) source('R/all.R')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) copy_dirfiles('hakestore/models-2021-ss-input-files', 'models')" | tee -a /home/ec2-user/.Rprofile
echo "if(interactive()) system_('chmod -R 777 models')" | tee -a /home/ec2-user/.Rprofile

# Open in a web browser using instance IP:8787 like this example, 3.96.123.102:8787
# Rstudio login: rstudio (USER above)
# Password: a (PASSWORD above)

