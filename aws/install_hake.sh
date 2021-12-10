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

# Set up directory structure like this /home/ec2-user/hake-assessment/hakedrave/models-2021/...
# Pull the hake repository and make it writable for user ec2-user
cd /home/ec2-user
git clone https://github.com/pacific-hake/hake-assessment
chmod -R 777 hake-assessment

# Create the new directory to mount the S3 drive on
mkdir -p /home/ec2-user/hake-assessment/hakestore
# Mount it at /home/ec2-user/hake-assessment/hakestore using S3 fuse
s3fs hakestore -o use_cache=/tmp -o allow_other -o umask=0000 /home/ec2-user/hake-assessment/hakestore

# Install htop
yum -y install htop

# Set the ssh server up to send timeout checks to the clients to make sure they are still connected
echo "TCPKeepAlive yes" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 600" | tee -a /etc/ssh/sshd_config
echo "ClientAliveInterval 0" | tee -a /etc/ssh/sshd_config

# Install and run docker
amazon-linux-extras enable docker
yum -y install docker
# Adds user ec2-user to the docker group so you don't need to prefix with sudo
service docker start
usermod -a -G docker ec2-user
chmod 666 /var/run/docker.sock
docker pull cgrandin/hake
cd /home/ec2-user
docker run -d -p 8787:8787 -e USER=rstudio -e PASSWORD=a \
       -e MODEL_DIR=hakestore//models-2021 --name=hake \
       --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/hake

# Append the docker stuff to rc.local, so that the docker container is started at EVERY
# start, not just on instance creation as is with User Data.
'service docker start' >> /etc/rc.local
'usermod -a -G docker ec2-user' >> /etc/rc.local
'cd /home/ec2-user' >> /etc/rc.local
'docker run -d -p 8787:8787 -e USER=rstudio -e PASSWORD=a \
       -e MODEL_DIR=hakestore//models-2021 --name=hake \
       --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/hake' >> /etc/rc.local

# Open in a webbrowser using instance IP:8787 like this example, 3.96.123.102:8787
# Rstudio login: rstudio (USER above)
# Password: a (PASSWORD above)

