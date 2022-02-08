#/bin/bash

# Need to add users to the docker container as well for rstudio auth
# Add user cgrandin
useradd -m cgrandin
passwd -d cgrandin
usermod -a -G adm cgrandin
usermod -a -G wheel cgrandin
# cgrandin - Install AWS CLI
#cd /home/cgrandin
#curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
#unzip -o awscliv2.zip
#./aws/install

# Add user aedwards
useradd -m aedwards
passwd -d aedwards
usermod -a -G adm aedwards
usermod -a -G wheel aedwards

# Add user aberger
useradd -m aberger
passwd -d aberger
usermod -a -G adm aberger
usermod -a -G wheel aberger

# Add user kjohnson
useradd -m kjohnson
passwd -d kjohnson
usermod -a -G adm kjohnson
usermod -a -G wheel kjohnson

# Add user koken
useradd -m koken
passwd -d koken
usermod -a -G adm koken
usermod -a -G wheel koken

case $USER in
    cgrandin)
	port=8780
	;;
    aedwards)
	port=8781
	;;
    aberger)
	port=8782
	;;
    kjohnson)
	port=8783
	;;
    koken)
	port=8784
	;;
    *)
	port=8787
	;;
esac

# If the file has a line containing www-port, do nothing. Otherwise write the port
#  to the file. This is to cover the case where the rstudio server is stopped and restarted,
#  so that the file doesn't contain more than one www-port entry
grep -q "www-port" /etc/rstudio/rserver.conf || echo "www-port=$port" >> /etc/rstudio/rserver.conf

rstudio-server restart
# Necessary to stop Docker container from exiting after completion of CMD
tail -f /dev/null

