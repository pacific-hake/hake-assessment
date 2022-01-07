#/bin/bash

# Need to add users to the docker container as well for rstudio auth
# Add user cgrandin
useradd -m cgrandin
passwd -d cgrandin
usermod -a -G adm cgrandin
usermod -a -G wheel cgrandin

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

grep -q "www-port" /etc/rstudio/rserver.conf || echo "www-port=$port" >> /etc/rstudio/rserver.conf

# Necessary to stop Docker container from exiting after completion of CMD
rstudio-server start
tail -f /dev/null

