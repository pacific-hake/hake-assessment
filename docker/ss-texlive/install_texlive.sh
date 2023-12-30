# Install the newest TexLive
# Go here https://mirror.its.dal.ca/ctan/systems/texlive/tlnet/
# and find the newest version
#
tmp_tex=texlive_tmp
mkdir $tmp_tex
cd $tmp_tex
apt install -y wget
# Download the zipped file
wget https://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
zcat < install-tl-unx.tar.gz | tar xf -

mv `ls -d */` install
cd install
# Run the installer
perl ./install-tl --no-interaction
